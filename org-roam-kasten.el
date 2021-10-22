;;; org-roam-kasten.el --- Browse your Zettelkasten with a "physical" feeling  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Amir Dekel

;; Author: Amir Dekel
;; URL: https://example.com/package-name.el
;; Version: 0.1-alpha
;; Package-Requires: ((org-roam "2.0.0") visual-fill-column adaptive-wrap)
;; Keywords: org-mode roam convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;;; Requirements

(require 'org-roam)
(require 'visual-fill-column)
(require 'adaptive-wrap)

;;;; Customization

;; TODO

;;;; Variables

(defvar ork-buffer-name "*Zettelkasten*")

(defvar ork-index-tag "@topic"
  "Tag defining Zettelkasten entry nodes.")

;;;;; Local variables for the kasten buffer

(defvar-local ork--history nil)
(defvar-local ork--history-forward nil)
(defvar-local ork--current-node nil)
(defvar-local ork--current-title nil)
(defvar-local ork--current-level nil)
(defvar-local ork--current-content nil)
(defvar-local ork--current-child-nodes nil)
(defvar-local ork--currently-examining-folgezettel nil)

(make-variable-buffer-local 'ork--buffer)

;;;;; Keymaps

;; TODO: Use separate keymap; possible with the template's snippet.

;;;; Commands

;;;###autoload
(defun ork-enter (&optional other-window initial-input)
  "Find a node for entering the Zettelkasten."
  (interactive current-prefix-arg)
  (let* ((node (org-roam-node-read initial-input
                                   'ork--index-p
                                   'org-roam-node-read-sort-by-file-mtime
                                   t))
         (kasten (ork--get-buffer-create)))
    (set-buffer kasten)
    (ork--load-display node)
    (switch-to-buffer kasten)
    (recenter)))

;;;; Functions

(define-minor-mode org-roam-kasten-mode ""
  :lighter " ork"
  :keymap '(("]" . ork-next-physical-zettel)
            ("[" . ork-previous-physical-zettel)
            ("l" . ork-history-back)
            ("r" . ork-history-forward)
            (" " . ork-examine-folgezettel)
            ("^" . ork-parent-zettel)
            ("\r" . ork-follow-folgezettel-or-link-at-point)
            ("\t" . ork-show-content-or-next-link)
            ([backtab] . org-previous-link)
            ("o" . ork-visit-node)
            ("q" . quit-window)))

;;;;; Public

(defun ork-show-content-or-next-link ()
  "Show the subtree if it is folded. Otherwise go to next link."
  (interactive)
  (if (string-equal org-cycle-subtree-status "folded")
      (org-cycle)
    (org-next-link)))

(defun ork-history-back ()
  "Move back in history in current kasten node."
  (interactive)
  (when (ork--buffer-p)
    (if-let ((node (pop ork--history)))
        (progn (push ork--current-node ork--history-forward)
               (ork--load-display node t t))
      (user-error "No further history."))))

(defun ork-history-forward ()
  "Move forward in history in current kasten node."
  (interactive)
  (when (ork--buffer-p)
    (if-let ((node (pop ork--history-forward)))
        (progn (push ork--current-node ork--history)
               (ork--load-display node t t))
      (user-error "No further forward history."))))

(defun ork-follow-folgezettel-or-link-at-point ()
  "If currently examining a folgezettel, follow it.
Otherwise if point is on a link: if it is a roam node, follow it;
otherwise visit it normally in other window."
  (interactive)
  (if ork--currently-examining-folgezettel
      (ork--load-display (nth ork--currently-examining-folgezettel
                              ork--current-child-nodes))
      (let ((object (org-element-context)))
        (if (and (string-equal "link" (org-element-type object))
                 (string-equal "id" (org-element-property :type object)))
            (let ((node (org-roam-node-from-id (string-trim-left
                                                (org-element-property :raw-link object)
                                                "id:"))))
              (if node
                  (ork--load-display node)
                (org-open-at-point)))
          (org-open-at-point)))))

(defun ork-next-physical-zettel (&optional prev)
  "Display the next physical zettel in the current kasten.
If PREV, display the previous physical zettel."
  (interactive)
  (let ((next-zettel (ork--next-physical-node ork--current-node prev)))
    (when next-zettel
      (ork--load-display next-zettel nil t))))

(defun ork-previous-physical-zettel ()
  "Display the previous physical zettel in the current kasten."
  (interactive)
  (ork-next-physical-zettel t))

(defun ork-examine-folgezettel ()
  (interactive)
  (cond ((not ork--currently-examining-folgezettel)
         (when ork--current-child-nodes
           (setq ork--currently-examining-folgezettel 0)))
        ((< ork--currently-examining-folgezettel (1- (length ork--current-child-nodes)))
         (setq ork--currently-examining-folgezettel (1+ ork--currently-examining-folgezettel)))
        ((setq ork--currently-examining-folgezettel nil)))
  (ork--update-folgezettel-line))

(defun ork-parent-zettel ()
  (interactive)
  (when-let ((parent (ork--parent-node ork--current-node)))
    (ork--load-display parent nil t)))

(defun ork-visit-node (other-window)
  "Visit the node currently in display (with prefix - in other window)."
  (interactive "P")
  (org-roam-node-visit ork--current-node other-window))

;;;;; Private

(defun ork--index-p (node)
  "Query whether the node is an index node. Only nodes tagged
with `ork-index-tag' will be included in the
completion buffer."
  (member ork-index-tag (org-roam-node-tags node)))

(defun ork--buffer-p ()
  "Return true if the current buffer is ork buffer."
  (and (boundp 'ork--buffer)
       ork--buffer))

(defun ork--node-content (node)
  "Extract the content of NODE until the next sibling or child,
excluding the property drawer."
  (with-current-buffer (org-roam-node-find-noselect node)
    (org-with-wide-buffer
     (goto-char (org-roam-node-point node))
     (let* ((content-begin-re (if (= 0 (org-roam-node-level node))
                                  "^[^#: ]"
                                ":end:\n+"))
            (content-begin (save-excursion
                             (re-search-forward content-begin-re nil t)))
            (content-end (or (outline-next-heading)
                             (point-max))))
       (if (<= content-end content-begin)
           ""
         (buffer-substring-no-properties content-begin content-end))))))

(defun ork--next-physical-node (node &optional prev)
  "Find the next physical node.
If PREV is non-nil then find the previous node."
  (let ((begin (org-roam-node-point node))
        (fn (if prev #'outline-previous-heading
              #'outline-next-heading)))
    (with-current-buffer (org-roam-node-find-noselect node t)
      (org-with-wide-buffer
       (while (and (funcall fn)
                   (not (org-entry-get (point) "ID"))))
       (unless (= (point) begin)
         (org-roam-node-at-point))))))

(defun ork--child-nodes (node)
  (with-current-buffer (org-roam-node-find-noselect node)
    (let* ((level (org-roam-node-level node))
           (node-tree (org-map-entries 'org-roam-node-at-point nil
                                       (if (= level 0) 'file 'tree))))
      (seq-filter (lambda (node) (= (+ level 1) (org-roam-node-level node)))
                  node-tree))))

(defun ork--parent-node (node)
  (with-current-buffer (org-roam-node-find-noselect node)
    (unless (not (org-current-level))
      (while (and
              (org-up-heading-or-point-min)
              (not (org-roam-node-at-point))))
      (org-roam-node-at-point))))

(defun ork--load-node (node &optional preserve-history)
  "Loads and parses NODE into the buffer-local variables.
If PRESERVE-HISTORY, don't reset ork--history-forward and don't add
to ork--history (used when moving back/forwards in history)."
  (when (ork--buffer-p)                 ;safety measure
    (unless preserve-history
      (setq ork--history-forward nil)
      (when ork--current-node
        (push ork--current-node ork--history)))
    (setq ork--current-node node
          ork--current-title (org-roam-node-title node)
          ork--current-level (org-roam-node-level node)
          ork--current-content (ork--node-content node)
          ork--current-child-nodes (ork--child-nodes node)
          ork--currently-examining-folgezettel nil)))

(defun ork--update-folgezettel-line ()
  "Update the line displaying Folgezettel."
  (when (ork--buffer-p)                 ;safety measure
    (save-excursion
      (let ((inhibit-read-only t)
            (kill-whole-line nil)
            (folge-title (when ork--currently-examining-folgezettel
                           (org-roam-node-title
                            (nth ork--currently-examining-folgezettel ork--current-child-nodes)))))
        (goto-char (point-min))
        (kill-line)
        (if folge-title
            (insert "** " folge-title)
          (insert "# " (int-to-string (length ork--current-child-nodes)) " folgezettel"))))))

(defun ork--display-buffer (&optional folded)
  "(Re)display the kasten buffer with the current node.
If FOLDED, fold the heading.'"
  (when (ork--buffer-p)                 ;safety measure
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "# " (int-to-string (length ork--current-child-nodes)) " folgezettel\n\n")
      (insert "* " ork--current-title "\n\n" ork--current-content)
      (outline-previous-heading)
      (when folded
        (org-cycle-internal-local)))))

(defun ork--load-display (node &optional preserve-history folded)
  "Loads NODE and (re)displays the buffer.
Passes PRESERVE-HISTORY to `ork--load-node'.
Passes FOLDED to `ork--display-buffer'."
  (ork--load-node node preserve-history)
  (ork--display-buffer folded))

(defun ork--get-buffer-create ()
  (let ((buf (get-buffer-create ork-buffer-name)))
    (with-current-buffer buf
      (unless (ork--buffer-p)               ;test if the buffer is new
        (cd org-roam-directory)
        (org-mode)
        (org-roam-kasten-mode)
        (flyspell-mode-off)
        (visual-line-mode)
        (visual-fill-column-mode)
        (adaptive-wrap-prefix-mode)
        (read-only-mode)
        (setq ork--buffer t))
      buf)))

;;;; Footer

(provide 'org-roam-kasten)

;;; org-roam-kasten.el ends here
