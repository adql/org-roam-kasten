;;; org-buffer.el --- Org Roam Kasten: Zettelkasten buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Amir Dekel

;; Author: Amir Dekel
;; URL: https://example.com/package-name.el
;; Version: 0.1-alpha
;; Package-Requires: ((org-roam "2.0.0"))
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

(require 'ork-get)

;;;; Variables

(defvar ork-buffer-name "*Zettelkasten*")

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

;;;; Functions

(define-minor-mode org-roam-kasten-mode ""
  :lighter " ork"
  :keymap '(("]" . ork-next-physical-zettel)
            ("[" . ork-previous-physical-zettel)
            ("n" . ork-next-zettel-same-level)
            ("p" . ork-previous-zettel-same-level)
            ("l" . ork-history-back)
            ("r" . ork-history-forward)
            (" " . ork-examine-folgezettel)
            ("^" . ork-parent-zettel)
            ("\r" . ork-follow-folgezettel-or-link-at-point)
            ("\t" . ork-show-content-or-next-link)
            ([backtab] . org-previous-link)
            ("o" . ork-visit-node)
            ("\C-cl" . ork-store-link)
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
               (ork-buffer-load-refresh node t t))
      (user-error "No further history."))))

(defun ork-history-forward ()
  "Move forward in history in current kasten node."
  (interactive)
  (when (ork--buffer-p)
    (if-let ((node (pop ork--history-forward)))
        (progn (push ork--current-node ork--history)
               (ork-buffer-load-refresh node t t))
      (user-error "No further forward history."))))

(defun ork-follow-folgezettel-or-link-at-point ()
  "If currently examining a folgezettel, follow it.
Otherwise if point is on a link: if it is a roam node, follow it;
otherwise visit it normally in other window."
  (interactive)
  (if ork--currently-examining-folgezettel
      (ork-buffer-load-refresh (nth ork--currently-examining-folgezettel
                              ork--current-child-nodes))
    (let ((object (org-element-context)))
      (when (string-equal "link" (org-element-type object))
        (if (string-equal "id" (org-element-property :type object))
            (if-let ((node (org-roam-node-from-id (string-trim-left
                                                   (org-element-property :raw-link object)
                                                   "id:"))))
                (ork-buffer-load-refresh node)
              (org-open-at-point))
          (org-open-at-point))))))

(defun ork-next-physical-zettel (&optional prev same-level)
  "Display the next \"physical\" zettel in the current kasten.
If PREV, display the previous physical zettel. If SAME-LEVEL,
display the next or previous zettel at the same level under the
current parent zettel."
  (interactive)
  (let ((next-zettel (ork--next-node ork--current-node prev same-level)))
    (when next-zettel
      (ork-buffer-load-refresh next-zettel nil t))))

(defun ork-previous-physical-zettel ()
  "Display the previous physical zettel in the current kasten."
  (interactive)
  (ork-next-physical-zettel t))

(defun ork-next-zettel-same-level ()
  "Display the next zettel at the same level under the current
parent zettel."
  (interactive)
  (ork-next-physical-zettel nil t))

(defun ork-previous-zettel-same-level ()
  "Display the previous zettel at the same level under the
current parent zettel."
  (interactive)
  (ork-next-physical-zettel t t))

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
    (ork-buffer-load-refresh parent nil t)))

(defun ork-visit-node (other-window)
  "Visit the node currently in display (with prefix - in other window)."
  (interactive "P")
  (org-roam-node-visit ork--current-node other-window))

(defun ork-store-link ()
  "Store the link to the node currently in display."
  (interactive)
  (org-with-point-at (org-roam-node-marker ork--current-node)
    (org-store-link nil t)))

;;;;; Private

(defun ork-buffer-get-buffer-create ()
  "Returns the zettelkasten buffer, possibly creating a new one.

The buffer name is determined by `ork-buffer-name'."
  (let ((buf (get-buffer-create ork-buffer-name)))
    (with-current-buffer buf
      (unless (ork--buffer-p)               ;test if the buffer is new
        (cd org-roam-directory)
        (org-mode)
        (org-roam-kasten-mode)
        (read-only-mode)
        (setq ork--buffer t))
      buf)))

(defun ork--buffer-p ()
  "Return true if the current buffer is ork buffer."
  (and (boundp 'ork--buffer)
       ork--buffer))

;;;;;; Buffer manipulation

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
        (goto-line 3)
        (kill-line)
        (if folge-title
            (insert "** " folge-title)
          (insert "# " (int-to-string (length ork--current-child-nodes)) " folgezettel"))))))

(defun ork--insert-zettel (&optional folded)
  "Display the currently loaded node as a zettel.
This means inserting `ork--current-title' and `ork--current-content'
alongside information about folgezettel."
  (insert "# " (int-to-string (length ork--current-child-nodes)) " folgezettel\n\n")
  (insert "* " ork--current-title "\n\n" ork--current-content)
  (outline-previous-heading)
  (when folded
    (org-cycle-internal-local)))

(defun ork--insert-index ()
  "Display the currently loaded node as an index."
  (insert "# ~~ Index ~~\n\n")
  (let ((folgezettel-titles (mapcar 'org-roam-node-title ork--current-child-nodes)))
    (cl-destructuring-bind (titles-before titles-after)
        (ork--sibling-titles ork--current-node)
      (mapc (lambda (title) (insert "# " title "\n")) titles-before)
      (insert "\n* " ork--current-title "\n\n" ork--current-content)
      (mapc (lambda (title) (insert "# └ " title "\n")) folgezettel-titles)
      (insert "\n")
      (mapc (lambda (title) (insert "# " title "\n")) titles-after)
      (outline-previous-heading))))

(defun ork--refresh-buffer (&optional folded)
  "Refresh the kasten buffer to display the current loaded node.
If FOLDED, fold the heading.'"
  (when (ork--buffer-p)                 ;safety measure
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "# ~~~ Zettelkasten ~~~\n\n")
      (ork--insert-zettel folded))))

(defun ork-buffer-load-refresh (node &optional preserve-history folded)
  "Loads NODE and refreshes the buffer.
Passes PRESERVE-HISTORY to `ork--load-node'.
Passes FOLDED to `ork--refresh-buffer'."
  (ork--load-node node preserve-history)
  (ork--refresh-buffer folded))

;;;; Footer

(provide 'ork-buffer)

;;; ork-buffer.el ends here