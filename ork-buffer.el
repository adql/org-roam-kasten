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

;;;; Variables

(defvar ork-buffer-name "*Zettelkasten*")

;;;;; Local variables for the kasten buffer

(defvar-local ork-buffer--history nil)
(defvar-local ork-buffer--history-forward nil)
(defvar-local ork-buffer--current-node nil)
(defvar-local ork-buffer--current-title nil)
(defvar-local ork-buffer--current-level nil)
(defvar-local ork-buffer--current-content nil)
(defvar-local ork-buffer--current-display-properties nil)
(defvar-local ork-buffer--current-child-nodes nil)
(defvar-local ork-buffer--currently-examining-folgezettel nil)

(make-variable-buffer-local 'ork-buffer--buffer) ;TODO -- use variable org-roam-mode instead

;;;; Functions

(define-minor-mode org-roam-kasten-mode ""
  :lighter " ork"
  :keymap '(("]" . ork-buffer-next-physical-zettel)
            ("[" . ork-buffer-previous-physical-zettel)
            ("n" . ork-buffer-next-zettel-same-level)
            ("p" . ork-buffer-previous-zettel-same-level)
            ("l" . ork-buffer-history-back)
            ("r" . ork-buffer-history-forward)
            (" " . ork-buffer-examine-folgezettel)
            ("^" . ork-buffer-parent-zettel)
            ("\r" . ork-buffer-follow-folgezettel-or-link-at-point)
            ("\t" . ork-buffer-show-content-or-next-link)
            ([backtab] . org-previous-link)
            ("o" . ork-buffer-visit-node)
            ("\C-cl" . ork-buffer-store-link)
            ("q" . quit-window)))

;;;;; Public

(defun ork-buffer-show-content-or-next-link ()
  "Show the subtree if it is folded. Otherwise go to next link."
  (interactive)
  (if (string-equal org-cycle-subtree-status "folded")
      (org-cycle)
    (org-next-link)))

(defun ork-buffer-history-back ()
  "Move back in history in current kasten node."
  (interactive)
  (when (ork-buffer--buffer-p)
    (if-let ((node (pop ork-buffer--history)))
        (progn (push ork-buffer--current-node ork-buffer--history-forward)
               (ork-buffer-load-refresh node t t))
      (user-error "No further history."))))

(defun ork-buffer-history-forward ()
  "Move forward in history in current kasten node."
  (interactive)
  (when (ork-buffer--buffer-p)
    (if-let ((node (pop ork-buffer--history-forward)))
        (progn (push ork-buffer--current-node ork-buffer--history)
               (ork-buffer-load-refresh node t t))
      (user-error "No further forward history."))))

(defun ork-buffer-follow-folgezettel-or-link-at-point ()
  "If currently examining a folgezettel, follow it.
Otherwise if point is on a link: if it is a roam node, follow it;
otherwise visit it normally in other window."
  (interactive)
  (if ork-buffer--currently-examining-folgezettel
      (ork-buffer-load-refresh (nth ork-buffer--currently-examining-folgezettel
                              ork-buffer--current-child-nodes))
    (let ((object (org-element-context)))
      (when (string-equal "link" (org-element-type object))
        (if (string-equal "id" (org-element-property :type object))
            (if-let ((node (org-roam-node-from-id (string-trim-left
                                                   (org-element-property :raw-link object)
                                                   "id:"))))
                (ork-buffer-load-refresh node)
              (org-open-at-point))
          (org-open-at-point))))))

(defun ork-buffer-next-physical-zettel (&optional prev same-level)
  "Display the next \"physical\" zettel in the current kasten.
If PREV, display the previous physical zettel. If SAME-LEVEL,
display the next or previous zettel at the same level under the
current parent zettel."
  (interactive)
  (let ((next-zettel (ork-get-next-node ork-buffer--current-node prev same-level)))
    (when next-zettel
      (ork-buffer-load-refresh next-zettel nil t))))

(defun ork-buffer-previous-physical-zettel ()
  "Display the previous physical zettel in the current kasten."
  (interactive)
  (ork-buffer-next-physical-zettel t))

(defun ork-buffer-next-zettel-same-level ()
  "Display the next zettel at the same level under the current
parent zettel."
  (interactive)
  (ork-buffer-next-physical-zettel nil t))

(defun ork-buffer-previous-zettel-same-level ()
  "Display the previous zettel at the same level under the
current parent zettel."
  (interactive)
  (ork-buffer-next-physical-zettel t t))

(defun ork-buffer-examine-folgezettel ()
  (interactive)
  (cond ((not ork-buffer--currently-examining-folgezettel)
         (when ork-buffer--current-child-nodes
           (setq ork-buffer--currently-examining-folgezettel 0)))
        ((< ork-buffer--currently-examining-folgezettel (1- (length ork-buffer--current-child-nodes)))
         (setq ork-buffer--currently-examining-folgezettel (1+ ork-buffer--currently-examining-folgezettel)))
        ((setq ork-buffer--currently-examining-folgezettel nil)))
  (ork-buffer--update-folgezettel-line))

(defun ork-buffer-parent-zettel ()
  (interactive)
  (when-let ((parent (ork-get-parent-node ork-buffer--current-node)))
    (ork-buffer-load-refresh parent nil t)))

(defun ork-buffer-visit-node (other-window)
  "Visit the node currently in display (with prefix - in other window)."
  (interactive "P")
  (org-roam-node-visit ork-buffer--current-node other-window))

(defun ork-buffer-store-link ()
  "Store the link to the node currently in display."
  (interactive)
  (org-with-point-at (org-roam-node-marker ork-buffer--current-node)
    (org-store-link nil t)))

;;;;; Private

(defun ork-buffer-get-buffer-create ()
  "Returns the zettelkasten buffer, possibly creating a new one.

The buffer name is determined by `ork-buffer-name'."
  (let ((buf (get-buffer-create ork-buffer-name)))
    (with-current-buffer buf
      (unless (ork-buffer--buffer-p)               ;test if the buffer is new
        (cd org-roam-directory)
        (org-mode)
        (org-roam-kasten-mode)
        (read-only-mode)
        (setq org-hide-emphasis-markers t
              ork-buffer--buffer t))
      buf)))

(defun ork-buffer--buffer-p ()
  "Return true if the current buffer is ork buffer."
  (and (boundp 'ork-buffer--buffer)
       ork-buffer--buffer))

;;;;;; Buffer manipulation

(defun ork-buffer--load-node (node &optional preserve-history)
  "Loads and parses NODE into the buffer-local variables.
If PRESERVE-HISTORY, don't reset ork-buffer--history-forward and don't add
to ork-buffer--history (used when moving back/forwards in history)."
  (when (ork-buffer--buffer-p)                 ;safety measure
    (unless preserve-history
      (setq ork-buffer--history-forward nil)
      (when ork-buffer--current-node
        (push ork-buffer--current-node ork-buffer--history)))
    (setq ork-buffer--current-node node
          ork-buffer--current-title (org-roam-node-title node)
          ork-buffer--current-level (org-roam-node-level node)
          ork-buffer--current-content (ork-get-node-content node)
          ork-buffer--current-display-properties (ork-get-display-properties node)
          ork-buffer--current-child-nodes (ork-get-child-nodes node)
          ork-buffer--currently-examining-folgezettel nil)))

(defun ork-buffer--update-folgezettel-line ()
  "Update the line displaying Folgezettel."
  (when (ork-buffer--buffer-p)                 ;safety measure
    (save-excursion
      (let ((inhibit-read-only t)
            (kill-whole-line nil)
            (folge-title (when ork-buffer--currently-examining-folgezettel
                           (org-roam-node-title
                            (nth ork-buffer--currently-examining-folgezettel ork-buffer--current-child-nodes)))))
        (goto-line 3)
        (kill-line)
        (if folge-title
            (insert "** " folge-title)
          (insert "# " (int-to-string (length ork-buffer--current-child-nodes)) " folgezettel"))))))

(defun ork-buffer--insert-zettel (&optional folded)
  "Display the currently loaded node as a zettel.
This means inserting `ork-buffer--current-title' and `ork-buffer--current-content'
alongside information about folgezettel."
  (insert "# " (int-to-string (length ork-buffer--current-child-nodes)) " folgezettel\n\n")
  (insert "* " ork-buffer--current-title "\n\n" ork-buffer--current-content)
  (outline-previous-heading)
  (when folded
    (org-cycle-internal-local)))

(defun ork-buffer--insert-index ()
  "Display the currently loaded node as an index."
  (insert "# ~~ Index ~~\n\n")
  (let ((folgezettel-titles (mapcar 'org-roam-node-title ork-buffer--current-child-nodes)))
    (cl-destructuring-bind (titles-before titles-after)
        (ork-get-sibling-titles ork-buffer--current-node)
      (mapc (lambda (title) (insert "# " title "\n")) titles-before)
      (insert "\n* " ork-buffer--current-title "\n\n" ork-buffer--current-content)
      (mapc (lambda (title) (insert "# └ " title "\n")) folgezettel-titles)
      (insert "\n")
      (mapc (lambda (title) (insert "# " title "\n")) titles-after)
      (outline-previous-heading))))

(defun ork-buffer--refresh-buffer (&optional folded)
  "Refresh the kasten buffer to display the current loaded node.
If FOLDED, fold the heading.'"
  (when (ork-buffer--buffer-p)                 ;safety measure
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "# ~~~ Zettelkasten ~~~\n\n")
      (ork-buffer--insert-zettel folded))))

(defun ork-buffer-load-refresh (node &optional preserve-history folded)
  "Loads NODE and refreshes the buffer.
Passes PRESERVE-HISTORY to `ork-buffer--load-node'.
Passes FOLDED to `ork-buffer--refresh-buffer'."
  (ork-buffer--load-node node preserve-history)
  (ork-buffer--refresh-buffer folded))

;;;; Footer

(provide 'ork-buffer)

;;; ork-buffer.el ends here
