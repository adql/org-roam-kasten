;;; org-roam-kasten.el --- Browse your zettelkasten with a "physical" feeling  -*- lexical-binding: t; -*-

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

(require 'org-roam)
(require 'cl-lib)                       ;technically it's already required by org-roam

;;;; Customization

;; TODO

;;;; Variables

(defvar ork-buffer-name "*Zettelkasten*")

(defvar ork-entry-tag-re "^@.+"
  "Tag defining zettelkasten entry nodes.
All entry nodes will be included in the completion buffer.")

(defvar ork-directory-file-node-re "0_.*"
  "Regex for the file holding a file-level node for the directory.")

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
  "Find a node for entering the zettelkasten."
  (interactive current-prefix-arg)
  (let* ((node (org-roam-node-read initial-input
                                   'ork--entry-p
                                   'org-roam-node-read-sort-by-file-mtime
                                   t))
         (kasten (ork--get-buffer-create)))
    (set-buffer kasten)
    (ork--load-refresh node)
    (switch-to-buffer kasten)
    (recenter)))

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
               (ork--load-refresh node t t))
      (user-error "No further history."))))

(defun ork-history-forward ()
  "Move forward in history in current kasten node."
  (interactive)
  (when (ork--buffer-p)
    (if-let ((node (pop ork--history-forward)))
        (progn (push ork--current-node ork--history)
               (ork--load-refresh node t t))
      (user-error "No further forward history."))))

(defun ork-follow-folgezettel-or-link-at-point ()
  "If currently examining a folgezettel, follow it.
Otherwise if point is on a link: if it is a roam node, follow it;
otherwise visit it normally in other window."
  (interactive)
  (if ork--currently-examining-folgezettel
      (ork--load-refresh (nth ork--currently-examining-folgezettel
                              ork--current-child-nodes))
    (let ((object (org-element-context)))
      (when (string-equal "link" (org-element-type object))
        (if (string-equal "id" (org-element-property :type object))
            (if-let ((node (org-roam-node-from-id (string-trim-left
                                                   (org-element-property :raw-link object)
                                                   "id:"))))
                (ork--load-refresh node)
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
      (ork--load-refresh next-zettel nil t))))

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
    (ork--load-refresh parent nil t)))

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

(defun ork--get-buffer-create ()
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

;;;;;; Predicates

(defun ork--entry-p (node)
  "Query whether the node is an entry node.

An entry node is defined by having a tag that matches
`ork-entry-tag-re'."
  (seq-filter (apply-partially 'string-match-p ork-entry-tag-re)
              (org-roam-node-tags node)))

(defun ork--directory-node-p (node)
  "Return non-nil if the current node is the current directory's directory-node."
  (and (= 0 (org-roam-node-level node))
       (string-match-p ork-directory-file-node-re
                       (file-name-nondirectory (org-roam-node-file node)))))

(defun ork--buffer-p ()
  "Return true if the current buffer is ork buffer."
  (and (boundp 'ork--buffer)
       ork--buffer))

;;;;;; Retrieval of content, metadata, related nodes, etc.

(defun ork--node-content (node)
  "Extract the content of NODE until the next sibling or child,
excluding the property drawer."
  (org-with-point-at (org-roam-node-marker node)
    (org-with-wide-buffer
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

(defun ork--next-node (node &optional prev same-level)
  "Find the next node, corresponding to the next \"physical\" zettel.
If PREV is non-nil then find the previous node. If SAME-LEVEL is
non-nil then find the next or previous node at the same level under
the current parent heading."
  (if (and same-level
           (= 0 ork--current-level))
      (ork--next-node-other-file node prev t)
    (let ((begin (org-roam-node-point node))
          (fn (cond ((and prev same-level) (lambda ()
                                             (org-backward-heading-same-level 1)))
                    (same-level (lambda ()
                                  (org-forward-heading-same-level 1)))
                    (prev #'outline-previous-heading)
                    (t #'outline-next-heading))))
      (org-with-point-at (org-roam-node-marker node)
        (org-with-wide-buffer
         (while (and (funcall fn)
                     (not (org-entry-get (point) "ID"))))
         (if (equal node (org-roam-node-at-point))
             (ork--next-node-other-file node prev)
           (org-roam-node-at-point)))))))

(defun ork--next-node-other-file (node &optional prev root)
  "find the first node in a file next to that of NODE.
If PREV, find the last node in a previous file.
If ROOT, only consider level-0 nodes."
  (let* ((node-file (org-roam-node-file node))
         (dir-org-files (directory-files (file-name-directory node-file)
                                         t "\\.org"))
         (add (if prev -1 1))
         (next-index (+ add (seq-position dir-org-files node-file)))
         next-node)
    (while (not (or (= next-index -1)
                    (= next-index (length dir-org-files))
                    next-node))
      (let* ((query (vector :select [id level] :from 'nodes
                            :where `(= file ,(nth next-index dir-org-files))
                            :order :by 'pos))
             (query-result (seq-filter (lambda (result)
                                         (if root
                                             (= 0 (cadr result)) t))
                                       (org-roam-db-query query))))
        (when query-result
          (setq next-node (org-roam-node-from-id
                           (if prev
                               (caar (last query-result))
                             (caar query-result)))))
        (setq next-index (+ add next-index))))
    next-node))

(defun ork--child-nodes (node)
  (org-with-point-at (org-roam-node-marker node)
    (let* ((level (org-roam-node-level node))
           (node-tree (org-map-entries 'org-roam-node-at-point nil
                                       (if (= level 0) 'file 'tree)))
           (heading-child-nodes (seq-filter (lambda (node)
                                              (= (+ level 1) (org-roam-node-level node)))
                                            node-tree)))
      (if (ork--directory-node-p node)
          (append (seq-filter (lambda (node) (not (ork--directory-node-p node)))
                              (mapcar (lambda (file)
                                        (let ((query (vector :select 'id :from 'nodes
                                                             :where `(and (= level 0)
                                                                          (= file ,file)))))
                                          (org-roam-node-from-id (caar (org-roam-db-query query)))))
                                      (directory-files (file-name-directory (org-roam-node-file node)) t "\\.org$")))
                  heading-child-nodes)
        heading-child-nodes))))

(defun ork--sibling-titles (node)
  "Returns the titles of all sibling nodes of NODE.
The titles are returned as a list of two lists, the first of
preceding nodes, the second of following nodes."
  (org-with-point-at (org-roam-node-marker node)
    (let ((level (org-roam-node-level node)))
      (if (zerop level)
          '(nil nil)
        (let* ((nodes-at-level (org-map-entries (lambda ()
                                                  (org-roam-node-title (org-roam-node-at-point)))
                                                (concat "+LEVEL=" (number-to-string level) "+ID={.+}")
                                                'file))
               (node-index (seq-position nodes-at-level (org-roam-node-title node))))
          (list (seq-take nodes-at-level node-index) (seq-drop nodes-at-level (1+ node-index))))))))

(defun ork--parent-node (node)
  (if (> (org-roam-node-level node) 0)
      (org-with-point-at (org-roam-node-marker node)
        (while (and
                (org-up-heading-or-point-min)
                (not (org-roam-node-at-point))))
        (org-roam-node-at-point))
    (let ((dir (file-name-directory (org-roam-node-file node)))
          parent-node)
      (when (ork--directory-node-p node)
        (setq dir (file-name-directory (directory-file-name dir))))
      (while (and (not parent-node)
                  (>= (length dir) (length (expand-file-name org-roam-directory))))
        (setq parent-node (ork--directory-node dir))
        (setq dir (file-name-directory (directory-file-name dir))))
      parent-node)))

(defun ork--directory-node (directory)
  "Find the directory node of DIRECTORY.
nil if none."
  (let ((node-file (car (directory-files directory t ork-directory-file-node-re))))
    (when node-file
      (with-current-buffer (find-file-noselect node-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-roam-node-at-point))))))

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

(defun ork--load-refresh (node &optional preserve-history folded)
  "Loads NODE and refreshes the buffer.
Passes PRESERVE-HISTORY to `ork--load-node'.
Passes FOLDED to `ork--refresh-buffer'."
  (ork--load-node node preserve-history)
  (ork--refresh-buffer folded))

;;;; Footer

(provide 'org-roam-kasten)

;;; org-roam-kasten.el ends here
