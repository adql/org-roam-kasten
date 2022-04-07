;;; ork-get.el --- Org Roam Kasten: content retrieval ""  -*- lexical-binding: t; -*-

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

;;;; Functions

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
           (= 0 ork--current-level))    ;FIXME -- shouldn't refer to buffer
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

;;;; Footer

(provide 'ork-get)

;;; ork-get.el ends here
