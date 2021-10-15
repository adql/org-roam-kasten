(defvar ork-buffer-name "Zettelkasten")

(defvar ork-index-tag "@topic"
  "Tag defining Zettelkasten entry nodes.")

(make-variable-buffer-local 'ork--buffer)

(define-minor-mode org-roam-kasten-mode ""
  :lighter " ork"
  :keymap '(("]" . ork-next-physical-zettel)
            ("[" . ork-previous-physical-zettel)
            (" " . ork-examine-folgezettel)
            ("" . ork-follow-folgezettel-or-link-at-point)
            ("q" . quit-window)))

(defun ork--index-p (node)
  "Query whether the node is an index node. Only nodes tagged
with `ork-index-tag' will be included in the
completion buffer."
  (member ork-index-tag (org-roam-node-tags node)))

(defun ork--buffer-p ()
  "Return true if the current buffer is ork buffer."
  (and (boundp 'ork--buffer)
       ork--buffer))

;; (defun ork--node-begin-point (node)
;;   (- (org-roam-node-point node) 1))

;; (defun ork--node-end-point (node)
;;   (let ((begin (org-roam-node-point node))
;;         (level (org-roam-node-level node)))
;;     (save-excursion
;;       (set-buffer (org-roam-node-find-noselect node))
;;       (goto-char begin)
;;       (while (and (outline-next-heading)
;;                   (not (org-entry-get (point) "ID"))
;;                   (<= level (org-current-level))))
;;       (if (= (point) (point-max))
;;           (point-max)
;;         (- (point) 1)))))

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

(defun ork-cycle-zettel ()
  (interactive)
  (save-excursion
    (if (char-equal ?* (char-after (point-min)))
        (org-cycle-internal-local))))

(defun ork--load-node (node)
  "Loads and parses NODE into the buffer-local variables."
  (when (ork--buffer-p)                 ;safety measure
    (setq-local ork--current-node node
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

(defun ork--display-buffer ()
  "(Re)display the kasten buffer with the current node."
  (when (ork--buffer-p)                 ;safety measure
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "# " (int-to-string (length ork--current-child-nodes)) " folgezettel\n\n")
      (insert "* " ork--current-title "\n\n" ork--current-content)
      (outline-previous-heading)
      (org-cycle-internal-local))))

(defun ork--load-display (node)
  "Loads NODE and (re)displays the buffer."
  (ork--load-node node)
  (ork--display-buffer))

(defun ork--get-buffer-create ()
  (let ((buf (get-buffer-create ork-buffer-name)))
    (with-current-buffer buf
      (cd org-roam-directory)
      (org-mode)
      (org-roam-kasten-mode)
      (flyspell-mode-off)
      (setq ork--buffer t)
      (read-only-mode))
    buf))

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
      (ork--load-display next-zettel))))

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
