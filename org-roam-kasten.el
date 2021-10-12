(defvar ork-buffer-name "Zettelkasten")

(defvar ork-index-tag "@topic"
  "Tag defining Zettelkasten entry nodes.")

(make-variable-buffer-local 'ork--buffer)

(define-minor-mode org-roam-kasten-mode ""
  :lighter " ork"
  :keymap '(("n" . ork-next-physical-zettel)
            ("p" . ork-previous-physical-zettel)
            ("o" . ork-follow-link-at-point)
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

(defun ork-cycle-zettel ()
  (interactive)
  (save-excursion
    (if (char-equal ?* (char-after (point-min)))
        (org-cycle-internal-local))))

(defun ork--load-node (node)
  "Loads and parses NODE into the buffer-local variables."
  (setq-local ork--current-node node
              ork--current-title (org-roam-node-title node)
              ork--current-level (org-roam-node-level node)
              ork--current-content (ork--node-content node)))

(defun ork--display-buffer ()
  "(Re)display the kasten buffer with the current node."
  (when (ork--buffer-p)                 ;safety measure
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "* " ork--current-title "\n\n" ork--current-content)
      (goto-char (point-min)))))

(defun ork--load-display (kasten node)
  "Loads a node and (re)displays the buffer."
  (with-current-buffer kasten
    (ork--load-node node)
    (ork--display-buffer)))

(defun ork--get-buffer-create ()
  (let ((buf (get-buffer-create ork-buffer-name)))
    (with-current-buffer buf
      (cd org-roam-directory)
      (org-mode)
      (org-roam-kasten-mode)
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
    (ork--load-display kasten node)
    (switch-to-buffer kasten)
    (recenter)))

(defun ork-follow-link-at-point ()
  "Follow the link in the kasten buffer if it's a node;
otherwise open it normally."
  (interactive)
  (let ((object (org-element-context)))
    (if (and (string-equal "link" (org-element-type object))
             (string-equal "id" (org-element-property :type object)))
        (let ((node (org-roam-node-from-id (string-trim-left
                                            (org-element-property :raw-link object)
                                            "id:"))))
          (if node
              (ork--load-display (current-buffer) node)
            (org-open-at-point)))
      (org-open-at-point))))

(defun ork-next-physical-zettel (&optional prev)
  "Display the next physical zettel in the current kasten.
If PREV, display the previous physical zettel."
  (interactive)
  (let ((next-zettel (ork--next-physical-node ork--current-node prev)))
    (when next-zettel
      (ork--load-display (current-buffer) next-zettel))))

(defun ork-previous-physical-zettel ()
  "Display the previous physical zettel in the current kasten."
  (interactive)
  (ork-next-physical-zettel t))
