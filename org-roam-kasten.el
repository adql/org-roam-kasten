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

(require 'ork-buffer)
(require 'ork-get)

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
         (kasten (ork-buffer-get-buffer-create)))
    (set-buffer kasten)
    (ork-buffer-load-refresh node)
    (switch-to-buffer kasten)
    (recenter)))

;;;; Footer

(provide 'org-roam-kasten)

;;; org-roam-kasten.el ends here
