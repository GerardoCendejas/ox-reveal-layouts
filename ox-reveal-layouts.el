;;; ox-reveal-layouts.el --- Predefined layouts for ox-reveal  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2025 Gerardo Cendejas Mendoza

;; Author: Gerardo Cendejas Mendoza <gc597@cornell.edu>
;; Maintainer: Gerardo Cendejas Mendoza <gc597@cornell.edu>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; Keywords: ox-reveal, layouts, presentations
;; URL: https://github.com/GerardoCendejas/ox-reveal-layouts

;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;  This package provides a set of predefined layouts for use with ox-reveal presentations.
;;  It includes a transient menu for easy insertion of these layouts into your org-mode files.

;;; Code:

(require 'transient)

;;; 1. Variables and Configuration

(defvar ox-reveal-layouts-css-path
  (expand-file-name "ox-reveal-layouts.css" (file-name-directory (or load-file-name buffer-file-name)))
  "Route to the CSS file with the layout definitions.")

;;; 2. Functions to Insert Layouts

(defun orl-insert-grid-4 ()
  "Insert a 4-image grid layout quickly with placeholder images."
  (interactive)
  (let* ((path-a (file-relative-name (read-file-name "Choose Image A (Top-Left): ")))
	 (path-b (file-relative-name (read-file-name "Choose Image B (Top-Right): ")))
	 (path-c (file-relative-name (read-file-name "Choose Image C (Bottom-Left): ")))
	 (path-d (file-relative-name (read-file-name "Choose Image D (Bottom-Right): "))))
    
    (insert
     (format
      "#+BEGIN_EXPORT html
<div class=\"orf-grid-4\">
  <div class=\"orf-grid-item\">
    <span>a)</span>
    <img src=\"%s\" alt=\"Image A\">
  </div>
  <div class=\"orf-grid-item\">
    <span>b)</span>
    <img src=\"%s\" alt=\"Image B\">
  </div>
  <div class=\"orf-grid-item\">
    <span>c)</span>
    <img src=\"%s\" alt=\"Image C\">
  </div>
  <div class=\"orf-grid-item\">
    <span>d)</span>
    <img src=\"%s\" alt=\"Image D\">
  </div>
</div>
#+END_EXPORT\n"
      path-a path-b path-c path-d)))
  )

;;; 3. Transient Menu Definition

;; Defining the transient menu for layout insertion
(transient-define-prefix ox-reveal-layouts-menu ()
  "Main menu for layouts."
  ["Quick Insert Layouts"
   ("g" "Grid 4 Images" orl-insert-grid-4)]
  
  ["CSS Management"
   ("i" "Inicializar CSS en buffer" (lambda () (interactive) (message "CSS inyectado (TODO)")))]
  
  ["Help & Exit"
   ("q" "Exit" transient-quit-one)])

(provide 'ox-reveal-layouts)

;;; ox-reveal-layouts.el ends here
