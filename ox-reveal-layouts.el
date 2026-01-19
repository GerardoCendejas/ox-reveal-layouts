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

(defgroup ox-reveal-layouts nil
  "Options for ox-reveal-layouts."
  :group 'org-export)

(defcustom ox-reveal-layouts-reveal-root-path "https://cdn.jsdelivr.net/npm/reveal.js"
  "Path to the reveal.js library."
  :type 'string
  :group 'ox-reveal-layouts)

(defcustom ox-reveal-layouts-title-slide-template nil
  "Path to a custom HTML template for the title slide."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Custom HTML"))
  :group 'ox-reveal-layouts)

;;; 2. Functions to Insert Layouts

(defun ox-reveal-layouts-setup-css ()
  "Insert the CSS link for ox-reveal layouts at the beginning of the document."
  (interactive)
  ;; Moves to the beginning of the buffer to insert the CSS link
  (save-excursion
    (goto-char (point-min))
    ;; Insert the CSS link
    (insert (format "#+REVEAL_EXTRA_CSS: %s\n" ox-reveal-layouts-css-path))
    (message "¡CSS link added! Reload reveal to see changes.")))

(defun ox-reveal-layouts-init-presentation ()
  "Insert a basic ox-reveal presentation header with user info and date."
  (interactive)
  (let ((title-template (if ox-reveal-layouts-title-slide-template
                            (format "#+REVEAL_TITLE_SLIDE_TEMPLATE: %s\n" ox-reveal-layouts-title-slide-template)
                          ""))) ;; If no template, leave empty
    
    (insert
     (format
      "#+TITLE:
#+AUTHOR: %s
#+DATE: %s
#+OPTIONS: toc:nil num:nil ^:{}
#+REVEAL_ROOT: %s
#+REVEAL_THEME: white
#+REVEAL_TRANS: fade
#+REVEAL_EXTRA_CSS: %s
%s
* First Slide

"
      user-full-name            ; Your full name (from Emacs)
      (format-time-string "%Y-%m-%d") ; Today's date
      ox-reveal-layouts-reveal-root-path      ; The path to reveal.js
      ox-reveal-layouts-css-path ; The CSS file path
      title-template))          ; The title slide template if any

    ;; Move cursor to the title line for easy editing
    (goto-char (point-min))
    (search-forward "#+TITLE: " nil t)))

(defun ox-reveal-layouts-insert-side-by-side ()
  "Insert a side-by-side layout with two images using a fixed container."
  (interactive)
  (let* ((path-a (file-relative-name (read-file-name "Choose Image Left: ")))
         (path-b (file-relative-name (read-file-name "Choose Image Right: "))))
    
    (insert
     (format
      "#+BEGIN_EXPORT html

<div class=\"orf-slide-container\">
  <div class=\"orf-layout-2-cols\">
    <img src=\"%s\" class=\"orf-img-fit\">
    <img src=\"%s\" class=\"orf-img-fit\">
  </div>
</div>

#+END_EXPORT\n"
      path-a path-b))))

(defun ox-reveal-layouts-insert-split-text-right ()
  "Insert a layout with text on the left and an image on the right.  Allow for text input on the left side."
  (interactive)
  (let ((path-img (file-relative-name (read-file-name "Choose Image Right: "))))
    (insert
     (format
      "#+BEGIN_EXPORT html

<div class=\"orf-slide-container\">
  <div class=\"orf-layout-2-cols\">
    <div class=\"orf-text-container\">

#+END_EXPORT

# Write your text here

#+BEGIN_EXPORT html

    </div>
    <img src=\"%s\" class=\"orf-img-fit\">
  </div>
</div>

#+END_EXPORT\n"
      path-img))
    ;; Position cursor for text input
    (search-backward "# Write your text here")))

(defun ox-reveal-layouts-insert-split-text-left ()
  "Insert a layout with text on the left and an image on the left.  Allow for text input on the left side."
  (interactive)
  (let ((path-img (file-relative-name (read-file-name "Choose Image Left: "))))
    (insert
     (format
      "#+BEGIN_EXPORT html

<div class=\"orf-slide-container\">
  <div class=\"orf-layout-2-cols\">
    <img src=\"%s\" class=\"orf-img-fit\">
    <div class=\"orf-text-container\">

#+END_EXPORT

# Write your text here

#+BEGIN_EXPORT html

    </div>
  </div>
</div>

#+END_EXPORT\n"
      path-img))
    ;; Position cursor for text input
    (search-backward "# Write your text here")))

(defun ox-reveal-layouts-insert-grid-4 ()
  "Insert a 2x2 grid layout with four images."
  (interactive)
  (let* ((path-a (file-relative-name (read-file-name "Choose Image A (Top-Left): ")))
	 (path-b (file-relative-name (read-file-name "Choose Image B (Top-Right): ")))
	 (path-c (file-relative-name (read-file-name "Choose Image C (Bottom-Left): ")))
	 (path-d (file-relative-name (read-file-name "Choose Image D (Bottom-Right): "))))
    
    (insert
     (format
      "#+BEGIN_EXPORT html

<div class=\"orf-slide-container\">
  <div class=\"orf-layout-grid-4\">
    <img src=\"%s\" class=\"orf-img-fit\">
    <img src=\"%s\" class=\"orf-img-fit\">
    <img src=\"%s\" class=\"orf-img-fit\">
    <img src=\"%s\" class=\"orf-img-fit\">
  </div>
</div>

#+END_EXPORT\n"
      path-a path-b path-c path-d)))
  )

;;; Pin Insertion Functions

(defun org-reveal-layouts-insert-pin-html (img-path class-or-style &optional is-custom)
  "Insert an HTML pin with given image path and class or style.
Argument IMG-PATH path to image to insert.
Argument CLASS-OR-STYLE CSS style.
Optional argument IS-CUSTOM Is CSS positioning custom."
  (let ((style-attr (if is-custom (format "style=\"%s\"" class-or-style) "")))
    (insert
     (format
      "\n#+BEGIN_EXPORT html

<img src=\"%s\" class=\"orf-pin %s\" %s>

#+END_EXPORT\n"
      img-path
      (if is-custom "" class-or-style) ; Use class if not custom
      style-attr))))

(defun org-reveal-layouts-insert-pin-preset (css-class)
  "Insert a pin using a preset CSS class for positioning.
Argument CSS-CLASS CSS class for pin positioning."
  (let ((path (file-relative-name (read-file-name "Choose Image (Pin): "))))
    (org-reveal-layouts-insert-pin-html path css-class nil)))

(defun org-reveal-layouts-insert-pin-custom ()
  "Insert a pin asking for Top and Left percentages (0-100)."
  (interactive)
  (let* ((path (file-relative-name (read-file-name "Choose Image (Pin): ")))
         ;; 1. We ask for the coordinates
         (top (read-number "Top Position % (0-100): "))
         (left (read-number "Left Position % (0-100): "))
         ;; 2. Format them into a style string
         ;; Example: "top: 20%; left: 30%;"
         (coords (format "top: %s%%; left: %s%%;" top left)))
    
    ;; 3. Insert the pin with custom styles
    (org-reveal-layouts-insert-pin-html path coords t)))

;; Interactive functions for preset pin positions
(defun org-reveal-layouts-pin-tl ()
  "INsert pin at Top-Left position."
  (interactive) (org-reveal-layouts-insert-pin-preset "orf-pos-tl"))
(defun org-reveal-layouts-pin-tr ()
  "Insert pin at Top-Right position."
  (interactive) (org-reveal-layouts-insert-pin-preset "orf-pos-tr"))
(defun org-reveal-layouts-pin-bl ()
  "Insert pin at Bottom-Left position."
  (interactive) (org-reveal-layouts-insert-pin-preset "orf-pos-bl"))
(defun org-reveal-layouts-pin-br ()
  "Insert pin at Bottom-Right position."
  (interactive) (org-reveal-layouts-insert-pin-preset "orf-pos-br"))

;; Citation Insertion Function
(defun org-reveal-layouts-insert-citation ()
  "Insert a citation container at the bottom of the slide.
Leaves cursor inside for manual typing or org-cite insertion."
  (interactive)
  (insert
   "
#+ATTR_HTML: :class orf-citation
#+BEGIN_div
  ")
  ;; Position cursor for citation input
  (let ((p (point)))
    (insert "\n#+END_div\n")
    (goto-char p)))

;;; 3. Transient Menu Definition

;; Defining the transient menu for pin positioning
(transient-define-prefix ox-reveal-layouts-pin-menu ()
  "Pin positioning menu."
  ["Preset Positions"
   ("q" "Top Left ↖" org-reveal-layouts-pin-tl)
   ("w" "Top Right ↗" org-reveal-layouts-pin-tr)
   ("a" "Bottom Left ↙" org-reveal-layouts-pin-bl)
   ("s" "Bottom Right ↘" org-reveal-layouts-pin-br)]
  
  ["Manual Positioning"
   ("c" "Custom Coordinates" org-reveal-layouts-insert-pin-custom)]
  
  ["Back"
   ("g" "Back to main manu" ox-reveal-layouts-menu)])

;; Defining the transient menu for layout insertion
(transient-define-prefix ox-reveal-layouts-menu ()
  "Main menu for layouts."
  ;; Menu structure
  ["Project Setup"
   ("n" "New Presentation Template" ox-reveal-layouts-init-presentation)
   ("i" "Inject CSS only" ox-reveal-layouts-setup-css)]

  ;; Add layout options here
  ["Image Layouts"
   ("g" "Grid 4 Images" ox-reveal-layouts-insert-grid-4)
   ("s" "Side-by-Side Images" ox-reveal-layouts-insert-side-by-side)
   ("r" "Image Right, Text Left" ox-reveal-layouts-insert-split-text-left)
   ("l" "Image Left, Text Right" ox-reveal-layouts-insert-split-text-right)]

  ["Extras"
   ("p" "Pins" ox-reveal-layouts-pin-menu)
   ("c" "Citation / Footer" org-reveal-layouts-insert-citation)]
  
  ["Help & Exit"
   ("q" "Exit" transient-quit-one)])

(provide 'ox-reveal-layouts)

;;; ox-reveal-layouts.el ends here
