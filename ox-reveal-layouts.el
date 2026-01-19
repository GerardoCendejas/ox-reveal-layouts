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
    (search-forward "#+TITLE: " nil t)
    (forward-char 8)))

(defun ox-reveal-layouts-insert-side-by-side (&optional args)
  "Insert a side-by-side layout.  Asks for Image+Caption pairs sequentially.
Optional argument ARGS Transient arguments."
  (interactive (list (transient-args 'ox-reveal-layouts-menu)))
  
  (let* (;; Detect if --caption flag is present
         (use-caption (member "--caption" args))
         
         ;; First Block: Ask for Image and then Caption
         (path-a (file-relative-name (read-file-name "Choose Image Left: ")))
         (cap-a (if use-caption (read-string "Caption Left: ") nil))
         
         ;; Second Block: Ask for Image and then Caption
         (path-b (file-relative-name (read-file-name "Choose Image Right: ")))
         (cap-b (if use-caption (read-string "Caption Right: ") nil)))
    
    (insert
     (format
      "#+BEGIN_EXPORT html

<div class=\"orf-slide-container\">
  <div class=\"orf-layout-2-cols\">
  
    <figure class=\"orf-figure\">
      <img src=\"%s\">
      %s
    </figure>

    <figure class=\"orf-figure\">
      <img src=\"%s\">
      %s
    </figure>

  </div>
</div>

#+END_EXPORT\n"
      ;; Fill in the values
      path-a
      (if (and cap-a (not (string-empty-p cap-a)))
          (format "<figcaption>%s</figcaption>" cap-a) "")
      
      path-b
      (if (and cap-b (not (string-empty-p cap-b)))
          (format "<figcaption>%s</figcaption>" cap-b) "")
      ))))

(defun ox-reveal-layouts-insert-split-text-left (&optional args)
  "Insert a layout with text on the left and an image on the right.
Supports --caption.
Optional argument ARGS Transient arguments."
  (interactive (list (transient-args 'ox-reveal-layouts-menu)))
  
  (let* ((path-img (file-relative-name (read-file-name "Choose Image Right: ")))
         ;; Detect if --caption flag is present
         (use-caption (member "--caption" args))
         ;; If caption is to be used, ask for caption text
         (caption-text (if use-caption (read-string "Caption Right: ") nil)))
    
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

    <figure class=\"orf-figure\">
      <img src=\"%s\">
      %s
    </figure>

  </div>
</div>

#+END_EXPORT\n"
      path-img
      ;; Insert caption if provided
      (if (and caption-text (not (string-empty-p caption-text)))
          (format "<figcaption>%s</figcaption>" caption-text)
        "")))
    
    ;; Position cursor for text input
    (search-backward "# Write your text here")))

(defun ox-reveal-layouts-insert-split-text-right (&optional args)
  "Insert a layout with text on the right and an image on the left.
Supports --caption.
Optional argument ARGS Transient arguments."
  (interactive (list (transient-args 'ox-reveal-layouts-menu)))
  
  (let* ((path-img (file-relative-name (read-file-name "Choose Image Left: ")))
         ;; Detect if --caption flag is present
         (use-caption (member "--caption" args))
         ;; If caption is to be used, ask for caption text
         (caption-text (if use-caption (read-string "Caption Left: ") nil)))
    
    (insert
     (format
      "#+BEGIN_EXPORT html

<div class=\"orf-slide-container\">
  <div class=\"orf-layout-2-cols\">

    <figure class=\"orf-figure\">
      <img src=\"%s\">
      %s
    </figure>

    <div class=\"orf-text-container\">

#+END_EXPORT

# Write your text here

#+BEGIN_EXPORT html

    </div>
  </div>
</div>

#+END_EXPORT\n"
      path-img
      ;; Insert caption if provided
      (if (and caption-text (not (string-empty-p caption-text)))
          (format "<figcaption>%s</figcaption>" caption-text)
        "")))
    
    ;; Position cursor for text input
    (search-backward "# Write your text here")))

(defun ox-reveal-layouts-insert-grid-4 (&optional args)
  "Insert a 2x2 grid layout with four images.  Supports --caption.
Optional argument ARGS Transient arguments."
  (interactive (list (transient-args 'ox-reveal-layouts-menu)))
  
  (let* ((use-caption (member "--caption" args))
         
         ;; First Block: Ask for Image and then Caption
         (path-a (file-relative-name (read-file-name "Choose Image (Top-Left): ")))
         (cap-a (if use-caption (read-string "Caption (Top-Left): ") nil))
         
         ;; Second Block: Ask for Image and then Caption
         (path-b (file-relative-name (read-file-name "Choose Image (Top-Right): ")))
         (cap-b (if use-caption (read-string "Caption (Top-Right): ") nil))
         
         ;; Third Block: Ask for Image and then Caption
         (path-c (file-relative-name (read-file-name "Choose Image (Bottom-Left): ")))
         (cap-c (if use-caption (read-string "Caption (Bottom-Left): ") nil))
         
         ;; Fourth Block: Ask for Image and then Caption
         (path-d (file-relative-name (read-file-name "Choose Image (Bottom-Right): ")))
         (cap-d (if use-caption (read-string "Caption (Bottom-Right): ") nil)))
    
    (insert
     (format
      "#+BEGIN_EXPORT html

<div class=\"orf-slide-container\">
  <div class=\"orf-layout-grid-4\">
  
    <figure class=\"orf-figure\">
      <img src=\"%s\">
      %s
    </figure>
    
    <figure class=\"orf-figure\">
      <img src=\"%s\">
      %s
    </figure>
    
    <figure class=\"orf-figure\">
      <img src=\"%s\">
      %s
    </figure>
    
    <figure class=\"orf-figure\">
      <img src=\"%s\">
      %s
    </figure>

  </div>
</div>

#+END_EXPORT\n"
      ;; Fill in the values
      path-a (if (and cap-a (not (string-empty-p cap-a))) (format "<figcaption>%s</figcaption>" cap-a) "")
      path-b (if (and cap-b (not (string-empty-p cap-b))) (format "<figcaption>%s</figcaption>" cap-b) "")
      path-c (if (and cap-c (not (string-empty-p cap-c))) (format "<figcaption>%s</figcaption>" cap-c) "")
      path-d (if (and cap-d (not (string-empty-p cap-d))) (format "<figcaption>%s</figcaption>" cap-d) "")
      ))))

;;; Vertical Layouts

(defun ox-reveal-layouts-insert-image-centered (&optional args)
  "Insert a layout with a centered image.  Supports optional --caption switch.
Optional argument ARGS Tansient arguments."
  (interactive (list (transient-args 'ox-reveal-layouts-menu)))
  
  (let* ((path (file-relative-name (read-file-name "Choose Image: ")))
         ;; Detect if --caption flag is present
         (use-caption (member "--caption" args))
         ;; If caption is to be used, ask for caption text
         (caption-text (if use-caption (read-string "Caption: ") nil)))
    
    (insert
     (format
      "#+BEGIN_EXPORT html
<div class=\"orf-slide-container\">
  <figure class=\"orf-figure\">
    <img src=\"%s\">
    %s
  </figure>
</div>
#+END_EXPORT\n"
      path
      ;; Insert caption if provided
      (if caption-text
          (format "<figcaption>%s</figcaption>" caption-text)
        "")
      ))))

(defun ox-reveal-layouts-insert-stack (&optional args)
  "Insert a vertical stack layout (2 images) reusing the vertical layout CSS.
Supports the --caption switch.
Optional argument ARGS list of transient arguments."
  (interactive (list (transient-args 'ox-reveal-layouts-menu)))
  
  (let* ((use-caption (member "--caption" args))
         
         ;; --- Top Image ---
         (path-top (file-relative-name (read-file-name "Choose Image (Top): ")))
         (cap-top (if use-caption (read-string "Caption (Top): ") nil))
         
         ;; --- Bottom Image ---
         (path-bot (file-relative-name (read-file-name "Choose Image (Bottom): ")))
         (cap-bot (if use-caption (read-string "Caption (Bottom): ") nil)))
    
    (insert
     (format
      "#+BEGIN_EXPORT html

<div class=\"orf-slide-container\">
  <div class=\"orf-layout-vertical\">
  
    <div class=\"orf-vertical-img\">
      <figure class=\"orf-figure\">
        <img src=\"%s\">
        %s
      </figure>
    </div>

    <div class=\"orf-vertical-img\">
      <figure class=\"orf-figure\">
        <img src=\"%s\">
        %s
      </figure>
    </div>

  </div>
</div>

#+END_EXPORT\n"
      ;; Fill Top Data
      path-top (if (and cap-top (not (string-empty-p cap-top)))
                   (format "<figcaption>%s</figcaption>" cap-top) "")
      ;; Fill Bottom Data
      path-bot (if (and cap-bot (not (string-empty-p cap-bot)))
                   (format "<figcaption>%s</figcaption>" cap-bot) "")
      ))))

(defun ox-reveal-layouts-insert-text-top-img-bottom (&optional args)
  "Insert a layout with text on top and an image at the bottom.
Supports the --caption switch from the menu.
Optional argument ARGS list of transient arguments."
  (interactive (list (transient-args 'ox-reveal-layouts-menu)))
  
  (let* ((path (file-relative-name (read-file-name "Choose Image (Bottom): ")))
         ;; Detect if the --caption flag is active
         (use-caption (member "--caption" args))
         ;; Ask for caption text if needed
         (caption-text (if use-caption (read-string "Caption (Bottom): ") nil)))
    
    (insert
     (format
      "#+BEGIN_EXPORT html

<div class=\"orf-slide-container\">
  <div class=\"orf-layout-vertical\">
    
    <div class=\"orf-vertical-text\">

#+END_EXPORT

# Write your text here...

#+BEGIN_EXPORT html

    </div>

    <div class=\"orf-vertical-img\">
      <figure class=\"orf-figure\">
        <img src=\"%s\">
        %s
      </figure>
    </div>

  </div>
</div>

#+END_EXPORT\n"
      path
      ;; Insert caption if provided
      (if (and caption-text (not (string-empty-p caption-text)))
          (format "<figcaption>%s</figcaption>" caption-text)
        "")))
    
    ;; Move the cursor to the text area
    (search-backward "# Write your text here")))

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
  ;; -- MAIN HORIZONTAL BLOCK (These groups will appear side-by-side) --
  [
   ;; COLUMN 1: Setup & Extras (Grouped to save space) Transient arguments.
   ["Setup & Tools"
    ("n" "New Template" ox-reveal-layouts-init-presentation)
    ("m" "Inject CSS" ox-reveal-layouts-setup-css)]

   ;; COLUMN 2: Pure Image Layouts
   ["Image Layouts"
    ("i" "Centered Image" ox-reveal-layouts-insert-image-centered)
    ("s" "Side-by-Side" ox-reveal-layouts-insert-side-by-side)
    ("g" "Grid 4 Images" ox-reveal-layouts-insert-grid-4)
    ("k" "Vertical Stack" ox-reveal-layouts-insert-stack)
    ]

   ;; COLUMN 3: Text & Image Mixed
   ["Text & Image"
    ("v" "Text Top | Image Bottom" ox-reveal-layouts-insert-text-top-img-bottom)
    ("r" "Text Left | Image Right" ox-reveal-layouts-insert-split-text-left)
    ("l" "Image Left | Text Right" ox-reveal-layouts-insert-split-text-right)]

   ;; COLUMN 4: Pins & Citations
   ["Extra Info"
    ("p" "Pins / Stickers" ox-reveal-layouts-pin-menu)
    ("c" "Citation / Footer" org-reveal-layouts-insert-citation)]

   ;; COLUMN 5: Options
   ["Options"
   ("-c" "Add Caption to Image(s)" "--caption")]
   ]

  ;; -- BOTTOM BLOCK (Separate, for exiting) --
  [""
   ("q" "Exit" transient-quit-one)]
  )

(provide 'ox-reveal-layouts)

;;; ox-reveal-layouts.el ends here
