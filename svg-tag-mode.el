;;; svg-tag-mode.el --- Replace keywords with SVG tags -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/svg-tag-mode
;; Keywords: convenience
;; Version: 0.1

;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor mode replaces keywords or expressions with SVG rounded
;; box labels that are fully customizable.
;;
;; Usage example:
;; --------------
;;
;; 1. Replace :TODO: keyword with default face/padding/radius
;;
;;    (setq svg-tag-tags '((":TODO:"  (svg-tag-make "TODO")))
;;    (svg-tag-mode)
;;
;;
;; 2. Replace any letter between () with a circle
;;
;;    (defun svg-tag-round (text)
;;      (svg-tag-make (substring text 1 -1) nil 1 1 12))
;;    (setq svg-tag-tags '(("([0-9])" svg-tag-round)))
;;    (svg-tag-mode)
;;

;;; Code:

(require 'svg)
(eval-when-compile (require 'subr-x))

;; (defvar svg-tag-tags nil)
(defvar svg-tag-tags--active nil)

(defgroup svg-tag nil
  "Replace keywords with SVG rounded box labels"
  :group 'convenience
  :prefix "svg-tag-")

(defcustom svg-tag-default-outer-padding 1
  "Default outer padding (in characters, null or positive)."
  :type 'integer
  :group 'svg-tag)

(defcustom svg-tag-default-inner-padding 1
  "Default inner padding (in characters, null or positive)."
  :type 'integer
  :group 'svg-tag)

(defcustom svg-tag-default-radius 3
  "Default radius  (in pixels, null or positive)."
  :type 'integer
  :group 'svg-tag)

(defcustom svg-tag-default-line-width 1
  "Default border line width  (in pixels, null or positive)."
  :type 'integer
  :group 'svg-tag)

(defcustom svg-tag-vertical-offset 0
  "Vertical offset for text (in pixels).
This should be zero for most fonts but some fonts may need this."
  :type 'integer
  :group 'svg-tag)

(defcustom svg-tag-horizontal-offset 0
  "Horizontal offset for text (in pixels).
This should be zero for most fonts but some fonts may need this."
  :type 'integer
  :group 'svg-tag)

(defface svg-tag-default-face
  `((t :foreground "white"
       :background "#FFAB91"
       :box (:line-width 1 :color "#FFAB91" :style nil)
       :family ,(face-attribute 'default :family)
       :weight ,(face-attribute 'default :weight)
       :height ,(if (display-graphic-p)
                    (- (face-attribute 'default :height) 20)
                  1)))
  "Default face for tag"
  :group 'svg-tag)

(defcustom svg-tag-tags
  '((" TODO " . (svg-tag-make "TODO")))
  "An alist mapping keywords to tags used to display them.

Each entry has the form (keyword . tag).  Keyword is used as part
of a regular expression and tag can be either a svg tag
previously created by `svg-tag-make' or a function that takes a
string as argument and returns a tag.  When tag is a function, this
allows to create dynamic tags."
  :group 'svg-tag
  :type '(repeat (cons (string :tag "Keyword")
                       (sexp   :tag "Tag"))))

;; SVG font weights translation
(defvar svg-tag--font-weights '((thin       . 100)
                                (ultralight . 200)
                                (light      . 300)
                                (regular    . 400)
                                (medium     . 500)
                                (semibold   . 600)
                                (bold       . 700)
                                (extrabold  . 800)
                                (black      . 900)))

(defun svg-tag-make (text &optional face inner-padding outer-padding radius)
  "Create a SVG image displaying TEXT in a rounded box using FACE style.
INNER-PADDING, OUTER-PADDING and RADIUS controls the visual aspect of the box."
  (let* ((face       (or face 'svg-tag-default-face))
         (foreground (face-attribute face :foreground))
         (background (face-attribute face :background))
         (stroke     (or (plist-get (face-attribute face :box) :color)
                         foreground))
         ;; This does not seem to get the actual box line-width
         (line-width (or (plist-get (face-attribute face :box) :line-width)
                         svg-tag-default-line-width))
         (family     (face-attribute face :family))
         ;; (weight     (face-attribute face :weight))
         (weight     (cdr (assoc (face-attribute face :weight)
                                 svg-tag--font-weights)))
         (size       (/ (face-attribute face :height) 10))

         (tag-char-width  (window-font-width nil face))
         (tag-char-height (window-font-height nil face))
         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         (inner-padding   (or inner-padding svg-tag-default-inner-padding))
         (outer-padding   (or outer-padding svg-tag-default-outer-padding))

         (text (string-trim text))
         (tag-width (* (+ (length text) inner-padding) txt-char-width))
         (tag-height (* txt-char-height 0.9))

         (svg-width (+ tag-width (* outer-padding txt-char-width)))
         (svg-height tag-height)

         (tag-x (/ (- svg-width tag-width) 2))
         (text-x (+ tag-x (/ (- tag-width (* (length text) tag-char-width)) 2)))
         (text-y (- tag-char-height (- txt-char-height tag-char-height)))

         (radius  (or radius svg-tag-default-radius))
         (svg (svg-create svg-width svg-height)))

    (svg-rectangle svg tag-x 0 tag-width tag-height
                   :fill        stroke
                   :rx          radius)
    (svg-rectangle svg
                   (+ tag-x (/ line-width 2.0))
                   (/ line-width 2.0)
                   (- tag-width line-width)
                   (- tag-height line-width)
                   :fill        background
                   :rx          (- radius (/ line-width 2.0)))
    (svg-text      svg text
                   :font-family family
                   :font-weight weight
                   :font-size   size
                   :fill        foreground
                   :x           (+ text-x svg-tag-horizontal-offset)
                   :y           (+ text-y svg-tag-vertical-offset))
    (svg-image svg :scale 1 :ascent 'center)))


(defun svg-tag--build-keywords (item)
  "Internal.  Build the list of keyword from ITEM."
  (let ((pattern  (format "\\(%s\\)" (car item)))
        (tag      (cdr item)))
    (when (and (symbolp tag) (fboundp tag))
      (setq tag `(,tag (match-string 0))))
    (setq tag  ``(face nil display ,,tag))
    `(,pattern 1 ,tag)))

(defun svg-tag-mode-on ()
  "Activate SVG tag mode."
  (add-to-list 'font-lock-extra-managed-props 'display)
  (when svg-tag-tags--active
    (font-lock-remove-keywords
     nil (mapcar #'svg-tag--build-keywords svg-tag-tags--active)))
  (when svg-tag-tags
    (font-lock-add-keywords
     nil (mapcar #'svg-tag--build-keywords svg-tag-tags)))
  (setq svg-tag-tags--active (copy-sequence svg-tag-tags))
  (message "SVG tag mode on")
  (font-lock-flush))

(defun svg-tag-mode-off ()
  "Deactivate SVG tag mode."
  (when svg-tag-tags--active
    (font-lock-remove-keywords
     nil (mapcar #'svg-tag--build-keywords svg-tag-tags--active)))
  (setq svg-tag-tags--active nil)
  (message "SVG tag mode off")
  (font-lock-flush))

(define-minor-mode svg-tag-mode
  "Minor mode for graphical tag as rounded box."
  :group 'svg-tag
  (if svg-tag-mode (svg-tag-mode-on) (svg-tag-mode-off)))

(define-globalized-minor-mode global-svg-tag-mode svg-tag-mode
  svg-tag-mode-on)

(provide 'svg-tag-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; svg-tag-mode.el ends here
