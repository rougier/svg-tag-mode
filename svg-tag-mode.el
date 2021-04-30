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

(defun svg-tag--font-weight (face)
  (cdr (assq (face-attribute face :weight nil 'default)
             svg-tag--font-weights)))

(defun svg-tag-make (text &optional face inner-padding outer-padding radius)
  "Create a SVG image displaying TEXT in a rounded box using FACE style.
INNER-PADDING, OUTER-PADDING and RADIUS controls the visual aspect of the box."
  (svg-tag--make
   (string-trim text)
   (or face 'svg-tag-default-face)
   (or inner-padding svg-tag-default-inner-padding)
   (or outer-padding svg-tag-default-outer-padding)
   (or radius svg-tag-default-radius)))

(defun svg-tag--make (text face padding margin radius)
  (let* ((foreground  (face-attribute face :foreground nil t))
         (background  (face-attribute face :background nil t))
         (box         (face-attribute face :box nil t))
         (box         (if (eq box 'unspecified) nil box))
         (box-color   (or (plist-get box :color) foreground))
         (box-width   (/ (or (plist-get box :line-width) 1) 2.0))
         (font-family (face-attribute face :family nil 'default))
         (font-weight (alist-get (face-attribute face :weight nil 'default)
                                 svg-tag--font-weights))
         (txt-width   (window-font-width))
         (svg-width   (* txt-width (+ (length text) padding margin)))
         (tag-width   (* txt-width (+ (length text) padding)))
         (tag-x       (* txt-width (/ margin 2.0)))
         (text-x      (+ tag-x
                         (/ (- tag-width (* (length text) txt-width))
                            2)))
         (font-size   (* (ceiling
                          (* (face-attribute face :height nil 'default)
                             0.1))
                         (image-compute-scaling-factor 'auto)))
         (txt-height  (window-font-height))
         (svg-height  txt-height)
         (tag-height  (- txt-height 2))
         (text-y      font-size)
         (svg         (svg-create svg-width svg-height)))
    (svg-rectangle svg tag-x 0 tag-width tag-height
                   :fill (if box box-color background)
                   :rx   radius)
    (when box
      (svg-rectangle svg
                     (+ tag-x         box-width)
                     (+ 0             box-width)
                     (- tag-width  (* box-width 2))
                     (- tag-height (* box-width 2))
                     :fill background
                     :rx   (- radius box-width)))
    (svg-text svg text
              :font-family font-family
              :font-weight font-weight
              :font-size   font-size
              :fill        foreground
              :x           text-x
              :y           text-y)
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
