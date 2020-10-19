;;; svg-tag-mode.el --- Replace keywords with SVG tags  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/svg-tag-mode
;; Keywords: convenience

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
;; see <http://www.gnu.org/licenses/>.

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
;; 2. Replace any letter betwen @ with a circle
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
  "Default border line width  (in pixels, null or positive)"
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
       :background "orange"
       :box (:line-width 1 :color "orange" :style nil)
       :family ,(face-attribute 'default :family)
       :weight ,(face-attribute 'default :weight)
       :height 120))
  "Default face for tag"
  :group 'svg-tag)

(defcustom svg-tag-tags
  '((":TODO:" . (svg-tag-make "TODO")))
  "An alist mapping keywords to tags used to display them.

Each entry has the form (keyword . tag). Keyword is used as part
of a regular expression and tag can be either a svg tag
previously created by svg-tag-make or a function that takes a
string as argument and returns a tag. When tag is a function, this
allows to create dynamic tags."
  :group 'svg-tag
  :type '(repeat (cons (string :tag "Keyword")
                       (sexp   :tag "Tag"))))


(defun svg-tag-make (text &optional face inner-padding outer-padding radius)
  (let* ((face       (or face 'svg-tag-default-face))
         (foreground (face-attribute face :foreground))
         (background (face-attribute face :background))
         (box        (face-attribute face :box))
         (stroke     (or (plist-get (face-attribute face :box) :color)
                         foreground))
         ;; This does not seem to get the actual box line-width
         (line-width (or (plist-get (face-attribute face :box) :line-width)
                         svg-tag-default-line-width))
         (family     (face-attribute face :family))
         (weight     (face-attribute face :weight))
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
    (svg-rectangle svg (+ tag-x (/ line-width 2.0)) (/ line-width 2.0)
                       (- tag-width line-width) (- tag-height line-width)
                   :fill        background
                   :rx          (- radius (/ line-width 2.0)))
    (svg-text      svg text 
                   :font-family family
                   :font-weight weight
                   :font-size   size
                   :fill        foreground
                   :x           (+ text-x svg-tag-horizontal-offset)
                   :y           (+ text-y svg-tag-vertical-offset))
    (svg-image svg :ascent 'center)))


(defun tag-svg--build-keywords (item)
  (let ((pattern  (format "\\(%s\\)" (car item)))
        (tag      (cdr item)))
    (when (and (symbolp tag) (fboundp tag))
      (setq tag `(,tag (match-string 0))))
    (setq tag  ``(face nil display ,,tag))
    `(,pattern 1 ,tag)))


(defun svg-tag-mode-on ()
  (add-to-list 'font-lock-extra-managed-props 'display)
  (when svg-tag-tags--active
    (font-lock-remove-keywords nil
          (mapcar 'tag-svg--build-keywords svg-tag-tags--active)))
  (when svg-tag-tags
    (font-lock-add-keywords nil
                            (mapcar 'tag-svg--build-keywords svg-tag-tags)))
  (setq svg-tag-tags--active (copy-sequence svg-tag-tags))
  (message "SVG tag mode on"))

(defun svg-tag-mode-off ()
  (when svg-tag-tags--active
    (font-lock-remove-keywords nil
               (mapcar 'tag-svg--build-keywords svg-tag-tags--active)))
  (setq svg-tag-tags--active nil)
  (message "SVG tag mode off"))

(define-minor-mode svg-tag-mode
  "Minor mode for graphical tag as rounded box."
  :group 'svg-tag
  (if svg-tag-mode (svg-tag-mode-on) (svg-tag-mode-off))
  (font-lock-flush))

(provide 'svg-tag-mode)

;;; svg-tag-mode.el ends here
