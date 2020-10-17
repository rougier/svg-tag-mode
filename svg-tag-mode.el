;;; svg-tag-mode.el --- Replace keywords with SVG tags  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/svg-tag-mode
;; Keywords: convenience

;; Package-Requires: ((emacs "25"))

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

;; This minor mode replaces keywords or expressions with a SVG rounded
;; box label this customizable.
;;
;; Usage example:
;; --------------
;;
;; 1. Replace :TODO: keyword with default face/padding/radius
;;
;;    (setq svg-tag-todo (svg-tag-make "TODO"))
;;    (setq svg-tags
;;          '(("\\(:TODO:\\)" 1 `(face nil display ,svg-tag-todo))
;;    (svg-tag-mode)
;;
;;
;; 2. Replace :TODO: keyword with specific face/padding/radius
;;
;;    (defface svg-tag-todo-face
;;      '((t :foreground "black" :background "white" :box "black"
;;           :family "Roboto Mono" :weight light :height 120))
;;      "Face for note tag" :group nil)
;;    (setq svg-tag-todo (svg-tag-make "TODO" svg-tag-todo-face 1 1 3))
;;    (setq svg-tags
;;          '(("\\(:TODO:\\)" 1 `(face nil display ,svg-tag-todo))
;;    (svg-tag-mode)
;;
;; 3. Replace any letter betwen @ with a circle
;;
;;    (defun svg-tag-round (text)
;;      (svg-tag-make (substring text 1 -1) nil 1 1 12))
;;    (setq svg-tags
;;          '(("\\(=[0-9a-zA-Z- ]+?=\\)" 1
;;             `(face nil display ,(svg-tag-round (match-string 0))))))
;;    (svg-tag-mode)
;;
;;; Code:
(require 'svg)

(defvar svg-tags nil)
(defvar active-svg-tags nil)

(defgroup svg-tag nil
  "SVG tag mode"
  :group 'faces)

(defface svg-tag-default-face
  '((t :foreground "white"
       :background "orange"
       :box "orange"
       :family "Roboto Mono"
       :weight light
       :height 120))
  "Default face for tag"
  :group 'svg-tag-mode)

(defun svg-tag-make (text &optional face inner-padding outer-padding radius)
  (let* ((face       (or face 'svg-tag-default-face))
         (foreground (face-attribute face :foreground))
         (background (face-attribute face :background))
         (border     (face-attribute face :box))
         (family     (face-attribute face :family))
         (weight     (face-attribute face :weight))
         (size       (/ (face-attribute face :height) 10))

         (tag-char-width  (window-font-width nil face))
         (tag-char-height (window-font-height nil face))
         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         (inner-padding   (or inner-padding 1))
         (outer-padding   (or outer-padding 0))

         (text (string-trim text))
         (tag-width (* (+ (length text) inner-padding) txt-char-width))
         (tag-height (* txt-char-height 0.9))
         
         (svg-width (+ tag-width (* outer-padding txt-char-width)))
         (svg-height tag-height)

         (tag-x (/ (- svg-width tag-width) 2))
         (text-x (+ tag-x (/ (- tag-width (* (length text) tag-char-width)) 2)))
         (text-y (- tag-char-height (- txt-char-height tag-char-height)))
         
         (radius  (or radius 3))
         (svg (svg-create svg-width svg-height)))
         
    (svg-rectangle svg tag-x 0 tag-width tag-height
                   :fill        border
                   :rx          radius)
    (svg-rectangle svg (+ tag-x 0.5) 0.5 (- tag-width 1.0) (- tag-height 1.0)
                   :fill        background
                   :rx          (- radius 0.5))
    (svg-text      svg text 
                   :font-family family
                   :font-weight weight
                   :font-size   size
                   :fill        foreground
                   :x           text-x
                   :y           text-y)
    (svg-image svg :ascent 'center)))

(defun svg-tag-mode-on ()
  (add-to-list 'font-lock-extra-managed-props 'display)
  (if active-svg-tags
      (font-lock-remove-keywords nil active-svg-tags))
  (if svg-tags
      (font-lock-add-keywords nil svg-tags))
  (setq active-svg-tags (copy-sequence svg-tags))
  (message "Tag mode on"))

(defun svg-tag-mode-off ()  
  (if active-svg-tags
      (font-lock-remove-keywords nil active-svg-tags))
  (setq active-svg-tags nil)
  (message "Tag mode off"))

(define-minor-mode svg-tag-mode
  "Minor mode for graphical tag as rounded box."
  :group 'svg-tag-mode
  (if svg-tag-mode (svg-tag-mode-on) (svg-tag-mode-off))
  (font-lock-flush))

(provide 'svg-tag-mode)
