;; Copyright (C) 2020  Nicolas P. Rougier
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
(require 'svg-tag-mode)

(defface svg-tag-todo-face
  '((t :foreground "#ffffff" :background "#FFAB91" :box "#FFAB91"
       :family "Roboto Mono" :weight light :height 120))
  "Face for TODO  svg tag" :group nil)

(defface svg-tag-next-face
  '((t :foreground "white" :background "#673AB7" :box "#673AB7"
       :family "Roboto Mono" :weight light :height 120))
  "Face for NEXT svg tag" :group nil)

(defface svg-tag-done-face
  '((t :foreground "white" :background "#B0BEC5" :box "#B0BEC5"
       :family "Roboto Mono" :weight light :height 120))
  "Face for DONE  svg tag" :group nil)

(defface svg-tag-date-face
  '((t :foreground "black" :background "#ECEFF1" :box "#ECEFF1"
       :family "Roboto Mono" :weight light :height 120))
  "Face for date svg tag" :group nil)

(defface svg-tag-org-face
  '((t :foreground "black" :background "white" :box "black"
       :family "Roboto Mono" :weight light :height 120))
  "Default face for svg tag" :group nil)

(setq radius 2)

(setq svg-tag-org-todo (svg-tag-make "TODO" 'svg-tag-todo-face 1 1 radius))
(setq svg-tag-org-done (svg-tag-make "DONE" 'svg-tag-done-face 1 1 radius))
(setq svg-tag-org-hold (svg-tag-make "HOLD" 'svg-tag-done-face 1 1 radius))
(setq svg-tag-org-next (svg-tag-make "NEXT" 'svg-tag-next-face 1 1 radius))
(setq svg-tag-org-note (svg-tag-make "NOTE" 'svg-tag-org-face  1 1 radius))

(defun svg-tag-make-org-todo (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-done-face 1 1 radius))
(defun svg-tag-make-org-tag (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-org-face 1 1 radius))
(defun svg-tag-make-org-priority (text)
  (svg-tag-make (substring text 2 -1) 'svg-tag-org-face 1 0 radius))
(defun svg-tag-make-org-date (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-date-face 0 0 radius))

(setq svg-tag-tags
      '(("@[0-9a-zA-Z]+:"                   . svg-tag-make-org-tag)
        ("\\[#[ABC]\\]"                     . svg-tag-make-org-priority)
        (" TODO "                           . svg-tag-org-todo)
        (" DONE "                           . svg-tag-org-done)
        (" NEXT "                           . svg-tag-org-next)
        (" HOLD "                           . svg-tag-org-hold)
        (" NOTE "                           . svg-tag-org-note)
        ("<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\}>"
                                            . svg-tag-make-org-date)))

(svg-tag-mode 1)

;; To do:      TODO  NEXT  HOLD  DONE  
;; Tags:       :@MEETING:@NOTE:
;; Priorities: [#A] [#B] [#C]
;; Date:       <2020-11-07 Sat>

;;  DONE Make a pull request on melpa                             <2020-11-07 Sat>
;;  NEXT Wait for review
;;  TODO Post on Reddit




