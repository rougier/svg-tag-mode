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

(defface svg-tag-org-face
  '((t :foreground "#333333" :background "white"
       :box (:line-width 1 :color "black" :style nil)
       :family "Roboto Mono" :weight regular :height 120))
  "Default face for svg tag" :group nil)

(defface svg-tag-note-face
  '((t :foreground "#333333" :background "#FFFFFF"
       :box (:line-width 1 :color "#333333" :style nil)
       :family "Roboto Mono" :weight regular :height 120))
  "Default face for svg tag" :group nil)

(defface svg-tag-todo-face
  '((t :foreground "#ffffff" :background "#FFAB91"
       :box (:line-width 1 :color "#FFAB91" :style nil)
       :family "Roboto Mono" :weight regular :height 120))
  "Face for TODO  svg tag" :group nil)

(defface svg-tag-next-face
  '((t :foreground "white" :background "#673AB7"
       :box (:line-width 1 :color "#673AB7" :style nil)
       :family "Roboto Mono" :weight regular :height 120))
  "Face for NEXT svg tag" :group nil)

(defface svg-tag-done-face
  '((t :foreground "white" :background "#B0BEC5"
       :box (:line-width 1 :color "#B0BEC5" :style nil)
       :family "Roboto Mono" :weight regular :height 120))
  "Face for DONE  svg tag" :group nil)

(defface svg-tag-org-tag-face
  '((t :foreground "#ffffff" :background "#FFAB91"
       :box (:line-width 1 :color "#FFAB91" :style nil)
       :family "Roboto Mono" :weight regular :height 120))
  "Face for TODO  svg tag" :group nil)

(defface svg-tag-date-active-face
  '((t :foreground "white" :background "#673AB7"
       :box (:line-width 1 :color "#673AB7" :style nil)
       :family "Roboto Mono" :weight regular :height 120))
  "Face for active date svg tag" :group nil)

(defface svg-tag-time-active-face
  '((t :foreground "#673AB7" :background "#ffffff"
       :box (:line-width 1 :color "#673AB7" :style nil)
       :family "Roboto Mono" :weight light :height 120))
  "Face for active time svg tag" :group nil)

(defface svg-tag-date-inactive-face
  '((t :foreground "#ffffff" :background "#B0BEC5"
       :box (:line-width 1 :color "#B0BEC5" :style nil)
       :family "Roboto Mono" :weight regular :height 120))
  "Face for inactive date svg tag" :group nil)

(defface svg-tag-time-inactive-face
  '((t :foreground "#B0BEC5" :background "#ffffff"
       :box (:line-width 2 :color "#B0BEC5" :style nil)
       :family "Roboto Mono" :weight light :height 120))
  "Face for inactive time svg tag" :group nil)

(setq svg-tag-org-todo (svg-tag-make "TODO" 'svg-tag-todo-face 1 1 2))
(setq svg-tag-org-done (svg-tag-make "DONE" 'svg-tag-done-face 1 1 2))
(setq svg-tag-org-hold (svg-tag-make "HOLD" 'svg-tag-org-face 1 1 2))
(setq svg-tag-org-next (svg-tag-make "NEXT" 'svg-tag-next-face 1 1 2))
(setq svg-tag-org-note-tag (svg-tag-make "NOTE" 'svg-tag-note-face 1 1 2))
(setq svg-tag-org-canceled-tag (svg-tag-make "CANCELED" 'svg-tag-note-face 1 1 2))

(defun svg-tag-make-org-tag (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-org-tag-face 1 1 2))
(defun svg-tag-make-org-priority (text)
  (svg-tag-make (substring text 2 -1) 'svg-tag-org-face 1 0 2))

(defun svg-tag-make-org-date-active (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-date-active-face 0 0 0))
(defun svg-tag-make-org-time-active (text)
  (svg-tag-make (substring text 0 -1) 'svg-tag-time-active-face 1 0 0))
(defun svg-tag-make-org-range-active (text)
  (svg-tag-make (substring text 0 -1) 'svg-tag-time-active-face 0 0 0))

(defun svg-tag-make-org-date-inactive (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-date-inactive-face 0 0 0))
(defun svg-tag-make-org-time-inactive (text)
  (svg-tag-make (substring text 0 -1) 'svg-tag-time-inactive-face 1 0 0))
(defun svg-tag-make-org-range-inactive (text)
  (svg-tag-make (substring text 0 -1) 'svg-tag-time-inactive-face 0 0 0))


(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")

(setq svg-tag-tags
      `(("@[0-9a-zA-Z]+:"                   . svg-tag-make-org-tag)
        ("@NOTE:"                           . svg-tag-org-note-tag)
        ("@CANCELED:"                       . svg-tag-org-canceled-tag)
        ("\\[#[ABC]\\]"                     . svg-tag-make-org-priority)
        (" TODO "                           . svg-tag-org-todo)
        (" DONE "                           . svg-tag-org-done)
        (" NEXT "                           . svg-tag-org-next)
        (" HOLD "                           . svg-tag-org-hold)

        (,(concat "<" date-re  "[ >]")             . svg-tag-make-org-date-active)    
        (,(concat "<" date-re " " day-re "[ >]")   . svg-tag-make-org-date-active)    
        (,(concat time-re ">")                     . svg-tag-make-org-time-active)
        (,(concat time-re "-" time-re ">")         . svg-tag-make-org-range-active)

        (,(concat "\\[" date-re  "[] ]")           . svg-tag-make-org-date-inactive)    
        (,(concat "\\[" date-re " " day-re "[] ]") . svg-tag-make-org-date-inactive)    
        (,(concat time-re "\\]")                   . svg-tag-make-org-time-inactive)
        (,(concat time-re "-" time-re "\\]")       . svg-tag-make-org-range-inactive)))

(svg-tag-mode)

;; To do:       TODO  NEXT  HOLD  DONE  
;; Tags:       @MEETING:@NOTE:
;; Priorities:  [#A] [#B] [#C]
;; Active date:   <2020-11-07>
;;                <2020-11-07 Sat>
;;                <2020-11-07 Sat 14:00>
;;                <2020-11-07 Sat 14:00-15:00>

;; Inactive date: [2020-11-07]
;;                [2020-11-07 Sat]
;;                [2020-11-07 Sat 14:00]
;;                [2020-11-07 Sat 14:00-15:00]
