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

(defface svg-tag-note-face
  '((t :foreground "black" :background "white" :box "black"
       :family "Roboto Mono" :weight light :height 120))
  "Face for note tag" :group nil)
(defface svg-tag-keyboard-face
  '((t :foreground "#333333" :background "#f9f9f9" :box "#333333"
       :family "Roboto Mono" :weight light :height 120))
  "Face for keyboard bindings tag" :group nil)

(setq  svg-tag-todo (svg-tag-make "TODO" nil 1 1 2))
(setq  svg-tag-note (svg-tag-make "NOTE" 'svg-tag-note-face 1 1 2))
(defun svg-tag-round (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 12))
(defun svg-tag-quasi-round (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 8))
(defun svg-tag-keyboard (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-keyboard-face 1 1 2))

(setq svg-tags
      '(("\\(:TODO:\\)" 1 `(face nil display ,svg-tag-todo))
        ("\\(:NOTE:\\)" 1 `(face nil display ,svg-tag-note))
        ("\\(\@[0-9a-zA-Z]\@\\)" 1
         `(face nil display ,(svg-tag-round (match-string 0))))
        ("\\(\@[0-9a-zA-Z][0-9a-zA-Z]\@\\)" 1
         `(face nil display ,(svg-tag-quasi-round (match-string 0))))
        ("\\(=[0-9a-zA-Z- ]+?=\\)" 1
         `(face nil display ,(svg-tag-keyboard (match-string 0))))))

(svg-tag-mode 1)

;; A tag function using SVG to display a rounded box with outer and inner
;; padding and a controllable box radius. The resulting SVG is perfectly
;; aligned with regular text such that a =TAG= can be inserted and edited
;; anywhere in the text thanks to font-lock and the display property.

;;|:TODO:| Make a minor mode
;;|:NOTE:| Don't know how to do it, help needed…
;;|______| Perfect alignment with regular text
;;
;;  Save ................. =C-x=+=C-s=  Help ............... =C-h=
;;  Save as .............. =C-x=+=C-w=  Cancel ............. =C-g=
;;  Open a new file ...... =C-x=+=C-f=  Undo ............... =C-z=
;;  Open recent .......... =C-x=+=C-r=  Close buffer ....... =C-x=+=k=
;;  Browse directory ......=C-x=+=d=    Quit ............... =C-x=+=C-c=

;; ------------------------------------------------------------------------
;; :NOTE: Sections can be folded or unfolded. If you think a section has
;;        disappeared, it's probably because it is folded. To unfold it,
;;        place the cursor on the section title and press the =tab= key.
;; ------------------------------------------------------------------------
;; @1@@2@@3@@4@@5@@Z@@W@@12@@99@
;; ------------------------------------------------------------------------

