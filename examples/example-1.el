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
  '((t :foreground "black" :background "white" :box "black"))
  "Face for note tag" :group nil)

(defface svg-tag-keyboard-face
  '((t :foreground "#333333" :background "#f9f9f9" :box "#333333"))
  "Face for keyboard bindings tag" :group nil)

(defface svg-tag-org-face
  '((t :foreground "#333333" :background "#fffff0" :box "#333333"))
  "Face for keyboard bindings tag" :group nil)

(setq svg-tag-todo (svg-tag-make "TODO" 'svg-tag-org-face 1 1 6))

(setq svg-tag-note (svg-tag-make "NOTE" 'svg-tag-note-face 2 0 4))

(defun svg-tag-round (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 12))

(defun svg-tag-quasi-round (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 8))

(defun svg-tag-keyboard (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-keyboard-face 1 1 6))

(defun svg-tag-org (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-org-face 1 1 2))

(setq svg-tag-tags
      '(("@[0-9a-zA-Z]+:"                   . svg-tag-org)
        (":TODO:"                           . svg-tag-todo)
        (":NOTE:"                           . svg-tag-note)
        ("\([0-9a-zA-Z]\)"                  . svg-tag-round)
        ("\([0-9a-zA-Z][0-9a-zA-Z]\)"       . svg-tag-quasi-round)
        ("|[0-9a-zA-Z- ⇥></%⌘^→←↑↓]+?|"    . svg-tag-keyboard)))

(svg-tag-mode 1)

;; :NOTE: SVG tag is a minor mode that displays a rounded box with outer
;; and inner padding and a controllable box radius. The resulting SVG is
;; perfectly aligned with regular text such that a |TAG| can be inserted
;; and edited anywhere in the text.
;;
;; :TODO: Migrate to ELPA or MELPA
;;
;; More examples:
;; --------------
;;
;;  Save ................. |C-x||C-s|  Help ............... |C-h|
;;  Save as .............. |C-x||C-w|  Cancel ............. |C-g|
;;  Open a new file ...... |C-x||C-f|  Undo ............... |C-z|
;;  Open recent .......... |C-x||C-r|  Close buffer ....... |C-x||k|
;;  Browse directory ......|C-x||d|    Quit ............... |C-x||C-c|
;;
;; ------------------------------------------------------------------------
;; (1)(2)(3)(4)(5)(Z)(W)(12)(99)
;; ------------------------------------------------------------------------

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
