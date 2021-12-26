;; Copyright (C) 2020, 2021  Nicolas P. Rougier
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

(setq svg-tag-tags
      '((":TODO:" . ((svg-tag-make "TODO" :face 'org-tag
                                   :radius 0 :inverse t :margin 0)))
        (":NOTE:" . ((svg-tag-make "NOTE" :face 'font-lock-comment-face
                                   :inverse nil :margin 0 :radius 0)))
        ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                                (svg-tag-make tag :beg 1 :end -1 :radius 12))))
        ("\([0-9a-zA-Z][0-9a-zA-Z]\)" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :end -1 :radius 8))))
        ("|[0-9a-zA-Z- ]+?|" . ((lambda (tag)
                                  (svg-tag-make tag :face 'font-lock-comment-face
                                                :margin 0 :beg 1 :end -1))))))
(svg-tag-mode t)

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
;;  Save ................. |C-x| |C-s|  Help ............... |C-h|
;;  Save as .............. |C-x| |C-w|  Cancel ............. |C-g|
;;  Open a new file ...... |C-x| |C-f|  Undo ............... |C-z|
;;  Open recent .......... |C-x| |C-r|  Close buffer ....... |C-x| |k|
;;  Browse directory ..... |C-x| |d|    Quit ............... |C-x| |C-c|
;;
;; ------------------------------------------------------------------------
;; (1)(2)(3)(4)(5)(Z)(W)(12)(99) (A)(B)(C)
;; ------------------------------------------------------------------------

