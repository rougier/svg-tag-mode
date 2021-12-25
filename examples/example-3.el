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

;; This replaces any occurence of ":TODO:" with a static SVG tag
;; displaying "TODO"
(setq svg-tag-tags
      '((":TODO:" . ((svg-tag-make "TODO" :margin 0)))))


;; This replaces any occurence of ":HELLO:" with a static SVG tag that
;; can be clicked to execute the specified command. Help message is
;; displayed when the tag is hovered with the pointer.
(setq svg-tag-tags
      '((":HELLO:" .  ((svg-tag-make "HELLO")
                       (lambda () (interactive) (message "Hello world!"))
                       "Print a greeting message"))))

;; This replaces any occurence of ":TODO:" with a dynamic SVG tag
;; displaying ":TODO:"
(setq svg-tag-tags
      '((":TODO:" . (svg-tag-make))))

;; This replaces any occurence of ":TODO:" with a dynamic SVG tag
;; displaying "TODO"
(setq svg-tag-tags
      '((":TODO:" . ((lambda (tag)
                       (svg-tag-make tag :beg 1 :end -1))))))

;; This replaces any occurence of ":XXX:" with a dynamic SVG tag
;; displaying "XXX"
(setq svg-tag-tags
      '(("\\(:[A-Z]+:\\)" . ((lambda (tag)
                               (svg-tag-make tag :beg 1 :end -1))))))

;; This replaces any occurence of ":XXX|YYY:" with two adjacent
;; dynamic SVG tags displaying "XXX" and "YYY"
(setq svg-tag-tags
      '(("\\(:[A-Z]+\\)\|[a-zA-Z#0-9]+:" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :inverse t
                                                          :margin 0 :crop-right t))))
        (":[A-Z]+\\(\|[a-zA-Z#0-9]+:\\)" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :end -1
                                                         :margin 0 :crop-left t))))))

;; This replaces any occurence of ":#TAG1:#TAG2:…:$" ($ means end of
;; line) with a dynamic collection of SVG tags. Note the # symbol in
;; front of tags. This is mandatory because Emacs cannot do regex look
;; ahead.
(setq svg-tag-tags
      '(("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                     (svg-tag-make tag :beg 2))))
        ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2 :end -1))))))




