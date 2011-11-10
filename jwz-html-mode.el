;;; jwz-html-mode.el --- bare-bones HTML mode

;; Keywords: langauges

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; The other HTML modes I've seen are a pain in the ass because they're
;; needlessly complicated.  I don't want commands to insert matched tags
;; for me, and I don't want to be surprised by goofy overridden keybindings.
;; I just want: text mode, plus auto-fill, plus some minor fontification
;; of tags.  That's all.

(defvar jwz-html-mode-syntax-table
  (let ((i 0)
	(table (make-syntax-table)))
    (modify-syntax-entry ?\" "."     table)
    (modify-syntax-entry ?\\ "."     table)
    (modify-syntax-entry ?'  "w"     table)

    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?\; "."     table)

    (modify-syntax-entry ?&  "'"     table)

    (modify-syntax-entry ?\" "\""    table)

    (modify-syntax-entry ?<  ". 1"   table)
    (modify-syntax-entry ?!  ". 2"   table)
    (modify-syntax-entry ?-  ". 3"   table)
    (modify-syntax-entry ?>  ". 4"   table)
    table))

(defvar jwz-html-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'jwz-html-mode-map)
    (define-key map "\t" 'tab-to-tab-stop)
    map))

(defvar jwz-html-font-lock-keywords
  (list
   ;; tags
   '("<[^!>]+>" 0 font-lock-function-name-face t)

   ;; character entities
   '("&[^ \t\n;]+;" . font-lock-keyword-face)

   ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
   '("\\(<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>\\)"
     1 font-lock-comment-face t)

   ;; Now highlight strings again, sigh...
   '("\"[^\"]*\"" 0 font-lock-string-face t)

   ))


(defun jwz-html-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map jwz-html-mode-map)
  (setq mode-name "HTML")
  (setq major-mode 'jwz-html-mode)
  (set-syntax-table jwz-html-mode-syntax-table)
  (auto-fill-mode)

  (setq paragraph-separate "[ \t\f]*$"
	paragraph-start "[\n\f]"
	)

  (run-hooks 'jwz-html-mode-hook))

(fset 'html-mode 'jwz-html-mode)

(provide 'jwz-html-mode)
