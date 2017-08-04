
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/emacs/emacs-color-theme-solarized")
(add-to-list 'load-path "~/emacs/egg")
(add-to-list 'load-path "~/emacs/emacs-powerline")
(add-to-list 'load-path "~/emacs/haskell-mode")
(add-to-list 'load-path "~/emacs/indent-guide")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq custom-theme-load-path '("~/emacs/emacs-color-theme-solarized"))

;; jesus christ
(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)

(setq inhibit-startup-screen t) ;; the startup screen is so ugly
(transient-mark-mode 1) ;; works like zmacs-regions.  1 means turn it on.
(blink-cursor-mode 0) ;; 0 means turn. it. the fuck. off.
(menu-bar-mode 0) ;; ugly and useless
(condition-case () (tool-bar-mode 0) (error nil))  ;; ugly and useless
(condition-case () (set-scroll-bar-mode 'right) (error nil))

(global-font-lock-mode t)
(show-paren-mode t)

(setq autosave-dir "~/.emacs-autosaves/")
(setq backup-dir "~/.emacs-backups/")
(make-directory autosave-dir t)
(make-directory backup-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name (concat "#%" (buffer-name) "#")))))

(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq version-control t)

(if window-system (setq visible-bell t))

(if window-system
    ;; default colors if we're not in a terminal
     (progn
       (server-start)
       (enable-theme 'solarized-dark)
       ;;(set-face-background 'default "#dfdbc3")
       ;;(set-face-foreground 'default "#3b2322")
       (add-to-list 'default-frame-alist '(width . 84))
       (add-to-list 'default-frame-alist '(height . 60))
       (add-to-list 'default-frame-alist '(cursor-color . "#dc322f")))
  ;; default colors in a terminal
  (progn
    ;;(enable-theme 'solarized-dark)
    (set-face-background 'isearch "yellow")
    (set-face-foreground 'isearch "black")
    (set-face-background 'isearch-lazy-highlight-face "gray")
    (set-face-foreground 'isearch-lazy-highlight-face "black")
    (set-face-background 'region "lightblue")
    (set-face-foreground 'region "black")
    (set-face-background 'show-paren-match-face nil)
    (set-face-foreground 'show-paren-match-face nil)
    (set-face-bold 'show-paren-match-face t)
    (set-face-underline 'show-paren-match-face t)
    (set-face-background 'show-paren-mismatch-face "red")
    (set-face-foreground 'show-paren-mismatch-face "black")
    (set-face-bold 'show-paren-mismatch-face t)
    (set-face-underline 'show-paren-mismatch-face t)
    ))

;; for line numbers
(require 'linum)
(if window-system (global-linum-mode 1))

(ivy-mode)

(global-flycheck-mode)

;; mako mode stuff
(condition-case ()
    (progn
      (load "mmm-mako.el")
      (add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
      (mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako))
  (error nil))

;; js mode stuff
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; php mode stuff
(autoload 'php-mode "php-mode" "PHP editing mode." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))


;; ruby mode stuff
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;(autoload 'run-ruby "inf-ruby"
;  "Run an inferior Ruby process")
;(autoload 'inf-ruby-keys "inf-ruby"
;  "Set local key defs for inf-ruby in ruby-mode")
;(add-hook 'ruby-mode-hook
;          '(lambda ()
;             (inf-ruby-keys)
;             ))

;; haskell mode stuff
(autoload 'haskell-mode "haskell-mode" "Haskell editing mode." t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(condition-case ()
    (load "~/emacs-lisp/haskell-mode/haskell-site-file")
  (error nil))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(setq haskell-program-name "/usr/bin/ghci")

;; scala mode stuff
(require 'scala-mode-auto)

;; go mode stuff
(defun my-go-mode-hook ()
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  ;; autocomplete
  (auto-complete-mode 1)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-auto-start nil)
  (global-set-key "\M-/" 'auto-complete)
)

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key "\M-/" 'company-complete)

;; yaml-mode stuff
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;; Emacs/W3 Configuration
(condition-case () (require 'w3-auto "w3-auto") (error nil))

;; svn mode stuff
(condition-case ()
    (progn
      (require 'psvn)
      (set-face-foreground 'svn-status-filename-face "antique white")
      (set-face-foreground 'svn-status-directory-face "light sky blue")
      (set-face-foreground 'svn-status-marked-face "yellow")
      (set-face-foreground 'svn-status-modified-external-face "coral"))
  (error nil))

;; where is lisp?
(setq inferior-lisp-program "/usr/bin/clisp")


;; syntax highlighting
;;(setq font-lock-maximum-decoration t)
;(setq-default font-lock-auto-fontify t)
;(setq-default font-lock-use-fonts nil)
;(setq-default font-lock-maximum-decoration t)
;(setq-default font-lock-maximum-size 256000)
;;(setq-default font-lock-mode-enable-list nil)
;;(setq-default font-lock-mode-disable-list nil)
;;(remove-hook 'font-lock-mode-hook 'turn-on-fast-lock)
;;(remove-hook 'font-lock-mode-hook 'turn-on-lazy-shot)

(defun set-tabs (c-like-p stupid-p)

  (if c-like-p
      (progn
        ;; Fucking RMSmacs (by version 21.3, but after 21.1) doesn't
        ;; fucking make these fucking variables local where it fucking
        ;; should, so the c-set-style we're about to do below breaks
        ;; fucking fill-paragraph for fucking all non-c-like modes by
        ;; fucking default.
        (make-local-variable 'paragraph-start)
        (make-local-variable 'paragraph-separate)

        (c-set-style "linux")))

  (setq c-basic-indent 4) ;; xemacs
  (setq c-basic-offset 4) ;; emacs
  (setq ruby-indent-level 2) ;; ruby-mode is stupid

  (modify-syntax-entry ?_ "_") ;; don't word-move over underscores

  ;; tab stuff as recommended by jwz
  ;; http://www.jwz.org/doc/tabs-vs-spaces.html
  (if stupid-p
      (progn
        (setq tab-width 4)
        (setq indent-tabs-mode t))
    (progn
        (setq tab-width 8)
        (setq indent-tabs-mode nil)))

  ;; make case labels indent inside switch statements
  (c-set-offset 'case-label '+)

  ;; make the ENTER key indent next line properly
  (local-set-key "\C-m" 'newline-and-indent)

  ;; code colors
  (let ((theme-yellow "#b58900")
        (theme-orange "#cb4b16")
        (theme-red "#dc322f")
        (theme-magenta "#d33682")
        (theme-violet "#6c71c4")
        (theme-blue "#268bd2")
        (theme-cyan "#2aa198")
        (theme-green "#859900"))
    (set-face-foreground 'highlight-indent-guides-character-face "black")
    (if window-system
        (progn
          (set-face-foreground 'font-lock-string-face theme-yellow)
          (set-face-foreground 'font-lock-keyword-face theme-violet)
          (set-face-foreground 'font-lock-type-face theme-cyan)
          )
      (progn
        (set-face-foreground 'font-lock-comment-face "#586e75")
        (set-face-foreground 'font-lock-string-face theme-yellow)
        (set-face-foreground 'font-lock-keyword-face theme-violet)
        (set-face-foreground 'font-lock-type-face theme-cyan)
        (set-face-foreground 'font-lock-function-name-face theme-blue)
        (set-face-foreground 'font-lock-variable-name-face theme-blue)
        (set-face-foreground 'font-lock-constant-face theme-yellow)
        ))
    )
)

(defun jwz-untabify ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)

; "c-like" modes
(dolist (mode '(
                java-mode-hook
                cperl-mode-hook
                c-mode-hook
                c++-mode-hook
                php-mode-hook
                )
              nil)
  (add-hook mode
            '(lambda ()
               (make-local-variable 'write-contents-hooks)
               (add-hook 'write-contents-hooks 'jwz-untabify)
               (set-tabs t nil)
               )))

(dolist (mode '(
                )
              nil)
  (add-hook mode
            '(lambda ()
               (make-local-variable 'write-contents-hooks)
               ;(add-hook 'write-contents-hooks 'jwz-untabify)
               (set-tabs t t)
               )))

; non-"c-like" modes
(dolist (mode '(
                html-mode-hook
                lisp-mode-hook
                emacs-lisp-mode-hook
                haskell-mode-hook
                haskell-c-mode-hook
                ruby-mode-hook
                python-mode-hook
                scala-mode-hook
                js2-mode-hook
                )
              nil)
  (add-hook mode
            '(lambda ()
               (make-local-variable 'write-contents-hooks)
               (add-hook 'write-contents-hooks 'jwz-untabify)
               (set-tabs nil nil)
               )))

(setq minibuffer-max-depth nil)

(line-number-mode t)
(column-number-mode t)


;; "M-x compile" sucks for projects that don't have a makefile in
;; every directory.  So, if Makefile doesn't exist in the current
;; default directory, use make-mf to run compile in the default
;; directory of buffer with a magic name (say, "Makefile").  If that
;; buffer doesn't exist, use make-cwd to default to the directory from
;; which emacs was run.
;; (defun make-mf ()
;;   (interactive)
;;   (with-current-buffer magic-make-buffer-name
;;     (compile "make")))

;; (defun make-cwd ()
;;   (interactive)
;;   (let ((curdir default-directory))
;;     (unwind-protect
;;         ((progn
;;            (cd-absolute command-line-default-directory)
;;            (compile "make")))
;;       (cd-absolute curdir))))

;; (defun make ()
;;   (interactive)
;;   (if (or (file-exists-p "Makefile")
;;           (file-exists-p "makefile")
;;           (file-exists-p "GNUmakefile"))
;;       (compile "make")
;;     (if (eq nil (get-buffer magic-make-buffer-name))
;;         (make-cwd)
;;       (make-mf))))

;; (defun make-clean ()
;;   (interactive)
;;   (with-current-buffer magic-make-buffer-name
;;     (compile "make clean")))

;; (setq magic-make-buffer-name "Makefile")

;; (define-key global-map [(control return)] 'make)

; make home and end do what they do on windows
(define-key global-map [find] `beginning-of-line)
(define-key global-map [select] `end-of-line)
(define-key global-map [home] `beginning-of-line)
(define-key global-map [end] `end-of-line)

; I never want help
(global-unset-key "\C-h")
(global-set-key "\C-h" 'backward-delete-char)

; I never want to lose my windows
(condition-case ()
    (if window-system
        (progn
          (global-unset-key "\C-z")
          (global-set-key "\C-z" 'undo)))
  (error nil))

; imenu is useful
(global-set-key "\M-s" 'imenu)

; for cvs mode
(setenv "CVS_RSH" "ssh")

; for git
(require 'dominating-file)
(require 'egg)

;(require 'indent-guide)
;(indent-guide-global-mode)
(setq highlight-indent-guides-auto-enabled nil)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;;(setq sql-mysql-program "/usr/local/mysql/bin/mysql")

(defun mechanics ()
  (interactive)
  (run-scheme
   "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib"
   ))

; codepad stuff!
(autoload 'codepad-paste-region "codepad" "Paste region to codepad.org." t)
(autoload 'codepad-paste-buffer "codepad" "Paste buffer to codepad.org." t)
(autoload 'codepad-fetch-code "codepad" "Fetch code from codepad.org." t)

; powerline!
(if window-system (require 'powerline))

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)


(if (eq system-type 'darwin)
  (progn
    (defun copy-from-osx ()
      (shell-command-to-string "pbpaste"))

    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)
  )
)

; this is slow, use only when needed
;(load-file "~/emacs/graphviz-dot-mode.el")

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(blink-cursor-mode nil)
;;  '(column-number-mode t)
;;  '(load-home-init-file t t)
;;  '(show-paren-mode t)
;;  '(transient-mark-mode t)
;;  '(paren-mode (quote paren) nil (paren))
;;  '(toolbar-visible-p nil)
;;  '(font-lock-mode nil nil (font-lock)))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "apple" :family "Menlo")))))

(condition-case () (require 'local) (error nil))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-allowed-file-name-masks
   (quote
    (("\\.py\\'" flymake-pyflakes-init nil nil)
     ("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init nil nil)
     ("\\.cs\\'" flymake-simple-make-init nil nil)
     ("\\.p[ml]\\'" flymake-perl-init nil nil)
     ("\\.php[345]?\\'" flymake-php-init nil nil)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup nil)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup nil)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup nil)
     ("\\.tex\\'" flymake-simple-tex-init nil nil)
     ("\\.idl\\'" flymake-simple-make-init nil nil))))
 '(package-selected-packages
   (quote
    (ivy projectile flycheck highlight-indent-guides company relax go-mode go-autocomplete gist exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
