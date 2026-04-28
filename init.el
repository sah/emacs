;;; init.el -*- lexical-binding: t -*-

;;; ----------------------------------------------------------------
;;; Package management
;;; ----------------------------------------------------------------

(require 'cl-lib)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents))

;; Refresh package contents once the first time a package needs installing,
;; so new machines get up-to-date packages without a manual M-x package-refresh-contents.
(defvar my/refreshed nil)
(advice-add 'package-install :before
            (lambda (&rest _)
              (unless my/refreshed
                (package-refresh-contents)
                (setq my/refreshed t))))

(require 'use-package)

(setq use-package-always-ensure t)

;; Send Customize-saved values to a separate file so they don't rewrite init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; Load per-machine config (e.g., (setq my/local-packages '(copilot))).
(defvar my/local-packages nil)
(let ((local (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local) (load local)))

;; Canonical package list — authoritative; autoremove uses it on each startup.
(setq package-selected-packages
      (append '(ag apheleia arduino-mode browse-kill-ring cape catppuccin-theme
                   consult corfu default-text-scale dockerfile-mode doom-modeline
                   dumb-jump ef-themes editorconfig embark embark-consult
                   exec-path-from-shell flexoki-themes go-mode
                   iedit magit marginalia markdown-mode modus-themes orderless
                   projectile reformatter standard-themes treesit-auto vertico
                   web-mode wgrep yaml-mode)
              my/local-packages))

;; Restore GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))))

;; Remove packages no longer referenced in init.el after each startup.
(defun my/package-autoremove-quietly ()
  (if (null package-selected-packages)
      (message "Skipping package-autoremove: package-selected-packages empty (init error?)")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (package-autoremove))))
(add-hook 'emacs-startup-hook #'my/package-autoremove-quietly)

;; Inherit $PATH from login shell (essential on macOS GUI)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;; Don't prompt about following symlinks to vc files, it's fine.
(setq vc-follow-symlinks t)


;;; ----------------------------------------------------------------
;;; UI basics
;;; ----------------------------------------------------------------

(setq inhibit-startup-screen t)
(transient-mark-mode 1)
(blink-cursor-mode 0)
(global-font-lock-mode t)
(show-paren-mode t)
(global-display-line-numbers-mode 1)
(line-number-mode t)
(column-number-mode t)

(setq minibuffer-max-depth nil)

(defun my/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function #'my/flash-mode-line)

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (when (and (display-graphic-p)
                       (not (frame-focus-state)))
              (select-frame-set-input-focus (selected-frame)))))


(cl-defun my/clipboard-mode (&key terminal integrated read-clipboard-on-yank)
  "When INTEGRATED, every kill hits the macOS clipboard.
  With READ-CLIPBOARD-ON-YANK, yank also reads the clipboard; otherwise yank stays kill-ring-local.
  Without INTEGRATED, only cmd-C/X/V touch the clipboard.
  When TERMINAL, use pbcopy/pbpaste; otherwise use gui-set-selection/gui-get-selection."
  (setq select-enable-clipboard integrated)
  (if integrated
      ;; ---- Integrated: s-x/s-c/s-v bound to standard kill/yank commands ----
      (progn
        (if terminal
            ;; Terminal: wire the kill ring through pbcopy (and optionally pbpaste).
            (progn
              (setq interprogram-cut-function
                    (lambda (text)
                      ;; Pipe, not pty: pbcopy hangs on a pty after stdin EOF.
                      (let* ((process-connection-type nil)
                             (proc (start-process "pbcopy" nil "pbcopy")))
                        (process-send-string proc text)
                        (process-send-eof proc))))
              (setq interprogram-paste-function
                    (when read-clipboard-on-yank
                      (lambda () (shell-command-to-string "pbpaste")))))
          ;; GUI: select-enable-clipboard already routes kills via gui-select-text;
          ;; no extra setup needed here.
          )
        ;; Wire s-x/s-c/s-v to the kill ring; the integration above carries them to the clipboard.
        (global-set-key (kbd "s-x") #'kill-region)
        (global-set-key (kbd "s-c") #'kill-ring-save)
        (global-set-key (kbd "s-v") #'yank))
    ;; ---- Non-integrated: s-x/s-c/s-v bypass the kill ring and go straight to the clipboard ----
    (let ((cut (if terminal
                   ;; Terminal: pipe to pbcopy.
                   (lambda (text)
                     (let* ((process-connection-type nil)
                            (proc (start-process "pbcopy" nil "pbcopy")))
                       (process-send-string proc text)
                       (process-send-eof proc)))
                 ;; GUI: set the CLIPBOARD selection directly.
                 (lambda (text) (gui-set-selection 'CLIPBOARD text))))
          (paste (if terminal
                     (lambda () (shell-command-to-string "pbpaste"))
                   (lambda () (gui-get-selection 'CLIPBOARD)))))
      ;; Wire s-x/s-c/s-v directly to cut/paste, leaving the kill ring untouched.
      (global-set-key (kbd "s-x")
                      (lambda (b e) (interactive "r")
                        (funcall cut (buffer-substring b e))
                        (delete-region b e)))
      (global-set-key (kbd "s-c")
                      (lambda (b e) (interactive "r")
                        (funcall cut (buffer-substring b e))))
      (global-set-key (kbd "s-v")
                      (lambda () (interactive)
                        (insert (funcall paste)))))))

(when (display-graphic-p)
  ;; server-start is deferred to emacs-startup-hook so that emacsclient
  ;; can't connect until after init.el fully loads — otherwise it visits
  ;; files mid-init and the window shows scratch instead of the file.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (server-start)
              (toggle-frame-fullscreen)))
  (my/clipboard-mode :integrated t :read-clipboard-on-yank t))

(unless (display-graphic-p)
  (my/clipboard-mode :terminal t :integrated t :read-clipboard-on-yank t))

;;; ----------------------------------------------------------------
;;; Themes — follow macOS appearance
;;; ----------------------------------------------------------------

(use-package flexoki-themes :defer t)
(use-package modus-themes :defer t)
(use-package ef-themes :defer t)
(use-package standard-themes :defer t)
(use-package catppuccin-theme :defer t)

(defun my/system-appearance ()
  "Return \\='dark or \\='light based on macOS setting."
  (if (and (eq system-type 'darwin)
           (executable-find "defaults"))
      (if (string-match-p "Dark"
                          (shell-command-to-string
                           "defaults read -g AppleInterfaceStyle 2>/dev/null"))
          'dark 'light)
    'dark))

(defun my/apply-theme (appearance)
  "Set frame background mode and load the matching theme."
  (setq frame-background-mode appearance)
  (mapc #'frame-set-background-mode (frame-list))
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light  (progn
               (setq catppuccin-flavor 'latte)
               (load-theme 'modus-operandi-tinted t)
                                        ;(load-theme 'standard-light-tinted t)
                                        ;(load-theme 'flexoki-themes-light t) ; not a well made theme
               ))
    ('dark  (progn (setq catppuccin-flavor 'mocha) ; or frappe, or macchiato
                   (load-theme 'catppuccin t)
                                        ;(load-theme 'ef-dream t)
                                        ;(load-theme 'ef-elea-dark t)
                                        ;(load-theme 'ef-maris-dark t)
                                        ;(load-theme 'standard-dark-tinted t)
                                        ;(load-theme 'modus-vivendi-tinted t)
                                        ;(load-theme 'flexoki-themes-dark t) ; too dark, not a well made theme
                   ))
    )
  (dolist (face (face-list))
    (set-face-attribute face nil :box nil :overline nil)) ; forcibly remove things that cause line height changes
  (when (display-graphic-p)
    (set-background-color (face-background 'default))
    (set-foreground-color (face-foreground 'default))))

(my/apply-theme (my/system-appearance))
(when (boundp 'ns-system-appearance-change-functions)
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

(defun my/check-appearance-change ()
  "Re-apply theme if macOS appearance has changed since last check."
  (let ((current (my/system-appearance)))
    (unless (eq current frame-background-mode)
      (my/apply-theme current))))

(when (eq system-type 'darwin)
  (run-with-idle-timer 5 t #'my/check-appearance-change))

;;; ----------------------------------------------------------------
;;; Modeline, help
;;; ----------------------------------------------------------------

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 25))

(use-package which-key
  :ensure nil
  :config (which-key-mode))

;;; ----------------------------------------------------------------
;;; Minibuffer completion: vertico stack
;;; ----------------------------------------------------------------

(use-package vertico
  :init (vertico-mode)
  :bind (:map vertico-map
              ("<tab>"   . minibuffer-complete) ; complete common prefix, don't jump to a candidate
              ))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package marginalia :init (marginalia-mode))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("M-y"     . consult-yank-pop)
         ("M-s l"   . consult-line)
         ("C-c s"   . consult-ripgrep)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)))

(use-package embark-consult
  :after (embark consult)
  :demand t)

;;; ----------------------------------------------------------------
;;; In-buffer completion: corfu + cape
;;;   - manual trigger only (corfu-auto nil)
;;;   - TAB completes common prefix, never commits
;;;   - cape-dabbrev pulls words from buffer, including comments
;;; ----------------------------------------------------------------

(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  :bind (:map corfu-map
              ("TAB"   . corfu-complete)
              ("<tab>" . corfu-complete)
              ("RET"   . nil)))    ; RET inserts newline, not completion

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; M-/ triggers completion in any mode
(global-set-key (kbd "M-/") #'completion-at-point)

;;; ----------------------------------------------------------------
;;; LSP via eglot + flymake (both built-in)
;;; ----------------------------------------------------------------

;; Defer FN until idle, then run it in the original buffer if still alive.
(defun my/run-async-in-buffer (fn)
  (when buffer-file-name
    (let ((buf (current-buffer)))
      (run-with-idle-timer 0 nil
                           (lambda ()
                             (when (buffer-live-p buf)
                               (with-current-buffer buf (funcall fn))))))))

;; Don't block file open on the eglot handshake; eglot comes online once its server is ready.
(defun my/eglot-ensure-async ()
  (my/run-async-in-buffer
   (lambda ()
     (eglot-ensure)
     ;; eglot-ensure connects via post-command-hook, which timers don't trigger.
     (run-hooks 'post-command-hook))))

(use-package eglot
  :ensure nil
  :hook ((typescript-ts-mode . my/eglot-ensure-async)
         (tsx-ts-mode . my/eglot-ensure-async)
         (js-ts-mode . my/eglot-ensure-async)
         (python-ts-mode . my/eglot-ensure-async)
         (go-ts-mode . my/eglot-ensure-async)
         (ruby-ts-mode . my/eglot-ensure-async)
         (c-ts-mode . my/eglot-ensure-async)
         (c++-ts-mode . my/eglot-ensure-async))
  :custom
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider)))

;; use basedpyright-langserver to work around projects that pin an old pyright version
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio"))))

;; Tell pyright we don't support workspace/didChangeWatchedFiles. Otherwise it
;; registers watches that exhaust macOS file descriptors on a large source
;; tree and the server crashes. In-Emacs edits still flow via
;; textDocument/didChange|Save; pick up external changes with M-x eglot-reconnect.
(with-eval-after-load 'eglot
  (cl-defmethod eglot-client-capabilities :around (server)
    (let ((caps (cl-call-next-method)))
      (when (cl-intersection (eglot--major-modes server)
                             '(python-mode python-ts-mode))
        (setf (cl-getf (cl-getf caps :workspace) :didChangeWatchedFiles)
              '(:dynamicRegistration :json-false)))
      caps)))

(use-package flymake
  :ensure nil
  :custom (flymake-margin-indicator-position nil) ; avoid terminal margin indicator that indents at a weird time and eats a precious column
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;;; ----------------------------------------------------------------
;;; Tree-sitter
;;; ----------------------------------------------------------------

(use-package treesit-auto :demand t)

;; treesit-auto's global mode is too slow; we install grammars on demand instead.
;; Add new languages here. To find the grammar-lang names:
;; M-x describe-variable RET treesit-auto-recipe-list
(defvar my/ts-languages
  '(;; (regex ts-mode grammar-lang)
    ("\\.c\\'" c-ts-mode c)
    ("\\.\\(cc\\|cpp\\|hh\\|hpp\\)\\'" c++-ts-mode cpp)
    ("\\.py\\'" python-ts-mode python)
    ("\\.m?js\\'" js-ts-mode javascript)
    ("\\.jsx\\'" js-ts-mode javascript)
    ("\\.ts\\'" typescript-ts-mode typescript)
    ("\\.tsx\\'" tsx-ts-mode tsx)
    ("\\.go\\'" go-ts-mode go)
    ("\\.rb\\'" ruby-ts-mode ruby)
    ("\\.ya?ml\\'" yaml-ts-mode yaml)))

(defun my/ts-grammar-source (lang)
  "Look up LANG's grammar source in treesit-auto's recipe list."
  (when-let ((recipe (seq-find (lambda (r) (eq (treesit-auto-recipe-lang r) lang))
                               treesit-auto-recipe-list)))
    (list (treesit-auto-recipe-url recipe)
          (treesit-auto-recipe-revision recipe)
          (treesit-auto-recipe-source-dir recipe))))

(defun my/ts-mode-dispatch ()
  "Install grammar for the current file on demand, then activate its ts-mode."
  (pcase-let ((`(,_ ,mode ,lang)
               (seq-find (lambda (e) (string-match-p (car e) buffer-file-name))
                         my/ts-languages)))
    (unless (treesit-language-available-p lang)
      (setf (alist-get lang treesit-language-source-alist)
            (my/ts-grammar-source lang))
      (treesit-install-language-grammar lang))
    (funcall mode)))

(dolist (entry my/ts-languages)
  (add-to-list 'auto-mode-alist (cons (car entry) #'my/ts-mode-dispatch)))


;;; ----------------------------------------------------------------
;;; Magit, projectile, dumb-jump, ag, iedit, misc
;;; ----------------------------------------------------------------

(use-package magit :bind ("C-x g" . magit-status))

(use-package wgrep
  :custom (wgrep-auto-save-buffer t)
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)))

;; C-c e while in consult-ripgrep: export all matches to a grep buffer and
;; enter wgrep so they can be edited in place and written back to disk at once.
(defun my/consult-ripgrep-to-wgrep ()
  (interactive)
  (letrec ((hook (lambda ()
                   (let ((buf (current-buffer)))
                     (run-with-idle-timer 0 nil
                       (lambda ()
                         (with-current-buffer buf
                           (wgrep-change-to-wgrep-mode)))))
                   (remove-hook 'grep-mode-hook hook))))
    (add-hook 'grep-mode-hook hook))
  (embark-export))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-c e") #'my/consult-ripgrep-to-wgrep))

(use-package projectile :defer 1 :config (projectile-mode 1))
(use-package ag :defer t)
(use-package iedit)
(use-package browse-kill-ring)

(use-package default-text-scale
  :bind (("s-=" . default-text-scale-increase)
         ("s-+" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("s-_" . default-text-scale-decrease)))

(use-package dumb-jump
  :config
  (setq dumb-jump-force-searcher 'ag)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function
        #'xref-show-definitions-completing-read))

;; Skip copilot when its language server isn't installed; otherwise don't block
;; file open — copilot comes online once its agent is ready.
(defun my/copilot-async ()
  (when (file-directory-p (expand-file-name ".cache/copilot/bin"
                                            user-emacs-directory))
    (my/run-async-in-buffer (lambda () (copilot-mode 1)))))

;; Copilot is per-machine — opt in via my/local-packages in local.el.
(when (memq 'copilot package-selected-packages)
  (use-package copilot
    :hook (prog-mode . my/copilot-async)
    :bind (:map copilot-completion-map
                ("<tab>" . copilot-accept-completion)
                ("TAB"   . copilot-accept-completion)
                ("C-<tab>" . copilot-accept-completion-by-word)))

  (with-eval-after-load 'copilot
    (dolist (entry '((emacs-lisp-mode      . lisp-body-indent)
                     (c-ts-mode            . c-ts-mode-indent-offset)
                     (c++-ts-mode          . c-ts-mode-indent-offset)
                     (python-ts-mode       . python-indent-offset)
                     (js-ts-mode           . js-indent-level)
                     (typescript-ts-mode   . typescript-ts-mode-indent-offset)
                     (tsx-ts-mode          . typescript-ts-mode-indent-offset)
                     (go-ts-mode           . go-ts-mode-indent-offset)
                     (ruby-ts-mode         . ruby-indent-level)))
      (add-to-list 'copilot-indentation-alist (list (car entry) (cdr entry))))))

;; (use-package paredit
;;   :hook ((emacs-lisp-mode lisp-mode scheme-mode) . paredit-mode))

;;; ----------------------------------------------------------------
;;; File-type modes and formatters
;;; ----------------------------------------------------------------

(use-package web-mode
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.html?\\'"     . web-mode)))

(use-package apheleia
  :defer 1
  :config
  (apheleia-global-mode +1))

(use-package yaml-mode :mode "\\.ya?ml\\'")
(use-package dockerfile-mode :defer t)
(use-package arduino-mode :defer t)
(use-package markdown-mode :defer t)
;; Python: ruff via reformatter
(use-package reformatter
  :hook ((python-mode    . ruff-format-on-save-mode)
         (python-ts-mode . ruff-format-on-save-mode))
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-")))

;; Go: gofmt/goimports on save
(use-package go-mode
  :defer t
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook 'gofmt-before-save nil t)
                     (setq gofmt-command "goimports"))))

;;; ----------------------------------------------------------------
;;; Backups, autosaves (keep existing locations)
;;; ----------------------------------------------------------------

(setq autosave-dir "~/.emacs-autosaves/"
      backup-dir   "~/.emacs-backups/")
(make-directory autosave-dir t)
(make-directory backup-dir   t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name (concat "#%" (buffer-name) "#")))))

(setq backup-directory-alist (list (cons "." backup-dir))
      make-backup-files      t
      backup-by-copying      t
      delete-old-versions    t
      kept-old-versions      2
      kept-new-versions      10
      version-control        t)

;;; ----------------------------------------------------------------
;;; Indentation: editorconfig first, these defaults as fallback
;;; ----------------------------------------------------------------

(use-package editorconfig :config (editorconfig-mode 1))

(defun infer-indentation-style ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count   (how-many "^\t" (point-min) (point-max))))
    (when (> space-count tab-count) (setq indent-tabs-mode nil))
    (when (> tab-count space-count) (setq indent-tabs-mode t))))

(defun set-tabs (c-like-p)
  (when c-like-p
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (c-set-style "linux"))
  (setq c-basic-indent       4
        c-basic-offset       4
        ruby-indent-level    4
        typescript-indent-level 2)
  (modify-syntax-entry ?_ "_")
  (setq tab-width        8
        indent-tabs-mode nil)
  (infer-indentation-style)
  (c-set-offset 'case-label '+)
  (local-set-key "\C-m" 'newline-and-indent))

(defun jwz-untabify ()
  (unless indent-tabs-mode
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (when (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
    nil))

;; Strip trailing whitespace on save in code-ish modes.
(dolist (hook '(prog-mode-hook conf-mode-hook sgml-mode-hook nxml-mode-hook))
  (add-hook hook (lambda ()
                   (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))

(dolist (mode '(java-mode-hook c-mode-hook c++-mode-hook
                               cperl-mode-hook php-mode-hook))
  (add-hook mode (lambda ()
                   (add-hook 'before-save-hook 'jwz-untabify nil t)
                   (set-tabs t))))

(dolist (mode '(emacs-lisp-mode-hook lisp-mode-hook html-mode-hook
                                     python-mode-hook     python-ts-mode-hook
                                     ruby-mode-hook       ruby-ts-mode-hook
                                     haskell-mode-hook    scala-mode-hook
                                     js-ts-mode-hook      typescript-ts-mode-hook tsx-ts-mode-hook
                                     web-mode-hook))
  (add-hook mode (lambda ()
                   (add-hook 'before-save-hook 'jwz-untabify nil t)
                   (set-tabs nil))))

;;; ----------------------------------------------------------------
;;; Key remaps
;;; ----------------------------------------------------------------

(global-unset-key "\M-g") (global-set-key "\M-g" 'goto-line)
(global-unset-key "\C-h") (global-set-key "\C-h" 'backward-delete-char)

(when (display-graphic-p)
  (global-unset-key "\C-z")
  (global-set-key "\C-z"      'undo)
  (global-set-key (kbd "s-z") 'undo))

(define-key global-map [home]   'beginning-of-line)
(define-key global-map [end]    'end-of-line)
(define-key global-map [find]   'beginning-of-line)
(define-key global-map [select] 'end-of-line)

;;; ----------------------------------------------------------------
;;; Terminal mouse
;;; ----------------------------------------------------------------

(unless (display-graphic-p)
  (require 'mouse)
  (xterm-mouse-mode t)
  (setq mouse-sel-mode t))

(provide 'init)
;;; init.el ends here
