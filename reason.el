(defun init-reason-merlin ()
  (progn
    ;; Load a single, canonical copy of merlin-mode
    (add-to-list 'load-path (expand-file-name "~/spacemacs/merlin/emacs"))

    (require 'reason-mode)
    (require 'merlin)
    (add-hook 'reason-mode-hook (lambda ()
                                  (add-hook 'before-save-hook 'refmt-before-save)
                                  (merlin-mode)))
    (setq merlin-ac-setup t)
    (require 'merlin-iedit)
    (defun evil-custom-merlin-iedit ()
      (interactive)
      (if iedit-mode (iedit-mode)
        (merlin-iedit-occurrences)))
    (define-key merlin-mode-map (kbd "C-c C-e") 'evil-custom-merlin-iedit)
    (define-key merlin-mode-map (kbd "C-c M-d") 'merlin-document)))

(defun init-reason ()
  ;;----------------------------------------------------------------------------
  ;; Reason setup
  ;;----------------------------------------------------------------------------
  (add-hook 'reason-mode-hook #'subword-mode)
  (defun shell-cmd (cmd)
    "Returns the stdout output of a shell command or nil if the command returned an error"
    (car (ignore-errors (apply 'process-lines (split-string cmd)))))

  (setq reason/refmt-bin
        (or (shell-cmd "refmt ----where")
            (shell-cmd "which refmt")))

  (setq reason/merlin-bin
        (or (shell-cmd "which ocamlmerlin")
            (shell-cmd "ocamlmerlin ----where")))

  (setq reason/merlin-base-dir
        (when reason/merlin-bin
          (replace-regexp-in-string "bin/ocamlmerlin$" "" reason/merlin-bin)))

  ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  (when reason/merlin-bin
    (setq merlin-command reason/merlin-bin))

  (when reason/refmt-bin
    (setq refmt-command reason/refmt-bin))

  ;; We're working on a native project, load and use merlin
  (init-reason-merlin))
