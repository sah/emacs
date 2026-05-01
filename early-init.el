;;; early-init.el --- Pre-init setup -*- lexical-binding: t -*-

;; Speed up startup by bumping GC threshold; restored in init.el
(setq gc-cons-threshold most-positive-fixnum)

;; We'll manage packages with use-package explicitly
(setq package-enable-at-startup nil)

;; Kill chrome before the first frame renders (no flash)
(push '(menu-bar-lines . 0)         default-frame-alist)
(push '(tool-bar-lines . 0)         default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Native-comp: silence warnings buffer, still log to *Messages*
(setq native-comp-async-report-warnings-errors 'silent)

(push '(font . "JetBrains Mono:weight=ultralight:size=16") default-frame-alist)
;;(push '(font . "Fira Code Light-16") default-frame-alist)
