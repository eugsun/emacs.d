;; -*- lexical-binding: t; -*-

;; Stolen from @purcell. Keep gc-cons-threshold reasonable unless necessary.
(setq normal-gc-cons-threshold (* 100 1024 1024))
(setq init-gc-cons-threshold (* 200 1024 1024))
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

;; Startup Performance
(defun display-startup-time ()
  (message "Emacs started in %s with %d garbage collections."
           (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

;; Native compilation
(setq comp-speed 3)
(setq comp-deferred-compilation t)

;; Custom-file
(defconst custom-file (expand-file-name "spam.el" user-emacs-directory))
;; NOERROR to ignore nonexistent file - Emacs will create it
(load custom-file t)

;; Base UI
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  )
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Use straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
;; straight.el integration
(setq straight-use-package-by-default t)

;;;;;;
;;;;;;
(add-to-list 'load-path "~/.emacs.d/custom/")

(load "init-editor")
(load "init-theme")
(load "init-writing")
(load "init-pm")
(load "init-web")
(load "init-lang")
(load "init-keys")
(load "init-skeleton")
(load "init-util")
(load "init-st")

(load-file custom-file)
