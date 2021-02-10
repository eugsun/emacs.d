;; -*- lexical-binding: t; -*-

;; Stolen from @purcell. Keep gc-cons-threshold reasonable unless necessary.
(setq normal-gc-cons-threshold (* 100 1024 1024))
(setq init-gc-cons-threshold (* 200 1024 1024))
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

(setq comp-speed 2)
(setq comp-deferred-compilation t)
(setq custom-file "~/.emacs.d/spam.el")

;; Base UI
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  )
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'org-plus-contrib)
  )

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;;;;;;
;;;;;;
(add-to-list 'load-path "~/.emacs.d/custom/")

(load "init-theme")
(load "init-editor")
(load "init-writing")
(load "init-pm")
(load "init-web")
(load "init-lang")
(load "init-keys")
(load "init-skeleton")

(load "init-util")

(load-file custom-file)
