;; -*- lexical-binding: t; -*-

;; Stolen from @purcell. Keep gc-cons-threshold reasonable unless necessary.
(setq normal-gc-cons-threshold (* 20 1024 1024))
(setq init-gc-cons-threshold (* 128 1024 1024))
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

(setq comp-speed 2)
(setq comp-deferred-compilation t)
(setq custom-file "~/.emacs.d/spam.el")

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

;;;;;;
;;;;;;
(add-to-list 'load-path "~/.emacs.d/custom/")

(load "init-editor")
(load "init-theme")
(load "init-writing")
(load "init-pm")
(load "init-web")
(load "init-lang")
(load "init-util")
(load "init-keys")
(load "init-skeleton")

(load-file custom-file)
