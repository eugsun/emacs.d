;; Turn off mouse interface
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Add package manager
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives 
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; set load-path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

;; tab settings
(setq-default indent-tabs-mode nil) ;use space
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-line-function 'insert-tab)

;; 80 column rule
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)

;; set colors
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")

;; put temp files to system temp folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; emacs <-> system copy
(setq x-select-enable-clipboard t)

;; mouse avoidence mode
(mouse-avoidance-mode 'animate)

;; init auto-complete
(load "init-ac.el")

;; init auctex-config
(load "init-auctex-config.el")

;; init ibus.el
(load "init-ibus.el")
