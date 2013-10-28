;; set load-path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/lib/")

;; tab settings
(setq-default indent-tabs-mode nil) ;use space
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-line-function 'insert-tab)

;; 80 column rule
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; set colors
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")

;; hide menubar ad toolbar
;;(menu-bar-mode -1)
(tool-bar-mode -1)

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
