;; Interactively Do Things!
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Uniquify file names by with directory
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save the last-visited position in a file
(require 'saveplace)
(setq-default save-place t)

;; Better default shortcuts
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Defaults to regex search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Show matching pairs of parentheses
(show-paren-mode 1)

;; Cut/Copy settings
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Turn off mouse interface
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Package manager settings
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  ;; the required packages
  (setq package-list '(auctex multiple-cursors
                              auto-complete auto-complete-auctex))

  ;; activate all the packages (in particular autoloads)
  (package-initialize)

  ;; fetch the list of packages available
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
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
(setq whitespace-line-column 100)
(global-whitespace-mode t)

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

;; init auctex
(load "init-auctex-config.el")

;; init ibus
(load "init-ibus.el")

;; init multiple-cursors
(load "init-multiple-cursors.el")
