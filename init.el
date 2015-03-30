;;
;; PACKAGE SETTINGS
;;
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  ;; the required packages
  (setq package-list '(dired+ flx-ido highlight-indentation yasnippet
                              smartparens projectile auto-complete
                              auto-complete-auctex find-file-in-repository
                              multiple-cursors editorconfig enh-ruby-mode robe
                              ruby-block ruby-end rvm jedi jedi-direx js3-mode
                              json-mode markdown-mode web-mode))

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

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/custom/")


;; No splash screen
(setq inhibit-startup-message t)

;; Uniquify file names by with directory
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save the last-visited position in a file
(require 'saveplace)
(setq-default save-place t)

;; Better default shortcuts
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x f") 'find-file-in-repository)

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

;; Tab settings
(setq-default indent-tabs-mode nil) ;use space
(setq-default tab-width 2)

;; Column rule
(setq column-number-mode t)
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 80)
(setq-default fill-column 80)
(global-whitespace-mode t)

;; Set colors
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")

;; Put temp files to system temp folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Mouse avoidence mode
(mouse-avoidance-mode 'animate)

;; Set font for all windows
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
(set-fontset-font t 'han (font-spec :family "WenQuanYi Micro Hei" :size 16))

;; Delete all trailing whitespace before every save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use editorconfig
(load "editorconfig")

;; Load customizations
(mapc 'load (file-expand-wildcards "~/.emacs.d/custom/*.el"))
