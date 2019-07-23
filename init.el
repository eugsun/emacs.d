;; Disable ugly UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; (add-to-list 'default-frame-alist '(font . "mononoki-12"))
(setq custom-file "~/.emacs.d/spam.el")

;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


;; Paths
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
 )

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory ".backup"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory ".backup") t)))
(setq save-place-file (concat user-emacs-directory ".places"))

(use-package iqa
  :ensure t
  :config
  (iqa-setup-default))

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1))


;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; (load-theme 'doom-peacock t)
  (setq current-theme 'doom-peacock)
  (defun sync-theme-with-time ()
    (setq hour (string-to-number (substring (current-time-string) 11 13)))
    (if (member hour (number-sequence 6 16))
        (setq now 'doom-solarized-light)
      (setq now 'doom-peacock))
    (if (or (not (boundp 'current-theme)) (eq now current-theme))
        nil
      (setq current-theme now))
    (load-theme now t)
    )
  (run-with-timer 0 3600 #'sync-theme-with-time)
  )


(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))


;; Typeface
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
;; -- Default
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata"))
(when (member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :font "Consolas"))
;; -- Unicode
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend))
;; -- Chinese
(when (member "WenQuanYi Micro Hei" (font-family-list))
  (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei" ))


;; Ivy
(use-package rg
  :ensure t)
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffer t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))
(use-package counsel
  :ensure t
  :init
  (setq counsel-rg-base-command
        "rg -S -M 140 --no-heading --line-number --color never %s ."))
(use-package swiper
  :ensure t)
(use-package counsel-projectile
  :ensure t
  :requires counsel projectile)

;; Show next steps
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))


;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))


;; All The Icons
(use-package all-the-icons :ensure t)

;; NeoTree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))


;; Editor
(setq ring-bell-function 'ignore)
(setq show-paren-delay 0)
(show-paren-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(use-package multiple-cursors
  :ensure t)

(use-package olivetti
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
)

(use-package avy
  :ensure t)
(use-package ace-window
  :ensure t)
(use-package browse-kill-ring
  :ensure t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Git
(use-package ssh-agency
  :ensure t)
(use-package magit
  :ensure t)
(use-package forge
  :requires magit
  :ensure t)

;; Tab settings
(setq-default indent-tabs-mode nil) ;use space
(setq-default tab-width 4)

;; Column rule
(setq column-number-mode t)
;; (use-package whitespace
;;   :ensure t
;;   :init
;;   (setq whitespace-style '(face empty tabs lines-tail trailing))
;;   (setq-default fill-column 80)
;;   (setq whitespace-line-column 80)
;;   )


;; Terminals
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-dedicated-select-after-open-p t)
  )


;; Thesaurus
(use-package powerthesaurus
  :ensure t)


;; Copied from better defaults
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      )

;;;;;;
;;;;;;
(load-file custom-file)
(load-file "~/.emacs.d/utils.el")
(mapc 'load (file-expand-wildcards "~/.emacs.d/custom/*.el"))
