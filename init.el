;; -*- lexical-binding: t; -*-

;; Stolen from @purcell. Keep gc-cons-threshold reasonable unless necessary.
(setq normal-gc-cons-threshold (* 20 1024 1024))
(setq init-gc-cons-threshold (* 128 1024 1024))
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

;; Settings for native builds
(setq comp-speed 3)
(setq comp-deferred-compilation t)

;; Disable ugly UI
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  )
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
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
  (package-install 'org-plus-contrib))
(require 'use-package)


;; Benchmark initialization
;;(use-package benchmark-init
;;  :ensure t)


;; Paths
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (setq exec-path-from-shell-variables
        `("PATH" "MANPATH" "WORKON_HOME"))
  (exec-path-from-shell-initialize))


;; Put temp files in a more sane place
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory ".backup"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory ".backup") t)))
(setq save-place-file (concat user-emacs-directory ".places"))


;; IQA allows find/reload of init file
(use-package iqa
  :ensure t
  :config
  (iqa-setup-default))

;; Workspaces
(use-package perspective
  :ensure t
  :config
  (persp-mode))

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


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


;; Show keybinding hints
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))


;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1)
  (setq projectile-indexing-method 'alien))


;; All The Icons
(use-package all-the-icons
  :ensure t)
;; NeoTree (which uses the icons)
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))


;; Editor
(global-undo-tree-mode)
(setq ring-bell-function 'ignore)
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)
(show-paren-mode 1)
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-hl-line-mode t)
(setq-default line-spacing 2)
(setq-default cursor-type '(bar . 2))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq column-number-mode t)

(use-package multiple-cursors
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t))

(use-package avy
  :ensure t)
(use-package ace-window
  :ensure t)
(use-package browse-kill-ring
  :ensure t)

(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode t))

(use-package expand-region
  :ensure t)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))
(use-package whitespace
  :ensure t
  :init
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (setq whitespace-line-column 88)
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package bufler
  :ensure t
  :init
  (evil-set-initial-state 'bufler-list-mode 'emacs)
  :config
  (bufler-mode))


;; Misc. variables
(setq save-interprogram-paste-before-kill t
      require-final-newline t
      load-prefer-newer t
      helm-ff-keep-cached-candidates nil)

(setq initial-major-mode 'org-mode)

;;;;;;
;;;;;;
(load-file custom-file)
(mapc 'load (file-expand-wildcards "~/.emacs.d/custom/*.el"))
