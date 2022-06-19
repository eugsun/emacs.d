;; -*- lexical-binding: t; -*-

;; Paths
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-arguments nil)
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables
        `("PATH" "MANPATH" "WORKON_HOME" "DICPATH"))
  :config
  (exec-path-from-shell-initialize))

;; IQA allows find/reload of init file
(use-package iqa
  :commands iqa-find-user-init-file
  :config
  (iqa-setup-default))


;; Workspaces
(use-package perspective
  :commands persp-switch
  :config
  (persp-mode))


;; Evil mode
(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-integration t)
  ;; (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))
(use-package evil-terminal-cursor-changer
  :after evil
  :init
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar) ; _
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate) ; or (etcc-on)
    ))


;; Ivy
(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffer t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-sort-max-size 7500)
  :config
  (ivy-mode 1))
(use-package counsel
  :after ivy
  ;; :init
  ;; (setq counsel-projectile-find-file-matcher 'ivy--re-filter)
  ;; (setq counsel-rg-base-command
  ;;       "rg -S -M 140 --no-heading --line-number --color never %s .")
  )
(use-package rg
  :commands counsel-rg)
(use-package swiper
  :after ivy)


;; Show keybinding hints
(use-package which-key
  :config
  (which-key-mode 1))


;; Neotree, which requires all the icons
(use-package neotree
  :commands neotree-toggle
  :init
  ;; NeoTree theme uses the icons
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
  (evil-set-initial-state 'neotree-mode 'emacs)
  )
(use-package all-the-icons
  :after neotree)


;; Editor
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq evil-undo-system 'undo-tree))

(use-package multiple-cursors)

(use-package yasnippet
  ;; :commands (yas/expand-snippet yas-insert-snippet)
  :config
  (yas-global-mode t))
(use-package yasnippet-snippets
  :after yasnippet)

(use-package smartparens
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  (sp-local-pair 'org-mode "~" "~")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "*" "*")
  (sp-local-pair 'org-mode "/" "/"))

(use-package avy)
(use-package ace-window)
(use-package browse-kill-ring)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode))
;; (use-package indent-guide
;;   :config
;;   (indent-guide-global-mode t))

(use-package expand-region)

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package bufler
  :commands bufler-list
  :init
  (evil-set-initial-state 'bufler-list-mode 'emacs)
  (setq completion-styles '(basic substring partial-completion))
  )

;; Terminals
(use-package multi-term
  :commands (multi-term-dedicated-toggle multi-term)
  :init
  (unless (memq window-system '(mac ns x))
    (setenv "SHELL" "powershell")
    (setq multi-term-program "powershell")
    )
  :config
  (setq multi-term-dedicated-select-after-open-p t))


;; Put temp files in a more sane place
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory ".backup"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory ".backup") t)))
(setq save-place-file (concat user-emacs-directory ".places"))

;; Global Editor Configuration
(require 'uniquify)
(require 'saveplace)
(setq-default save-place t)

(setq-default line-spacing 2)
(setq-default cursor-type '(bar . 2))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq ring-bell-function 'ignore)
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")
(setq column-number-mode t)

;; Rendering speedups
(setq bidi-paragraph-direction 'left-to-right)

(setq save-interprogram-paste-before-kill t
      require-final-newline t
      load-prefer-newer t)

(global-hl-line-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (setq initial-major-mode 'org-mode)
