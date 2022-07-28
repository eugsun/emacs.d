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

;; Timer
(use-package tmr)

;; Workspaces
(use-package perspective
  :commands persp-switch
  :config
  (persp-mode))

;; History
(use-package savehist
  :custom
  (history-length 25)
  :config
  (savehist-mode))

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


; Completion framework
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
(use-package vertico
  :custom
  (enable-recursive-minibuffers t)
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package consult
  :config
  (defun eugsun/consult-fd (&optional dir initial)
    "Find project files.
  A replacement for `projectile-find-file'."
    (interactive "P")
    (let ((consult-find-command "fd --color=never --hidden --exclude .git/ --full-path ARG OPTS"))
      (consult-find dir initial)))
  )
(use-package marginalia
  :config
  (marginalia-mode))
(use-package embark)
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Ivy
;; (use-package ivy
;;   :diminish ivy-mode
;;   :init
;;   (setq ivy-use-virtual-buffer t)
;;   (setq enable-recursive-minibuffers t)
;;   (setq ivy-sort-max-size 7500)
;;   :config
;;   (ivy-mode 1))
;; (use-package counsel
;;   :after ivy
;;   ;; :init
;;   ;; (setq counsel-projectile-find-file-matcher 'ivy--re-filter)
;;   ;; (setq counsel-rg-base-command
;;   ;;       "rg -S -M 140 --no-heading --line-number --color never %s .")
;;   )
;; (use-package rg
;;   :commands counsel-rg)
;; (use-package swiper
;;   :after ivy)

;; Search
(use-package rg)

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
  :custom
  (evil-undo-system 'undo-tree)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undos")))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode)
  )

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
  ;; (setq completion-styles '(basic substring partial-completion))
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
(save-place-mode t)

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

(recentf-mode t)

(setq use-dialog-box nil)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

;;;; colorize output in compile buffer
(require 'ansi-color)
(defun my/colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)
