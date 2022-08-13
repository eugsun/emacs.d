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
  :custom
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  (consult-project-function (lambda (_) (vc-root-dir)))
  :config
  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "--full-path"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

  ;; (setq eugsun/fd-args
  ;;       "fd --color=never --hidden --full-path -E {'.git/', '*/autogen_from_ruby/', '*/*.test.*', '*/*.rbi'} ARG OPTS")
  ;; (setq eugsun/fd-args
  ;;       "fd --color=never --hidden --full-path -E '.git/' -E '**/autogen_from_ruby/' -E '**/*.test.*' -E '*/*.rbi'")
  ;; (defun eugsun/consult-fd (&optional dir initial)
  ;;   "Find project files (excluding test files). Replacement for `projectile-find-file'."
  ;;   (interactive "P")
  ;;   (let ((consult-find-args eugsun/fd-args))
  ;;     (consult-find dir initial)))
  ;; (defun eugsun/consult-fd-with-tests (&optional dir initial)
  ;;   "Find project files (including test files). Replacement for `projectile-find-file'."
  ;;   (interactive "P")
  ;;   (let ((consult-find-args "fd --color=never --hidden --full-path --exclude .git/ --exclude '**/autogen_from_ruby/' ARG OPTS"))
  ;;     (consult-find dir initial)))
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
