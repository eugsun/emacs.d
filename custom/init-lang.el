;; -*- lexical-binding: t; -*-

(use-package flycheck
  :ensure t)

(use-package dap-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-completion-at-point t)
  (setq gc-cons-threshold init-gc-cons-threshold)
  (setq read-process-output-max (* 3 1024 1024))
  (setq lsp-prefer-capf t)
  (setq lsp-idle-delay 0.500)

  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)

  (use-package lsp-treemacs
    :ensure t)
  )

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 2)
  (global-company-mode t)

  ;; (use-package company-lsp
  ;;   :ensure t
  ;;   :commands company-lsp
  ;;   :config
  ;;   (setq company-lsp-cache-candidates 'auto)
  ;;   (setq company-lsp-async t)
  ;;   )
  )

;; Dart
(use-package dart-mode
  :ensure t
  :mode "\\.dart\\'"
  :config
  (setq dart-debug t)
  ;; server program
  (setq dart-sdk-path (concat (getenv "HOME") "/Apps/flutter/bin/cache/dart-sdk/"))
  (setq dart-analysis-server-bin (concat dart-sdk-path "bin/snapshots/analysis_server.dart.snapshot"))
  ;; (add-to-list 'eglot-server-programs `(dart-mode . ("dart" ,dart-analysis-server-bin "--lsp")))
  ;; project config
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD")
  ;; hooks. use flycheck instead of flymake
  ;; (advice-add 'dart-mode :after #'flymake-mode-off)
  ;; (advice-add 'dart-mode :after #'flycheck-mode-on-safe)
  ;; (advice-add 'dart-mode :after #'eglot-ensure)
  )
(use-package flutter
  :ensure t)
(use-package hover
  :ensure t
  :after flutter
  :config
  (general-define-key
   :keymaps 'dart-mode-map
   :states '(normal visual)
   :prefix "SPC"
   "n h r" #'hover-run-or-hot-reload
   "n h R" #'hover-run-or-hot-restart))
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp)
  )

;;; Configuration of Android projects use Groovy/Gradle
(use-package groovy-mode
  :ensure t
  :mode "\\.gradle\\'")

;; yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

;; Racket
(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'")


;; Python
(use-package python-mode
  :ensure t
  :mode "\\.python\\'")
(use-package lsp-python-ms
  :ensure t
  :after lsp-mode
  :hook (python-mode . lsp)
  )
(use-package python-black
  :ensure t
  :after python-mode
  :config
  (general-define-key
   :keymaps 'python-mode-map
   :prefix "SPC"
   :states '(normal visual)
   "lf" 'python-black-buffer
   ))
(use-package py-isort
  ;; requires:
  ;; pip install isort
  :ensure t
  :after python-mode)
(use-package lsp-pyright
  :ensure t
  :after python-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))


(use-package fountain-mode
  :ensure t)

(use-package imenu-list
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package ink-mode
  :ensure t
  :after olivetti
  :mode "\\.ink\\'"
  :config
  (add-hook 'ink-mode-hook 'olivetti-mode)
  (defun ink-mode-open ()
    (interactive)
    (let (($path (buffer-file-name)))
      (when (executable-find "open")
        (shell-command (format "open -a Inky \"%s\"" $path))
        )
      )
    )
  (general-define-key
   :keymaps 'ink-mode-map
   "C-c C-c" 'ink-mode-open
   )
  )

;; Clojure
(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'")
(use-package cider
  :ensure t
  :defer t)


;; CSharp
(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'"
  :config
  (add-hook 'csharp-mode-hook 'flycheck-mode)
  (advice-add 'csharp-mode :after #'lsp))

;; (use-package omnisharp
;;   :ensure t
;;   :after csharp-mode
;;   :config
;;   (add-to-list 'company-backends 'company-omnisharp)
;;   (defun my-csharp-mode-setup ()
;;     (omnisharp-mode)
;;     (company-mode)
;;     (flycheck-mode)

;;     (setq indent-tabs-mode nil)
;;     (setq c-syntactic-indentation t)
;;     (c-set-style "ellemtel")
;;     (setq c-basic-offset 4)
;;     (setq truncate-lines t)
;;     (setq tab-width 4)
;;     (setq evil-shift-width 4)
;;     (electric-pair-local-mode 1)

;;     (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
;;     (local-set-key (kbd "C-c C-c") 'recompile))
;;   (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;;   (general-define-key
;;    :keymaps 'omnisharp-mode-map
;;    "C-," '(omnisharp-run-code-action-refactoring :which-key "code action")
;;    "C-=" '(omnisharp-code-format-entire-file :which-key "reformat")
;;    )
;;   (general-define-key
;;    :keymaps 'omnisharp-mode-map
;;    :states '(normal visual)
;;    "gd"  'omnisharp-go-to-definition
;;    "gD"  'omnisharp-go-to-definition-other-window
;;    )
;;   )

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2)
  )


(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  )
(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp)
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  )


(use-package sml-mode
  :ensure t
  :mode "\\.sml\\'")


;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp)
         (go-mode . yas-minor-mode))
  :init
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))
