(use-package eglot
  :ensure t
  :config
  (setq eglot-connect-timeout 3)
  ;; (setq eglot-sync-connect nil)
  (setq eglot-auto-display-help-buffer t)
  )

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :config
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-eldoc-render-all nil)
;;   (setq lsp-prefer-flymake nil)
;;   (setq lsp-enable-completion-at-point t)

;;   (use-package lsp-ui
;;     :ensure t
;;     :commands lsp-ui-mode)
;;   )

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 3)
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
(use-package flutter
  :ensure t)
(use-package dart-mode
  :ensure t
  :config
  (setq dart-debug t)
  (setq dart-sdk-path "~/Apps/flutter/bin/cache/dart-sdk/")
  (add-to-list 'eglot-server-programs '(dart-mode . ("dart-analysis-server" "")))
  (advice-add 'dart-mode :after #'flymake-mode-off)
  (advice-add 'dart-mode :after #'flycheck-mode-on-safe)
  (advice-add 'dart-mode :after #'eglot-ensure)
  ;(add-hook 'dart-mode-hook #'lsp)
  ;; (add-hook 'dart-mode-hook 'eglot-ensure)
  ;; (add-hook 'dart-mode-hook 'flycheck-mode)
  ;; (add-hook 'dart-mode-hook '(lambda () (flymake-mode nil)))
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))
  )

;; yaml
(use-package yaml-mode
  :ensure t)


;; Racket
(use-package racket-mode
  :ensure t)


;; Python
(use-package elpy
  :ensure t
  :config
  (advice-add 'python-mode :after #'elpy-enable)
  (advice-add 'python-mode :after #'flymake-mode-off)
  (advice-add 'python-mode :after #'flycheck-mode-on-safe)
  )


;; PDF
;; (use-package pdf-tools
;;   :ensure t)


;; Fountain
(use-package fountain-mode
  :ensure t
  ;; :config
  ;; (add-hook 'fountain-mode-hook 'olivetti-mode)
  )

(use-package imenu-list
  :ensure t
  :config
  ;; (setq imenu-list-focus-after-activation t)
  )


;; Markdown
(use-package markdown-mode
  :ensure t
  ;; :config
  ;; (add-hook 'markdown-mode-hook 'olivetti-mode)
  )


;; Ink
(use-package ink-mode
  :ensure t
  :mode "\\.ink\\'"
  :config
  (add-hook 'ink-mode-hook 'olivetti-mode)
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
  (add-hook 'csharp-mode-hook 'flycheck-mode))

(use-package omnisharp
  :ensure t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  )

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (advice-add 'haskell-mode :after #'eglot-ensure)
  )
