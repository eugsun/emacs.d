(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-completion-at-point t)

  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)
  )

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1)
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t)

  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :config
    (setq company-lsp-cache-candidates t)
    (setq company-lsp-async t)
    )
  )

;; Dart
(use-package flutter
  :ensure t)
(use-package dart-mode
  :ensure t
  :init
  (setq dart-sdk-path "~/Apps/flutter/bin/cache/dart-sdk/")
  (add-hook 'dart-mode-hook #'lsp)
  (add-hook 'dart-mode-hook 'flycheck-mode)
  ;;(add-hook 'dart-mode-hook (lambda () (flymake-mode -1)))
  ;;(with-eval-after-load "projectile"
  ;;(add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  ;;(add-to-list 'projectile-project-root-files-bottom-up "BUILD"))
  ;;(setq lsp-auto-guess-root t)
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
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))


;; PDF
(use-package pdf-tools
  :ensure t)


;; Fountain
(use-package fountain-mode
  :ensure t
  :init
  (add-hook 'fountain-mode-hook 'olivetti-mode)
  )

(use-package imenu-list
  :ensure t
  :config
  ;; (setq imenu-list-focus-after-activation t)
  )


;; Markdown
(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook 'olivetti-mode)
  )
