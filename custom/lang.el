(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-prefer-flymake nil)
  )
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package company-lsp
  :ensure t
  :commands company-lsp)
(use-package flutter
  :ensure t)


;; Dart
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
