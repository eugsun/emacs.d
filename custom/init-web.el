;; -*- lexical-binding: t; -*-

(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(use-package emmet-mode
  :after web-mode
  :hook (web-mode . emmet-mode))

;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

;; Infra
(use-package terraform-mode
  :mode "\\.tf\\'")
