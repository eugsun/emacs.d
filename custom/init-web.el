;; -*- lexical-binding: t; -*-

(use-package emmet-mode
  :ensure t)

(use-package web-mode
  :requires emmet-mode
  :ensure t
  :mode ("\\.html\\'" "\\.css\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (emmet-mode t))

;; Javascript
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

;; Infra
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")
