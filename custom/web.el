(use-package emmet-mode
  :ensure t)

(use-package web-mode
  :requires emmet-mode
  :ensure t
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  :config
  (emmet-mode t)
  )

;; Javascript
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2)
  )

;; Infra
(use-package terraform-mode
  :ensure t)
