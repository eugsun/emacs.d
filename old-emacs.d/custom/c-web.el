(require 'web-mode)

;; File types
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Indentation
(defun web-mode-indent-hook ()
  "Hooks on indentations for web-mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
)
(add-hook 'web-mode-hook 'web-mode-indent-hook)

;; Emmet mode
(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
