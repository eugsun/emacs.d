;; Customizations
(defun xml-mode-indent-hook ()
  "Hooks on indentations for nxml-mode."
  (setq nxml-slash-auto-complete-flag 1)
)
(add-hook 'xml-mode-hook 'xml-mode-indent-hook)
