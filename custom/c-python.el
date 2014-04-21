(defun python-mode-indent-hook ()
  "Hooks on indentations for python-mode."
  (setq tab-width 2)
)
(add-hook 'python-mode-hook 'python-mode-indent-hook)
