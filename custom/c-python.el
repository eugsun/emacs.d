(defun python-mode-indent-hook ()
  "Hooks on indentations for python-mode."
  (setq tab-width 2)
  (setq python-indent 2)
)

(add-hook 'python-mode-hook 'python-mode-indent-hook)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'jedi-mode-hook 'jedi-direx:setup)

(setq jedi:complete-on-dot t)
(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
