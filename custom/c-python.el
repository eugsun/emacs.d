(defun python-mode-indent-hook ()
  "Hooks on indentations for python-mode."
  (setq tab-width 4)
  (setq python-indent 4)
  (setq python-indent-offset 4)
  )

(add-hook 'python-mode-hook 'python-mode-indent-hook)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'jedi-mode-hook 'jedi-direx:setup)

(defadvice auto-complete-mode (around disable-auto-complete-for-python)
  (unless (eq major-mode 'python-mode) ad-do-it))

(ad-activate 'auto-complete-mode)
;; (ac-config-default)

;; (setq jedi:complete-on-dot t)

(elpy-enable)

(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
(eval-after-load "python"
  '(define-key python-mode-map "\C-c8" 'elpy-autopep8-fix-code))
