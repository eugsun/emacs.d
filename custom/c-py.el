(setq elpy-rpc-timeout 5)
(setq elpy-rpc-backend "jedi")

(defun python-mode-indent-hook ()
  "Hooks on indentations for python-mode."
  (setq tab-width 4)
  (setq python-indent 4)
  (setq python-indent-offset 4)
  )
(add-hook 'python-mode-hook 'python-mode-indent-hook)



(defun custom-jedi-setup-hook ()
  (setq jedi:server-args
        (list "--virtual-env" python-shell-virtualenv-root))
  (jedi:setup)
  )
(add-hook 'pyvenv-post-activate-hooks 'custom-jedi-setup-hook)
;;(add-hook 'jedi-mode-hook 'jedi-direx:setup)

(setq jedi:complete-on-dot t)

;; (add-hook 'python-mode-hook 'jedi:setup)

;; (defadvice auto-complete-mode (around disable-auto-complete-for-python)
;;   (unless (eq major-mode 'python-mode) ad-do-it))

;; (ad-activate 'auto-complete-mode)
;; (ac-config-default)

;; (setq jedi:complete-on-dot t)
;; (when (require 'flycheck nil t)
;;   (remove-hook 'elpy-modules 'elpy-module-flymake))

(elpy-enable)

;; (eval-after-load "python"
;;   '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
;; (eval-after-load "python"
;;   '(define-key python-mode-map "\C-c8" 'elpy-autopep8-fix-code))
