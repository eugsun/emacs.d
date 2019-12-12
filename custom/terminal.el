;; Terminals
(use-package multi-term
  :ensure t
  :init
  (unless (memq window-system '(mac ns x))
    (setenv "SHELL" "powershell")
    (setq multi-term-program "powershell")
    )
  :config
  (setq multi-term-dedicated-select-after-open-p t)
  )


;; Aweshell
(add-to-list 'load-path (expand-file-name "~/.emacs.d/library/aweshell"))
(require 'aweshell)
