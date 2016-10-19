;; load autocomplete
(require 'auto-complete-config)
;;(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

(ac-config-default)

(setq ac-modes (delete 'python-mode ac-modes))
