;; set load-path
(add-to-list 'load-path "~/.emacs.d/")

;; set colors
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")

;; load autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

