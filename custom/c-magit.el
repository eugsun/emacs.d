(require 'magit)
;;(require 'magit-gh-pulls)

(global-set-key (kbd "C-c s") 'magit-status)

(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
