(require 'magit)
;;(require 'magit-gh-pulls)

(global-set-key (kbd "C-c s") 'magit-status)

;; (setq magit-refresh-status-buffer nil)
;; (setq auto-revert-buffer-list-filter
;;       'magit-auto-revert-repository-buffers-p)

(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
