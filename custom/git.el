;; Git
(use-package ssh-agency
  :ensure t)
(use-package magit
  :ensure t)
(use-package forge
  :requires magit
  :ensure t)

(defadvice magit-status (around magit-fullscreen activate)
  "Make magit-status run alone in a frame."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
