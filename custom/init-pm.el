;; -*- lexical-binding: t; -*-

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :commands counsel-projectile-switch-project
  :init
  (setq
   projectile-auto-discover nil
   projectile-ignored-projects '("~/")
  )
  (setq projectile-enable-caching t)
  ;; (setq projectile-require-project-root nil)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (when (executable-find "fd")
    (setq projectile-git-command "fd . -0 --type f --color=never")))

(use-package counsel-projectile
  :after (counsel projectile))

(use-package magit
  :commands magit-status
  :config
  (setq magit-refresh-verbose nil)      ; Toggle when need to profile
  (setq magit-refresh-status-buffer nil)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers))

;; TODO: Figure out performative alternative
;; (use-package forge
;;   :after magit)

;; TODO: Figure out performative alternative
;; (use-package magit-todos
;;   :after magit
;;   :config
;;   (magit-todos-mode))

;; Start ssh-agent in Windows for magit
(use-package ssh-agency
  :after magit)
