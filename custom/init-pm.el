;; -*- lexical-binding: t; -*-

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :commands counsel-projectile-switch-project
  :init
  (setq projectile-enable-caching t)
  ;; (setq projectile-require-project-root nil)
  (setq projectile-indexing-method 'alien)
  (when (executable-find "fd")
    (setq projectile-git-command "fd . -0 --type f --color=never")))
(use-package counsel-projectile
  :after (counsel projectile))

(use-package magit
  :commands magit-status)

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
