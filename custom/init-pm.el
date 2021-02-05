;; -*- lexical-binding: t; -*-

(use-package projectile
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode t)
  (setq projectile-indexing-method 'alien))
(use-package counsel-projectile
  :after (counsel projectile))

(use-package magit
  :commands magit-status)
(use-package forge
  :after magit)
(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))
;; Start ssh-agent in Windows for magit
(use-package ssh-agency
  :after magit)
