;; -*- lexical-binding: t; -*-

(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode t)
  (setq projectile-indexing-method 'alien))
(use-package counsel-projectile
  :ensure t
  :requires counsel projectile)


;; Start ssh-agent in Windows for magit
(use-package ssh-agency
  :ensure t)
(use-package magit
  :ensure t
  :defer t)
(use-package forge
  :ensure t
  :requires magit)
(use-package magit-todos
  :ensure t
  :requires magit
  :config
  (magit-todos-mode))
