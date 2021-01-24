;; -*- lexical-binding: t; -*-

(use-package counsel-projectile
  :ensure t
  :requires counsel projectile)
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1)
  (setq projectile-indexing-method 'alien))


(use-package ssh-agency
  :ensure t)
(use-package magit
  :ensure t)
(use-package forge
  :requires magit
  :ensure t)
(use-package magit-todos
  :ensure t
  :requires magit
  :init
  (magit-todos-mode))
