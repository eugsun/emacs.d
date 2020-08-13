;; Git
(use-package ssh-agency
  :ensure t)
(use-package magit
  :ensure t)
(use-package forge
  :requires magit
  :ensure t)
(use-package magit-todos
  :ensure t
  :init
  (magit-todos-mode))
