;; Disable ugly UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; (add-to-list 'default-frame-alist '(font . "mononoki-12"))
(setq spam-file "~/.emacs.d/spam.el")

;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ;; ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; Paths
(let ((path (shell-command-to-string ". ~/.dotfiles/paths.sh")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(setq backup-directory-alist '(("" . "~/.emacs.d/.backup")))

;; Evil mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))


;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-spacegrey t))


;; Buffer completion
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1))
(use-package counsel
  :ensure t)


;; Show next steps
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))


;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))


;; All The Icons
(use-package all-the-icons :ensure t)

;; NeoTree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))


;; Editor
(setq show-paren-delay 0)
(show-paren-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(use-package olivetti
  :ensure t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Magit
(use-package magit
  :ensure t)


;; Flair
(mouse-avoidance-mode 'animate)


(load-file spam-file)
(mapc 'load (file-expand-wildcards "~/.emacs.d/custom/*.el"))
