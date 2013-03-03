;; set load-path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/lib/")

;; set colors
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")

;; hide menubar ad toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; put temp files to system temp folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; emacs <-> system copy
(setq x-select-enable-clipboard t)

;; init auto-complete
(load "init-ac.el")

;; init auctex-config
(load "init-auctex-config.el")
