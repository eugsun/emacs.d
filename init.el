;;
;; PACKAGE SETTINGS
;;
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("elpy" . "https://jorgenschaefer.github.io/packages/"))

  ;; the required packages
  (setq package-list '(dired+ flx-ido highlight-indentation yasnippet
                              smartparens projectile auto-complete
                              multiple-cursors enh-ruby-mode robe
                              ruby-block ruby-end rvm jedi jedi-direx js2-mode
                              elpy emmet-mode neotree
                              json-mode markdown-mode web-mode
                              color-theme-sanityinc-tomorrow window-number
                              magit magit-gh-pulls
                              org-journal solarized-theme
                              helm-ls-git helm helm-ag))

  ;; activate all the packages (in particular autoloads)
  (package-initialize)

  ;; fetch the list of packages available
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  )

(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/custom/")


;; No splash screen
(setq inhibit-startup-message t)

;; Uniquify file names by with directory
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save the last-visited position in a file
(require 'saveplace)
(setq-default save-place t)

;; Show matching pairs of parentheses
(show-paren-mode 1)

;; Cut/Copy settings
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Turn off mouse interface
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Tab settings
(setq-default indent-tabs-mode nil) ;use space
(setq-default tab-width 4)

;; Column rule
(setq column-number-mode t)
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 90)
(setq-default fill-column 90)
(global-whitespace-mode t)

;; Set colors
;; (set-background-color "black")
;; (set-foreground-color "white")
;; (set-cursor-color "white")
(load-theme `solarized-dark t)

;; Put temp files to a temp folder
(defconst emacs-tmp-dir "~/.emacs.d/tmp/")
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

;; Mouse avoidence mode
(mouse-avoidance-mode 'animate)

;; Set font for all windows
;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
;; (set-fontset-font t 'han (font-spec :family "WenQuanYi Micro Hei" :size 16))

;; Delete all trailing whitespace before every save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set default mode
(setq-default major-mode 'org-mode)

;; Load customizations
(load "~/.emacs.d/shortcuts.el")
(mapc 'load (file-expand-wildcards "~/.emacs.d/custom/*.el"))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))


;; Neo Tree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(package-selected-packages
   (quote
    (ace-jump-mode helm-ls-git helm helm-ag window-number web-mode smartparens rvm ruby-end ruby-block robe projectile php-mode org-journal neotree multiple-cursors material-theme markdown-mode magit-gh-pulls json-mode js2-mode jedi-direx ido-vertical-mode ido-ubiquitous flycheck flx-ido find-file-in-repository enh-ruby-mode emmet-mode elpy dired+ color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized auto-complete-auctex angularjs-mode angular-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
