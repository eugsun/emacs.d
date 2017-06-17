;; File management
(require 'dired+)
(diredp-toggle-find-file-reuse-dir t)

;; Jump to position
(require `avy)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(avy-setup-default)

;; Writing
(require `darkroom)

;; Incremental search
(require `ivy)
(ivy-mode 1)

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-` i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-` u") 'counsel-unicode-char)
(global-set-key (kbd "C-` f") 'counsel-describe-function)
(global-set-key (kbd "C-` v") 'counsel-describe-variable)
(global-set-key (kbd "C-` l") 'counsel-find-library)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(setq ivy-re-builders-alist
      ;; allow input not in order
      '((t   . ivy--regex-ignore-order)))

;; Show window numbers
(require 'window-number)
(window-number-mode 1)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Templating
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs
             "~/.emacs.d/snippets")
(yas-global-mode)

;; Highlight indentation
(require 'highlight-indentation)
;; (set-face-background 'highlight-indentation-face "#e3e3d3")
;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
(highlight-indentation-mode 1)


;; Projectile
(projectile-global-mode)
(setq projectile-enable-caching t)
;; (setq projectile-completion-system 'grizzl)
;; Press Command-p for fuzzy find in project
(global-set-key (kbd "s-p") 'projectile-find-file)
;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
