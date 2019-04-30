;; Keybinding
;; Evil keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "/"   '(counsel-rg :which-key "ripgrep")
  "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  "SPC" '(counsel-M-x :which-key "M-x")
  ;; Files
  "ff"  '(counsel-find-file :which-key "find file")
  ;; Buffers
  "bb"  '(ivy-switch-buffer :which-key "switch buffer")
  "be"  '(eval-buffer :which-key "eval buffer")
  "bk"  '(kill-current-buffer :which-key "kill buffer")
  ;; Window
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wd"  '(delete-window :which-key "delete window")
  ;; Org
  "oo"  '(org-agenda :which-key "agenda home")
  "oj"  '(org-journal-new-entry :which-key "new journal entry")
  ;; Others
  "'"   '(ansi-term :which-key "open terminal")
  "gs"  '(magit :which-key "magit")
  "mo"  '(olivetti-mode :which-key "toggle olivetti mode")
))

;; Better default shortcuts
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Defaults to regex search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "C-(") 'previous-buffer)
(global-set-key (kbd "C-)") 'next-buffer)
