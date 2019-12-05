;; Evil keybinding
(use-package general
  :ensure t
  :config
  (general-evil-setup)

  (general-define-key
   :states '(normal visual)
   :prefix nil
   "TAB" 'indent-for-tab-command
   "gd"  'xref-find-definitions
   "gD"  'xref-find-definitions-other-window
   )

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "/"   '(counsel-rg :which-key "ripgrep")
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "p"   '(counsel-projectile-switch-project :which-key "switch to project")
   "0"   '(neotree-toggle :which-key "neotree")
   "n"   (general-simulate-key "C-c" :which-key "minor mode prefix")
   ;; Files
   "ff"  '(counsel-find-file :which-key "find file")
   "fi"  '(iqa-find-user-init-file :which-key "find init file")
   "fe"  '(iqa-reload-user-init-file :which-key "eval init file")
   ;; Buffers
   "bi"  '(ibuffer :which-key "ibuffer")
   "bb"  '(counsel-ibuffer :which-key "switch buffer")
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
   "w0"  '(delete-other-windows :which-key "delete other windows")
   "wb"  '(balance-windows :which-key "balance windows")
   ;; Org
   "oa"  '(org-agenda :which-key "agenda")
   "oj"  '(org-journal-new-entry :which-key "new journal entry")
   "oo"  '(org-capture :which-key "capture")
   "oh"  '((lambda () (interactive) (org-agenda nil "h")) :which-key "agenda home")
   "ob"  '(org-brain-visualize :which-key "brain")
   "on"  '((lambda () (interactive)(find-file org-default-notes-file)) :which-key "open notes")
   "ot"  '((lambda () (interactive)(find-file org-default-todos-file)) :which-key "open todos")
   "or"  '(org-refile :which-key "refile")
   ;; Search
   "ss"  '(swiper :which-key "search buffer")
   "sa"  '(swiper-all :which-key "search all buffers")
   "sp"  '(counsel-projectile-find-file :which-key "search file in project")
   "sd"  '(xref-find-definitions :which-key "find definition")
   "sD"  '(xref-find-definitions-other-window :which-key "find definition in other frame")
   ;; Modes
   "mo"  '(olivetti-mode :which-key "toggle olivetti mode")
   "mm"  '(mc/edit-lines :which-key "toggle multiple cursors [mc] for lines")
   "mn"  '(mc/mark-next-like-this :which-key "[mc] mark next like this")
   ;; Terminal
   "tt"  '(multi-term :which-key "open terminal")
   "tn"  '(multi-term-next :which-key "next terminal")
   "tp"  '(multi-term-prev :which-key "prev terminal")
   ;; LSP
   "ll"  '(lsp-execute-code-action :which-key "lsp action")
   "li"  '(lsp-organize-imports :which-key "organize imports")
   ;; Yasnippet
   "y"   '(yas-insert-snippet :which-key "yasnippet insert")
   ;; Others
   "'"   '(multi-term-dedicated-toggle :which-key "open terminal")
   "g"   '(magit :which-key "magit")
   "q"   '(save-buffers-kill-terminal :which-key "save all and quit")
   "k"   '(browse-kill-ring :which-key "browse kill ring")
   "j"   '(avy-goto-char-timer :which-key "jump to char")
   "T"   '(powerthesaurus-lookup-word-at-point :which-key "look up thesaurus")
   )

  (general-define-key
   :keymaps 'omnisharp-mode-map
   "C-," '(omnisharp-run-code-action-refactoring :which-key "code action")
   "C-=" '(omnisharp-code-format-entire-file :which-key "reformat")
   )

  )

;; Global keybinding
(global-set-key (kbd "M-0") 'maximize-window)
(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
