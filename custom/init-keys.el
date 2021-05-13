;; -*- lexical-binding: t; -*-

;; Evil keybinding
(use-package general
  :config
  (general-evil-setup)

  (general-define-key
   :states '(normal visual)
   :prefix nil
   "TAB" 'indent-for-tab-command
   "gd"  'xref-find-definitions
   "gD"  'xref-find-definitions-other-window
   ;; Navigation
   "j"   'evil-next-visual-line
   "k"   'evil-previous-visual-line
   )

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "SPC" '(counsel-M-x :which-key "M-x")
   "/"   '(counsel-rg :which-key "ripgrep")
   "."   '(counsel-projectile-find-file :which-key "find file in project")
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "previous buffer")
   "p"   '(counsel-projectile-switch-project :which-key "switch to project")
   "0"   '(neotree-toggle :which-key "neotree")
   "n"   (general-simulate-key "C-c" :which-key "minor mode prefix")
   ;; Files
   "ff"  '(counsel-find-file :which-key "find file")
   "fi"  '(iqa-find-user-init-file :which-key "find init file")
   "fe"  '(iqa-reload-user-init-file :which-key "eval init file")
   ;; Buffers
   "bi"  '(ibuffer :which-key "ibuffer")
   "br"  '(revert-buffer :which-key "revert buffer")
   "bb"  '(bufler-switch-buffer :which-key "switch buffer")
   "bl"  '(bufler-list :which-key "bufler list")
   "be"  '(eval-buffer :which-key "eval buffer")
   "bk"  '(kill-current-buffer :which-key "kill buffer")
   "bn"  '((lambda () (interactive)
             (let (($buf (generate-new-buffer "untitled")))
               (switch-to-buffer $buf)
               (funcall initial-major-mode)
               (setq buffer-offer-save t)
               )) :which-key "create new buffer")
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
   "oJ"  '(org-journal-new-entry :which-key "new journal entry")
   "oj"  '((lambda () (interactive) (org-journal-open-current-journal-file)) :which-key "today's journal")
   "of"  '((lambda () (interactive) (org/five-minute-journal-entry)) :which-key "new five-min entry")
   "oo"  '(org-capture :which-key "capture")
   "oh"  '((lambda () (interactive) (org-agenda nil "h")) :which-key "agenda home")
   "on"  '((lambda () (interactive) (find-file org-default-notes-file)) :which-key "open notes")
   "ot"  '((lambda () (interactive) (find-file org-default-todos-file)) :which-key "open todos")
   "oi"  '((lambda () (interactive) (find-file org-default-ideas-file)) :which-key "open ideas")
   "og"  '((lambda () (interactive) (find-file org-default-games-file)) :which-key "open games")
   "or"  '(org-refile :which-key "refile")
   "o;"  '(org-roam-buffer-toggle-display :which-key "roam display")
   "of"  '(lambda () (interactive) (org/five-minute-journal-entry))
   "ol"  'org-store-link
   ;; "oR"  '(org-re-reveal-export-to-html-and-browse)
   ;; Search
   "ss"  '(swiper :which-key "search buffer")
   "sa"  '(swiper-all :which-key "search all buffers")
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
   "lf"  '(lsp-format-buffer :which-key "format buffer")
   ;; Yasnippet
   "y"   '(yas-insert-snippet :which-key "yasnippet insert")
   ;; Workspaces
   ",,"  '(persp-switch :which-key "switch perspective")
   ",r"  '(persp-rename  :which-key "rename perspective")
   ",n"  '(persp-next :which-key "next perspective")
   ",p"  '(persp-prev :which-key "prev perspective")
   ;; Others
   ";"   '(aweshell-dedicated-toggle :which-key "toggle eshell")
   "'"   '(multi-term-dedicated-toggle :which-key "toggle terminal")
   "g"   '(magit :which-key "magit")
   "q"   '(save-buffers-kill-terminal :which-key "save all and quit")
   "k"   '(browse-kill-ring :which-key "browse kill ring")
   "j"   '(avy-goto-char-timer :which-key "jump to char")
   "T"   '(powerthesaurus-lookup-word-at-point :which-key "look up thesaurus")
   "d"   '(deft :which-key "deft")
   )

  ;; org-mode bindings
  (general-define-key
   :keymaps 'org-mode-map
   "<f8>" 'org-tree-slide-mode
   "s-<f8>" 'org-tree-slide-skip-done-toggle)

  (general-define-key
   :states '(normal visual)
   :prefix nil
   :keymaps 'org-mode-map
   "gd" 'org-open-at-point
   [C-return]  'org/insert-item
   )

  (general-define-key
   :states '(insert emacs)
   :prefix nil
   :keymaps 'org-mode-map
   [C-return]  'org/insert-item
   ))

;; Global keybinding
(global-set-key (kbd "M-0") 'maximize-window)
(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-'") #'imenu-list-smart-toggle)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-S") 'isearch-forward-symbol-at-point)

(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(global-set-key (kbd "C-=") #'er/expand-region)
