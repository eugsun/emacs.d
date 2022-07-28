;; -*- lexical-binding: t; -*-

;; Evil keybinding
(use-package general
  :config
  (general-evil-setup)

  (general-define-key
   :states '(normal visual)
   :prefix nil
   ;; "TAB" 'indent-for-tab-command
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
   ;; "SPC" '(counsel-M-x :which-key "M-x")
   "SPC" '(execute-extended-command :which-key "M-x")
   ;; "/"   '(counsel-projectile-rg :which-key "ripgrep")
   "/"   '(consult-ripgrep :which-key "ripgrep")
   ;; "."   '(projectile-find-file :which-key "find file in project")
   "."   '(eugsun/consult-fd :which-key "find file in project")
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "previous buffer")
   ;; "p"   '(projectile-switch-project :which-key "switch to project")
   "p"   '(project-switch-project :which-key "switch to project")
   "0"   '(neotree-toggle :which-key "neotree")
   "n"   (general-simulate-key "C-c" :which-key "minor mode prefix")
   ;; Files
   ;; "ff"  '(counsel-find-file :which-key "find file")
   "ff"  '(find-file :which-key "find file")
   "fi"  '(iqa-find-user-init-file :which-key "find init file")
   "fe"  '(iqa-reload-user-init-file :which-key "eval init file")
   ;; "fr"  '(counsel-recentf :which-key "open recent file")
   "fr"  '(consult-recent-file :which-key "open recent file")
   ;; Buffers
   "bi"  '(ibuffer :which-key "ibuffer")
   "br"  '(revert-buffer :which-key "revert buffer")
   ;; "bb"  '(counsel-switch-buffer :which-key "switch buffer")
   "bb"  '(consult-buffer :which-key "switch buffer")
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
   "oe"  'org-export-dispatch
   "oh"  '((lambda () (interactive) (org-agenda nil "h")) :which-key "agenda home")
   "on"  '((lambda () (interactive) (find-file org-default-notes-file)) :which-key "open notes")
   "ot"  '((lambda () (interactive) (find-file org-default-todos-file)) :which-key "open todos")
   "oi"  '((lambda () (interactive) (find-file org-default-ideas-file)) :which-key "open ideas")
   "og"  '((lambda () (interactive) (find-file org-default-games-file)) :which-key "open games")
   "or"  '(org-refile :which-key "refile")
   ;; "o;"  '(org-roam-buffer-toggle-display :which-key "roam display")
   "of"  '(lambda () (interactive) (org/five-minute-journal-entry))
   "ol"  'org-store-link
   "oL"  'org-toggle-link-display
   "oI"  'org-toggle-inline-images
   "oD"  'org-id-get-create
   ;; "oR"  '(org-re-reveal-export-to-html-and-browse)
   ;; Search
   ;; "ss"  '(swiper :which-key "search buffer")
   ;; "sa"  '(swiper-all :which-key "search all buffers")
   "ss"  '(consult-line :which-key "search buffer")
   "sa"  '(consult-line-multi :which-key "search all buffers")
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
   "gg"  '(magit :which-key "magit")
   "gb"  '(magit-blame :which-key "blame")
   "gl"  '(magit-log-current :which-key "log current branch")
   "gL"  '(magit-log-buffer-file :which-key "log current file")
   "q"   '(save-buffers-kill-terminal :which-key "save all and quit")
   "k"   '(browse-kill-ring :which-key "browse kill ring")
   "j"   '(avy-goto-char-timer :which-key "jump to char")
   "T"   '(powerthesaurus-lookup-word-at-point :which-key "look up thesaurus")
   "d"   '(deft :which-key "deft")
   "D"   '(dictionary-lookup-definition :which-key "dict lookup")
   "N"   '(denote :which-key "denote"))

  ;; org-mode bindings
  (general-define-key
   :keymaps 'org-mode-map
   "<f8>" 'org-tree-slide-mode
   "s-<f8>" 'org-tree-slide-skip-done-toggle
   [C-return]  'org/insert-item)

  (general-define-key
   :states '(normal visual)
   :prefix nil
   :keymaps 'org-mode-map
   "gd" 'org-open-at-point
   "TAB" 'org-cycle))

;; Global keybinding
(global-set-key (kbd "C-,") 'embark-act)

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

;; Character inserts
(defun help/real-insert (char)
  (cl-flet ((do-insert
              () (if (bound-and-true-p org-mode)
                     (org-self-insert-command 1)
                   (self-insert-command 1))))
    (setq last-command-event char)
    (do-insert)))
(defun help/insert-em-dash ()
  "Insert a EM-DASH.
- \"best limited to two appearances per sentence\"
- \"can be used in place of commas to enhance readability.
   Note, however, that dashes are always more emphatic than
   commas\"
- \"can replace a pair of parentheses. Dashes are considered
   less formal than parentheses; they are also more intrusive.
   If you want to draw attention to the parenthetical content,
   use dashes. If you want to include the parenthetical content
   more subtly, use parentheses.\"
  - \"Note that when dashes are used in place of parentheses,
     surrounding punctuation should be omitted.\"
- \"can be used in place of a colon when you want to emphasize
   the conclusion of your sentence. The dash is less formal than
   the colon.\"
- \"Two em dashes can be used to indicate missing portions of a
   word, whether unknown or intentionally omitted.\"
  - \"When an entire word is missing, either two or three em
     dashes can be used. Whichever length you choose, use it
     consistently throughout your document. Surrounding punctuation
     should be placed as usual.\"
- \"The em dash is typically used without spaces on either side,
   and that is the style used in this guide. Most newspapers,
   however, set the em dash off with a single space on each side.\"
Source: URL `https://www.thepunctuationguide.com/em-dash.html'"
  (interactive)
  (help/real-insert ?—))
(defun help/insert-en-dash ()
  "Insert a EN-DASH.
- \"is used to represent a span or range of numbers, dates,
   or time. There should be no space between the en dash and
   the adjacent material. Depending on the context, the en
   dash is read as “to” or “through.”\"
  - \"If you introduce a span or range with words such as
     'from' or 'between', do not use the en dash.\"
- \"is used to report scores or results of contests.\"
- \"an also be used between words to represent conflict,
   connection, or direction.\"
- \"When a compound adjective is formed with an element that
   is itself an open compound or hyphenated compound, some
   writers replace the customary hyphen with an en dash. This
   is an aesthetic choice more than anything.
Source: URL `https://www.thepunctuationguide.com/en-dash.html'"
  (interactive)
  (help/real-insert ?–))
(defun help/insert-hyphen ()
  "Insert a HYPHEN
- \"For most writers, the hyphen’s primary function is the
   formation of certain compound terms. The hyphen is also
   used for word division [in typesetting].
- \"Compound terms are those that consist of more than one
   word but represent a single item or idea.\"
Source: URL `https://www.thepunctuationguide.com/hyphen.html'"
  (interactive)
  (help/real-insert ?-))
(global-set-key (kbd "-") #'help/insert-hyphen)
(global-set-key (kbd "s-_") #'help/insert-em-dash)
(global-set-key (kbd "s--") #'help/insert-en-dash)
