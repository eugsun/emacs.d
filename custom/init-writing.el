;; -*- lexical-binding: t; -*-

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands (org-capture org-agenda deft)
  :init
  ;; Location
  (setq org-directory "~/Dropbox/Private/org/")
  (setq org-base org-directory)
  (setq org-agenda-files
        (append
         (file-expand-wildcards (concat org-directory "agenda/*.org"))
         (file-expand-wildcards (concat org-directory "agenda/job/*.org"))
         (file-expand-wildcards (concat org-directory "agenda/mobile/*.org"))
         ))
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-default-notes-file (concat org-base "agenda/notes.org"))
  (setq org-default-todos-file (concat org-base "agenda/todos.org"))
  (setq org-default-ideas-file (concat org-base "agenda/ideas.org"))
  (setq org-default-games-file (concat org-base "agenda/games.org"))

  ;; Editor
  (setq org-log-done t)
  (setq org-tags-column 2)
  (setq org-startup-indented t)

  ;; Habits
  (setq org-habit-preceding-days 14)
  (setq org-habit-following-days 1)
  (setq org-habit-graph-column 80)
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-show-all-today t)

  ;; Agenda
  (setq org-agenda-files
        (append
         (file-expand-wildcards (concat org-base "agenda/*.org"))
         (file-expand-wildcards (concat org-base "agenda/job/*.org"))
         (file-expand-wildcards (concat org-base "agenda/mobile/*.org"))
         ))
  (setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-deadline-warning-days 1)
  (setq org-agenda-custom-commands
        '(
          ("h" "Home View"
           (
            (tags "goal" ((org-agenda-overriding-header "Goals")))
            (agenda "")
            (alltodo '(:timestamp))
            )
           )
          ))

  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))

  (setq org-capture-templates
        `(
          ("t" "Todo" entry (file+headline org-default-todos-file "Tasks")
           ,(concat "* TODO %?\n"
                    "DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))"
                    " SCHEDULED: %t\n%i")
           )
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %?\nEntered on %U\n%i")
          ("i" "Idea" entry (file+headline org-default-ideas-file "Ideas")
           "* %?\nEntered on %U\n%i")
          )
        )

  (setq org-confirm-babel-evaluate nil)

  ;; - Auto-save after toggling todo status
  (advice-add 'org-agenda-todo :after #'org-save-all-org-buffers)

  :config
  ;; Bring back src block completion with <s
  (require 'org-tempo)
  ;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  )

;; Journal
(use-package org-journal
  :after org
  :config
  (add-hook 'org-journal-mode-hook #'outline-minor-mode)
  :custom
  (org-journal-dir (concat org-base "journal"))
  (org-journal-file-type 'monthly)
  (org-journal-date-format "%A, %m/%d/%Y")
  (org-journal-file-format "%Y%m.org")
  :init
  (setq org/five-min-template
        "** 5-minute journal :5min:
*** I'm grateful for
*** What would make today great? [/]
- [ ]
*** Daily affirmation
*** Notable things that happened today
*** How could I have made today better
")
  (defun org/five-minute-journal-entry ()
    (progn
      (org-journal-new-entry "5min")
      (beginning-of-line)
      (insert org/five-min-template)))
  )


;; Noter
(use-package org-noter
  :after org
  :config
  (setq org-noter-notes-search-path `(,(concat org-base "reading/")))
  (setq org-noter-doc-split-fraction '(0.67 . 0.67)))

(use-package org-edna
  :after org
  :init
  ;; This missing function causes malfunction when scheduling.
  (defun org-timestamp-from-string (ts)
    (org-read-date nil t ts)))


;; Presentation
(use-package org-re-reveal
  :commands org-re-reveal-export-to-html-and-browse)
(use-package org-re-reveal-ref
  :after org-re-reveal)

;; HTTP
(use-package verb
  :commands verb-command-map
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; Deft
(use-package deft
  :commands deft
  :config
  ;; (evil-set-initial-state 'deft-mode 'emacs)
  (setq deft-directory (concat org-directory "/kb"))
  (setq deft-recursive t)
  (setq deft-extensions '("org"))
  (setq deft-auto-save-interval 0)
  )

;; Org-roam
(use-package org-roam
  :after deft
  :config
  (setq org-roam-db-location "~/Den/org-roam.db")
  (setq org-roam-directory org-directory)
  )


;; --
;; Utils
;; --
(defun org/insert-item (&optional arg)
  "Liberated from Doom to handle insertion of items."
  (interactive "P")
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (org-insert-item (org-element-property :checkbox context))
       ;; Handle edge case where current item is empty and bottom of list is
       ;; flush against a new heading.
       (when (eq (org-element-property :contents-begin context)
                 (org-element-property :contents-end context))
         (org-end-of-item)
         (org-end-of-line)))

      ;; Add a new table row
      ((or `table `table-row)
       (save-excursion (org-table-insert-row t))
       (org-table-next-row)
       )

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (let (org-insert-heading-respect-content)
           (goto-char (line-end-position))
           (org-end-of-subtree)
           (insert "\n" (make-string level ?*) " "))
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo 'todo)))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))


(defun set-reader-view ()
  (text-scale-set 1)
  (setq line-spacing 4)
  (olivetti-set-width 88)
  )
(use-package olivetti
  :hook (text-mode . olivetti-mode)
  :init
  (if (memq window-system '(mac ns x))
      (setq olivetti-body-width 88)
    ;; TODO: Revisit when Emacs 27 fixes window-set-margins
    (setq olivetti-body-width 1.0)
    )
  :config
  (add-hook 'olivetti-mode-hook 'set-reader-view)
  )

;; Thesaurus
(use-package powerthesaurus
  :commands powerthesaurus-lookup-word-at-point)

;; Spellcheck
(when (executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  )

(when (memq window-system '(mac ns x))
  ;; TODO: Enable this in Windows after it's no longer slow
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
  )
