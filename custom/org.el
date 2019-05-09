(setq org-base "~/Dropbox/Private/org/")
(setq org-log-done t)

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Agenda
(setq org-agenda-files
      (append
       (file-expand-wildcards (concat org-base "agenda/*.org"))
       (file-expand-wildcards (concat org-base "agenda/job/*.org"))
      ))
(setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")

;; Notes
(setq org-default-notes-file (concat org-base "agenda/notes.org"))
(setq org-default-todos-file (concat org-base "agenda/todos.org"))

;; Journal
(use-package org-journal
  :ensure t
  :init
  :custom
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-dir (concat org-base "journal/"))
  (org-journal-date-format "%A, %m/%d/%Y")
  )

;; Customization
(setq org-agenda-custom-commands
      '(
        ("h" "Home View"
         (
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-Priority Items:")))
          (tags "plan" ((org-agenda-overriding-header "Plan")))
          (agenda "")
          (alltodo '(:timestamp))
          )
         )
        ))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))

(setq org-capture-templates
      `(("t" "Todo" entry (file+headline org-default-todos-file "Tasks")
         ,(concat "* TODO %?\n"
                 "  DEADLINE: <%(org-read-date nil nil \"+1d\")>\n"
                 "  SCHEDULED: <%(org-read-date nil nil \"\")>\n"
                 "  Entered on %U  %i\n\n")
         )
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?\n  Entered on %U  %i\n")))

(add-hook 'org-mode-hook 'olivetti-mode)
