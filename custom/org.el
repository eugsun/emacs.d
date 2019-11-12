;; Location
(setq org-base "~/Dropbox/Private/org/")
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-default-notes-file (concat org-base "agenda/notes.org"))
(setq org-default-todos-file (concat org-base "agenda/todos.org"))

;; Editor
(setq org-log-done t)
(setq org-tags-column 2)
(setq org-startup-indented t)

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
          ;; (tags "PRIORITY=\"A\""
          ;;       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
          ;;        (org-agenda-overriding-header "High-Priority Items:")))
          (tags "goal" ((org-agenda-overriding-header "Goals")))
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
                 "  DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))"
                 " SCHEDULED: %t\n"
                 "  Entered on %U\n  %i")
         )
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?\n  Entered on %U\n  %i")))

;; - Auto-save after toggling todo status
(advice-add 'org-agenda-todo :after #'org-save-all-org-buffers)

;; Journal
(use-package org-journal
  :ensure t
  :init
  :custom
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-dir (concat org-base "journal"))
  (org-journal-date-format "%A, %m/%d/%Y")
  )

;; Brain
(use-package org-brain
  :ensure t
  :init
  (setq org-brain-path (concat org-base "brain"))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  (add-hook 'org-brain-visualize-mode-hook 'olivetti-mode)
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  ;; (setq org-brain-visualize-default-choices 'all)
  ;; (setq org-brain-title-max-length 12)
  )


;; Noter
(use-package org-noter
  :ensure t
  :config
  (setq org-noter-notes-search-path `(,(concat org-base "reading/")))
  (setq org-noter-doc-split-fraction '(0.67 . 0.67))
  )


;; Habit
(require 'org-habit)
(setq org-habit-preceding-days 14
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today nil
      )

(use-package org-edna
  :ensure t
  :init
  ;; This missing function causes malfunction when scheduling.
  (defun org-timestamp-from-string (ts)
    (org-read-date nil t ts)
    )
  :config
  (org-edna-load)
  )


;; Presentation
(use-package org-re-reveal
  :ensure t
  )
(use-package org-re-reveal-ref
  :ensure t
  :after org-re-reveal
  )
