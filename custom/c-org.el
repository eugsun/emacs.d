(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-c \C-j" 'org-journal-new-entry)
(setq org-log-done t)

(setq org-journal-dir "~/Dropbox/Private/journal/")
(setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")
(setq org-agenda-files (cons org-journal-dir org-agenda-files))
