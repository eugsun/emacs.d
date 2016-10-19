(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")
(setq org-agenda-files (cons org-journal-dir org-agenda-files))
