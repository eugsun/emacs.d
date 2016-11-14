;; (require 'helm-ls-git)

(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-;") 'helm-ag-project-root)
(global-set-key (kbd "C-x C-'") 'helm-resume)
(helm-mode 1)
