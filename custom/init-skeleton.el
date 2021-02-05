;;; init-skeleton.el --- management of new-file skeletons  -*- lexical-binding: t; -*-

(defun my/autoinsert-yas-expand ()
  "Replace text in yasnippet template."
  (yas/expand-snippet (buffer-string) (point-min) (point-max)))

(defun my/convert-resource-name (content)
  (capitalize
  (replace-regexp-in-string
   "\.org" ""
  (replace-regexp-in-string
   "-" " "
   (replace-regexp-in-string "_" " : " content)
   ))))

(use-package autoinsert
  :custom
  (auto-insert-directory "~/.emacs.d/skeletons/")
  (auto-insert-query nil)
  :config
  (auto-insert-mode t)
  (define-auto-insert '("\\.org\\'" . "Org skeleton") ["org" my/autoinsert-yas-expand])
  (define-auto-insert '("\\.rkt\\'" . "Racket skeleton") ["rkt" my/autoinsert-yas-expand])
  (define-auto-insert '("\\.py\\'" . "Python skeleton") ["py" my/autoinsert-yas-expand]))
