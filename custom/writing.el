(defun set-reader-view ()
  (text-scale-set 1)
  (setq line-spacing 4)
  (olivetti-set-width 88)
  )
(use-package olivetti
  :ensure t
  :init
  (if (memq window-system '(mac ns x))
      (setq olivetti-body-width 88)
    ;; TODO: Revisit when Emacs 27 fixes window-set-margins
    (setq olivetti-body-width 1.0)
    )
  :config
  (add-hook 'text-mode-hook 'olivetti-mode)
  (add-hook 'olivetti-mode-hook 'set-reader-view)
  ;; (add-hook 'text-mode-hook '(whitespace-mode nil))
  )

;; Thesaurus
(use-package powerthesaurus
  :ensure t)

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
