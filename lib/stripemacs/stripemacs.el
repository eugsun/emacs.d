;;; init-st.el --- ST-specific configs       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Eugene Sun

;; Author: Eugene Sun <eugsun@st-eugsun1>
;; Keywords:

(defconst st/ruby-repo "~/stripe/pay-server")

;; Decides if the buffer is Ruby and in pay server
(defun st/activate-lsp-p (filename mode)
  (and
   (string-prefix-p (expand-file-name st/ruby-repo)
                    filename)
   (or (eq major-mode 'ruby-mode) (eq major-mode 'enh-ruby-mode))))

;; Configure the connection to Sorbet
;; (add-to-list 'eglot-server-programs
;;              `(enh-ruby-mode . ("pay" "exec" "scripts/bin/typecheck" "--lsp")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   '("pay" "exec" "scripts/bin/typecheck" "--lsp"))
  :major-modes '(ruby-mode enh-ruby-mode)
  :priority 25
  :activation-fn 'st/activate-lsp-p
  :server-id 'stripe-sorbet-lsp))

(provide 'stripemacs)
