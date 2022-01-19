;; -*- lexical-binding: t; -*-
(use-package flow-minor-mode
  :after web-mode
  ;; :mode "\\.js\\'"
  :config
  (add-hook 'web-mode-hook 'flow-minor-enable-automatically))

(use-package add-node-modules-path)
(use-package web-mode
  :mode (".html$" ".css$" ".vue$" ".js$" ".erb$")
  :init
  (add-node-modules-path)
  :custom
  (web-mode-script-padding 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  (web-mode-block-padding 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  :hook (web-mode . (lambda ()
                      (if (equal web-mode-content-type "javascript")
                          (web-mode-set-content-type "jsx")
                        (message "now set to: %s" web-mode-content-type)))))
(use-package emmet-mode
  :after web-mode
  :hook (web-mode . emmet-mode))

;; Javascript
;; TODO: Figure out how to make js2 work well with jsx files
;; (use-package js2-mode
;;   :mode "\\.jsx?\\'"
;;   :config
;;   (add-hook 'js-mode-hook 'js2-minor-mode)
;;   (add-hook 'js-mode-hook 'flycheck-mode)
;;   (setq js2-basic-offset 2)
;;   (setq js2-mode-show-parse-errors -1)
;;   )
;; (use-package flycheck-flow
;;   :mode "\\.jsx?\\'")
;; (use-package company-flow
;;   :mode "\\.jsx?\\'"
;;   :config
;;   (add-to-list 'company-backends 'company-flow))


(defun get-eslint-executable ()
  (let ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "package.json")))
    (and root
         (expand-file-name "node_modules/eslint/bin/eslint.js"
                           root))))

(defun my/use-eslint-from-node-modules ()
  (let ((eslint (get-eslint-executable)))
    (when (and eslint (file-executable-p eslint))
      (setq flycheck-javascript-eslint-executable eslint))))

(defun get-flow-executable ()
  (let ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "package.json")))
    (and root
         (expand-file-name "node_modules/flow-bin/cli.js"
                           root))))

(defun my/use-flow-from-node-modules ()
  (let ((flow (get-flow-executable)))
    (when (and flow (file-exists-p flow))
      (setq flycheck-javascript-flow-executable flow))))


;; Prettier
(use-package prettier-js
  :after web-mode)

;; (defun enable-minor-mode (my-pair)
;;   (if (buffer-file-name)
;;     (if (string-match (car my-pair) buffer-file-name)
;;       (funcall (cdr my-pair)))))

;; (use-package prettier-js
;;   :init
;;   (add-hook 'web-mode-hook #'(lambda () (enable-minor-mode '("\\.jsx?\\'" . prettier-js-mode)))))


;; Infra
(use-package terraform-mode
  :mode "\\.tf\\'")
