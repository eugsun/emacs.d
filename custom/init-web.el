;; -*- lexical-binding: t; -*-
;; -- Util
(defun get-eslint-executable ()
  (let ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules/")))
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
                "node_modules/")))
    (and root
         (expand-file-name "node_modules/flow-bin/cli.js"
                           root))))

(defun my/use-flow-from-node-modules ()
  (let ((flow (get-flow-executable)))
    (when (and flow (file-exists-p flow))
      (setq flycheck-javascript-flow-executable flow))))

;; -- Config
(use-package flow-minor-mode
  :after web-mode
  ;; :mode "\\.js\\'"
  :config
  (add-hook 'web-mode-hook 'flow-minor-enable-automatically))

(use-package add-node-modules-path)
(use-package web-mode
  :mode (".html$" ".css$" ".vue$" ".js$" ".erb$" ".tsx$")
  :init
  (add-node-modules-path)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
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
  :hook
  (web-mode . (lambda ()
                (if (equal web-mode-content-type "javascript")
                    (web-mode-set-content-type "jsx")
                  (message "now set to: %s" web-mode-content-type))))
  (web-mode . flycheck-mode)
  (web-mode . my/use-eslint-from-node-modules)
  (web-mode . my/use-flow-from-node-modules)
  )
(use-package emmet-mode
  :after web-mode
  :hook (web-mode . emmet-mode))

;; Javascript
(use-package tide :ensure t)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)


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


;; Prettier
(use-package prettier
  :after web-mode
  :init
  (add-hook 'web-mode-hook 'prettier-mode))

;; (defun enable-minor-mode (my-pair)
;;   (if (buffer-file-name)
;;     (if (string-match (car my-pair) buffer-file-name)
;;       (funcall (cdr my-pair)))))


;; Infra
(use-package terraform-mode
  :mode "\\.tf\\'")
