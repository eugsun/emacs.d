(setq-default js3-indent-level 2)

(defvar *quasi-js-current-pos* nil)

(defun quasi-js-function-search ()
  "Search for JS function definations. In a rather dumb way, but works, albeit
only for current buffer. Works recurcively too :)"
  (interactive)
  (let ((text (thing-at-point 'word)))
    (push (point) *quasi-js-current-pos*)
    (goto-char (point-min))
    (if (search-forward (concat "function " text) nil t)
        (recenter)
      (progn
        (goto-char (pop *quasi-js-current-pos*))
        (message "Could not find definition for %s" text)))))

(defun quasi-js-function-go-back ()
  "Go back to where you initiated search from"
  (interactive)
  (if *quasi-js-current-pos*
      (goto-char (pop *quasi-js-current-pos*))
    (message "Nowhere to jump!")))

;; Add hooks to js3-mode. It will cobbler the default tag-search bindings. Beware.
(add-hook 'js3-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") #'quasi-js-function-search)
            (local-set-key (kbd "M-,") #'quasi-js-function-go-back)))
