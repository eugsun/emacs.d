;; Theme
(use-package doom-themes
  :ensure t)
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; -- Automatically switch between ligh and dark theme based on time of day
(setq theme-autoswitch t)
(if (and theme-autoswitch (display-graphic-p))
    (progn
      (setq current-theme 'doom-peacock)
      (defun sync-theme-with-time ()
        (setq hour (string-to-number (substring (current-time-string) 11 13)))
        (if (member hour (number-sequence 6 14))
            (setq now 'doom-nord-light)
          (setq now 'doom-peacock))
        (if (or (not (boundp 'current-theme)) (eq now current-theme))
            nil
          (setq current-theme now))
        (load-theme now t)
        )
      (run-with-timer 0 3600 #'sync-theme-with-time)
      )
  (load-theme 'doom-peacock t)
  )

;; Typeface
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
;; -- Default
(when (member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil :font "Source Code Pro"))
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata"))
(when (member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :font "Consolas"))
;; -- Unicode
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend))
;; -- Chinese
(when (member "WenQuanYi Micro Hei" (font-family-list))
  (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei" ))

(font-family-list)
