;; loading ibus.el
(require 'ibus)

;; Turn on ibus-mode automatically after loading .emacs
(add-hook 'after-init-hook 'ibus-mode-on)

;; Make sure backspace works during typing
(ibus-define-common-key [backspace] t)

;; Change cursor color depending on IBus status
;(setq ibus-cursor-color '("red" "blue" "limegreen"))
;; Use C-SPC for Set Mark command
;(ibus-define-common-key ?\C-\s nil)
;; Use C-/ for Undo command
;(ibus-define-common-key ?\C-/ nil)
