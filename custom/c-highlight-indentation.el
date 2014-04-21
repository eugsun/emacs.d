(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(add-hook 'enh-ruby-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))
(add-hook 'coffee-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))
