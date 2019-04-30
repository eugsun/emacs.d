(add-hook 'enh-ruby-mode-hook 'robe-mode)

(require 'rvm)
(rvm-use-default)

(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . enh-ruby-mode))
