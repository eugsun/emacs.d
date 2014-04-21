(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; set xetex as the engine
(setq TeX-engine 'xetex)

;; set up reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; make pdflatex default
(setq TeX-PDF-mode t)

;; set up auto-complete for auctux
(require 'auto-complete-auctex)

;; add PDFLatex to the command-list
(add-hook 'LaTeX-mode-hook 'pdflatex-mode-hook)
(defun pdflatex-mode-hook ()
  (add-to-list 'TeX-command-list
               '("PDFLaTeX" "%'pdflatex%(mode)%' %t" TeX-run-TeX nil t)))

;; add Make to the command-list
(add-hook 'LaTeX-mode-hook 'make-mode-hook)
(defun make-mode-hook ()
  (add-to-list 'TeX-command-list
               '("Make" "%'make all%'" TeX-run-TeX nil t)))

;; set evince to be the default view program
(custom-set-variables
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open")))))
