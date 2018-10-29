;;; init-org.el --- Org-mode settings
;;; Commentary:
;;; Code:


(require 'ox-latex)


(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("latexmk -pdflatex='xelatex -shell-escape -interaction nonstopmode' -pdf -f %f && latexmk -bibtex -c"))
(setq-default org-format-latex-options
	      (cdr '(_ :scale 2)))


(provide 'init-org)


;;; init-org.el ends here
