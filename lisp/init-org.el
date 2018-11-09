;;; init-org.el --- Org-mode settings
;;; Commentary:
;;; Code:


(require 'ox-latex)


(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("latexmk -pdflatex='xelatex -8bit -shell-escape -interaction nonstopmode' -pdf -f %f && latexmk -bibtex -c"))
(setq-default org-format-latex-options
	      (plist-put org-format-latex-options :scale 2.0))


;;; babel settings.
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))


(provide 'init-org)


;;; init-org.el ends here
