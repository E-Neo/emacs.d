;;; init-org.el --- Org-mode settings
;;; Commentary:
;;; Code:


(require 'ox-latex)


(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("latexmk \
-pdflatex='xelatex -8bit -shell-escape -interaction nonstopmode' \
-pdf -f %f && latexmk -bibtex -c && rm -rf ltximg _minted-%b %b.tex"))
(setq-default org-format-latex-options
	      (plist-put org-format-latex-options :scale 2.0))

;;; The following code fixes org preview foreground color always black problem.
(let ((dvipng (alist-get 'dvipng org-preview-latex-process-alist)))
  (add-to-list 'org-preview-latex-process-alist
	       (cons 'dvipng (plist-put dvipng :latex-header "\\documentclass{article}
\\usepackage{amsmath}
\\pagestyle{empty}"))))

;;; babel settings.
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))


(provide 'init-org)


;;; init-org.el ends here
