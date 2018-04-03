;;; init-local.el -- local config
;;; Commentary:
;;; Code:

;; C/C++
(when (maybe-require-package 'irony)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (when (maybe-require-package 'company-irony)
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))
  (when (maybe-require-package 'flycheck-irony)
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  (when (maybe-require-package 'irony-eldoc)
    (add-hook 'irony-mode-hook #'irony-eldoc)))
(when (maybe-require-package 'google-c-style)
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-make-newline-indent))

;; Python
(defun workon (virtualenv)
  "Workon VIRTUALENV."
  (interactive
   (list (read-directory-name "workon: " "~/.virtualenvs/")))
  (pythonic-activate virtualenv))

;; Racket
(maybe-require-package 'racket-mode)

;; Emms
(when (maybe-require-package 'emms)
  (require 'emms-setup)
  (emms-all)
  (emms-default-players))

;; Org-mode
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(provide 'init-local)
;;; init-local.el ends here
