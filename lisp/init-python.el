;;; Package --- Python settings
;;; Commentary:
;;; Code:
(require 'rx)


(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode))
  :init
  (use-package company-anaconda
    :init
    (eval-after-load "company"
      '(add-to-list 'company-backends 'company-anaconda))))
;; (when (maybe-require-package 'anaconda-mode)
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;   (when (maybe-require-package 'company-anaconda)
;;     (eval-after-load "company"
;;       '(add-to-list 'company-backends 'company-anaconda))))


(defun workon (virtualenv)
  "Workon VIRTUALENV."
  (interactive (list (expand-file-name
		      (read-directory-name "workon: " "~/.virtualenvs/"))))
  (pythonic-activate virtualenv))


(defun deactivate ()
  "Deactivate virtualenv."
  (interactive)
  (pythonic-deactivate))


(provide 'init-python)


;;; init-python.el ends here
