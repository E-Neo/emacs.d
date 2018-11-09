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
      '(add-to-list 'company-backends 'company-anaconda)))
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "-i --simple-prompt")))


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
