;;; init-python.el --- Python settings
;;; Commentary:
;;; Code:


(setq-default
 flycheck-python-flake8-executable "flake8")

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode))
  :init
  (use-package company-anaconda
    :init
    (eval-after-load "company"
      '(add-to-list 'company-backends 'company-anaconda))))


(use-package cython-mode
  :init
  (use-package flycheck-cython))


(defun workon (virtualenv)
  "Workon VIRTUALENV."
  (interactive (list (expand-file-name
		      (read-directory-name "workon: " "~/usr/miniconda3/envs/"))))
  (pythonic-activate virtualenv)
  (if (file-exists-p (concat virtualenv "bin/ipython"))
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt")
    (setq python-shell-interpreter "python"
          python-shell-interpreter-args "-i")))

(defun deactivate ()
  "Deactivate virtualenv."
  (interactive)
  (pythonic-deactivate)
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt")
    (setq python-shell-interpreter "python"
          python-shell-interpreter-args "-i")))


(provide 'init-python)

;;; init-python.el ends here
