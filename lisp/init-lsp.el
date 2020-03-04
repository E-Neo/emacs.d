;;; init-lsp.el --- Emacs lsp
;;; Commentary:
;;; Code:

;;; C++
(use-package google-c-style
  :hook ((c++-mode . google-set-c-style)
         (c++-mode . google-make-newline-indent)))


;;; Go
(use-package go-mode)


;;; Python
(use-package pyvenv
  :config
  (eval-when-compile
    (defvar python-shell-interpreter)
    (defvar python-shell-interpreter-args))

  (defun workon (virtualenv)
    "Workon VIRTUALENV."
    (interactive (list (expand-file-name
                        (read-directory-name "workon: "
                                             "~/usr/miniconda3/envs/"))))
    (pyvenv-activate virtualenv)
    (if (file-exists-p (concat virtualenv "bin/ipython"))
        (setq python-shell-interpreter "ipython"
              python-shell-interpreter-args "-i --simple-prompt")
      (setq python-shell-interpreter "python"
            python-shell-interpreter-args "-i")))

  (defun deactivate ()
    "Deactivate virtualenv."
    (interactive)
    (pyvenv-deactivate)
    (if (executable-find "ipython")
        (setq python-shell-interpreter "ipython"
              python-shell-interpreter-args "-i --simple-prompt")
      (setq python-shell-interpreter "python"
            python-shell-interpreter-args "-i"))))


;;; Rust
(use-package rust-mode
  :config
  (setq-default tab-width 4))


(use-package lsp-mode
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  (setq lsp-prefer-flymake nil)
  (use-package lsp-ui
    :config
    (setq-default lsp-ui-flycheck-enable t)
    (setq-default lsp-ui-doc-enable nil)
    (setq-default lsp-ui-sideline-enable nil))
  (use-package company-lsp))

(provide 'init-lsp)

;;; init-lsp.el ends here
