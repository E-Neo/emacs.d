;;; init-lsp.el --- Emacs lsp
;;; Commentary:
;;; Code:

;;; C++
(use-package google-c-style
  :hook ((c++-mode . google-set-c-style)
         (c++-mode . google-make-newline-indent)))


;;; Erlang
(use-package erlang)


;;; Go
(use-package go-mode)


;;; Lua
(use-package lua-mode)


;;; Python
(use-package conda
  :functions try-to-use-ipython
  :config
  (setq-default
   conda-anaconda-home (expand-file-name "~/usr/miniconda3/")
   conda-env-home-directory (expand-file-name "~/usr/miniconda3/"))

  (defun try-to-use-ipython ()
    "Try to use ipython if installed."
    (if (executable-find
         (if (eq system-type 'windows-nt) "ipython.exe" "ipython"))
        (setq-default python-shell-interpreter "ipython"
                      python-shell-interpreter-args "-i --simple-prompt")
      (setq-default python-shell-interpreter "python"
                    python-shell-interpreter-args "-i")))

  (defun workon ()
    "Workon env."
    (interactive)
    (conda-env-activate)
    (try-to-use-ipython))

  (defun deactivate ()
    "Deactivate env."
    (interactive)
    (conda-env-deactivate)
    (try-to-use-ipython)))


;;; Rust
(use-package rust-mode
  :config
  (setq-default tab-width 4)
  (setq-default lsp-rust-server 'rust-analyzer))


;;; Various modes.
(use-package yaml-mode)


(use-package lsp-mode
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (erlang-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  (setq-default lsp-disabled-clients '((lua-mode . emmy-lua)))
  (setq lsp-signature-render-documentation nil)
  (use-package lsp-ui
    :config
    (setq-default lsp-ui-sideline-enable nil))
  (use-package company-lsp)
  (use-package lsp-ivy))

(provide 'init-lsp)

;;; init-lsp.el ends here
