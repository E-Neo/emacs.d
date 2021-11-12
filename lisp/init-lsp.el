;;; init-lsp.el --- Emacs lsp
;;; Commentary:
;;; Code:

;;; Cypher
(use-package cypher-mode)


;;; Dockerfile
(use-package dockerfile-mode)


;;; Erlang
(use-package erlang)


;;; Go
(use-package go-mode)


;;; Lua
(use-package lua-mode)


;;; Markdown
(use-package markdown-mode
  :config
  (use-package grip-mode
    :bind (:map markdown-mode-command-map
                ("g" . grip-mode))))


;;; OCaml
(use-package tuareg
  :config
  (use-package utop))

;;; Python
(use-package lsp-pyright
  :config
  (use-package py-autopep8
    :hook (python-mode . py-autopep8-enable-on-save)))

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
  :delight "🦀"
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
         (tuareg-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq-default lsp-disabled-clients '((lua-mode . emmy-lua)))
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-render-documentation nil)
  (use-package lsp-ui
    :config
    (setq-default lsp-ui-sideline-enable nil)
    (define-key lsp-ui-mode-map
      [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map
      [remap xref-find-references] #'lsp-ui-peek-find-references))
  (use-package lsp-ivy)
  (use-package lsp-treemacs
    :init
    (lsp-treemacs-sync-mode 1)))


(use-package dap-mode
  :config
  (require 'dap-cpptools))


(provide 'init-lsp)

;;; init-lsp.el ends here
