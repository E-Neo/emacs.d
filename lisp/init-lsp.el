;;; init-lsp.el --- Emacs lsp
;;; Commentary:
;;; Code:


(use-package tree-sitter
  :config
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;;; Clojure
(use-package clojure-mode
  :config
  (use-package cider))

;;; Cypher
(use-package cypher-mode)


;;; Dockerfile
(use-package dockerfile-mode)


;;; Erlang
(use-package erlang)


;;; Go
(use-package go-mode)


;;; HTML
(use-package rainbow-mode
  :hook ((css-mode . rainbow-mode)
         (html-mode . rainbow-mode)))


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


;;; Proof General
(use-package proof-general
  :hook
  (coq-goals-mode . (lambda ()
                      (setq prettify-symbols-alist
                            '(("forall" . ?∀)
                              ("exists" . ?∃)
                              ("~" . ?¬)
                              ("/\\" . ?∧)
                              ("\\/" . ?∨)
                              ("=>" . ?⇒)
                              ("->" . ?→)
                              ("<-" . ?←)
                              ("<->" . ?↔)
                              ("nat" . ?ℕ)
                              ("*" . ?×)))
                      (prettify-symbols-mode 1))))


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


;;; Racket
(use-package racket-mode)


;;; Rust
(use-package rust-mode
  :delight "🦀"
  :config
  (setq-default tab-width 4)
  (setq-default lsp-rust-server 'rust-analyzer))


;;; TypeScript
(use-package typescript-mode)


;;; Various modes.
(use-package yaml-mode)


(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (clojure-mode . eglot-ensure)
         (erlang-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (lua-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (tuareg-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (before-save . eglot-format-buffer)
         (before-save . eglot-code-action-organize-imports))
  :bind (("C-c l r r" . eglot-rename)
         ("C-c l r o" . eglot-code-action-organize-imports)
         ("C-c l = r" . eglot-format)
         ("C-c l = =" . eglot-format-buffer)))


(use-package dap-mode
  :config
  (require 'dap-cpptools))


(provide 'init-lsp)

;;; init-lsp.el ends here
