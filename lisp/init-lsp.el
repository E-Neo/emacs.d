;;; init-lsp.el --- Emacs lsp
;;; Commentary:
;;; Code:


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


;;; Guile
(use-package geiser-guile)


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


;;; Maxima
(use-package maxima
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :config
  (use-package company-maxima
    :hook ((maxima-mode . maxima-hook-function)
           (maxima-inferior-mode . maxima-hook-function))))


;;; Nix
(use-package nix-mode)


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


;;; Racket
(use-package racket-mode)


;;; Rust
(use-package rust-mode
  :delight "🦀"
  :config
  (setq-default tab-width 4)
  (setq-default rust-format-on-save t))


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
         ("C-c l = =" . eglot-format-buffer)
         ("C-c l w r" . eglot-reconnect)))


(provide 'init-lsp)

;;; init-lsp.el ends here
