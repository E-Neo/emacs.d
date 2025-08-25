;;; init-lsp.el --- Emacs lsp
;;; Commentary:
;;; Code:


;;; Clojure
(use-package clojure-mode
  :defer t
  :config
  (use-package cider
    :defer t))


;;; Cypher
(use-package cypher-mode
  :defer t)


;;; Dockerfile
(use-package dockerfile-mode
  :defer t)


;;; Erlang
(use-package erlang
  :defer t)


;;; Go
(use-package go-mode
  :defer t)


;;; Guile
(use-package geiser-guile
  :defer t)


;;; HTML
(use-package rainbow-mode
  :defer t
  :hook ((css-mode . rainbow-mode)
         (html-mode . rainbow-mode)))


;;; Lua
(use-package lua-mode
  :defer t)


;;; Markdown
(use-package markdown-mode
  :defer t
  :config
  (use-package grip-mode
    :defer t
    :bind (:map markdown-mode-command-map
                ("g" . grip-mode))))


;;; Maxima
(use-package maxima
  :defer t
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :config
  (use-package company-maxima
    :defer t
    :hook ((maxima-mode . maxima-hook-function)
           (maxima-inferior-mode . maxima-hook-function))))


;;; Nix
(use-package nix-mode
  :defer t)


;;; OCaml
(use-package tuareg
  :defer t
  :config
  (use-package utop
    :defer t))


;;; Proof General
(use-package proof-general
  :defer t
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
(use-package racket-mode
  :defer t)


;;; Rust
(use-package rust-mode
  :defer t
  :delight "🦀"
  :hook
  (rust-mode . my-line-column-120)
  (rust-ts-mode . my-line-column-120)
  :config
  (defun my-line-column-120 ()
    (setq-local whitespace-line-column 120))
  (setq-default tab-width 4)
  (setq-default rust-format-on-save t))


;;; TypeScript
(use-package typescript-mode
  :defer t)


;;; Various modes.
(use-package yaml-mode
  :defer t)


(use-package eglot
  :defer t
  :hook ((c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (clojure-mode . eglot-ensure)
         (erlang-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (lua-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (tuareg-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (before-save . my-eglot-before-save))
  :bind (("C-c l a a" . eglot-code-actions)
         ("C-c l r r" . eglot-rename)
         ("C-c l r o" . eglot-code-action-organize-imports)
         ("C-c l = r" . eglot-format)
         ("C-c l = =" . eglot-format-buffer)
         ("C-c l w r" . eglot-reconnect))
  :config
  (defun my-eglot-before-save ()
    (add-hook 'before-save-hook 'eglot-format-buffer nil t)))


(provide 'init-lsp)

;;; init-lsp.el ends here
