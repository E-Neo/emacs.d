;;; init-coq.el --- Coq
;;; Commentary:
;;; Code:

(use-package proof-general
  :config
  (custom-set-variables '(coq-prog-name "~/.opam/default/bin/coqtop"))
  (use-package company-coq
    :hook ((coq-mode . company-coq-mode))))


(use-package flycheck
  :init (setq flycheck-global-modes '(not coq-mode)))


(provide 'init-coq)

;;; init-coq.el ends here
