;;; init-coq.el --- Coq
;;; Commentary:
;;; Code:

(use-package proof-general
  :config
  (customize-set-variable 'coq-compile-before-require t)
  (customize-set-variable 'proof-three-window-mode-policy 'hybrid)
  (use-package company-coq
    :hook ((coq-mode . company-coq-mode))))


(use-package flycheck
  :init (setq flycheck-global-modes '(not coq-mode)))


(provide 'init-coq)

;;; init-coq.el ends here
