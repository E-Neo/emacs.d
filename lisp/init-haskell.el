;;; init-haskell.el --- Haskell settings
;;; Commentary:
;;; Code:


(use-package haskell-mode
  :init
  (use-package intero
    :hook (haskell-mode . intero-mode)
    :config
    (with-eval-after-load 'intero
      (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))))


(defcustom intero-extra-ghc-options '("-Wall")
  "Turn on all warnings."
  :group 'intero
  :type '(string))

(defcustom intero-extra-ghci-options '("-fno-defer-out-of-scope-variables")
  "Fix panic."
  :group 'intero
  :type '(string))

(provide 'init-haskell)


;;; init-haskell.el ends here
