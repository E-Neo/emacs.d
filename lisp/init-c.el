;;; init-c.el --- C/C++ settings
;;; Commentary:
;;; Code:

(use-package irony
  :hook ((c++-mode . irony-mode)
	 (c-mode . irony-mode)
	 (objc-mode . irony-mode)
	 (irony-mode . irony-cdb-autosetup-compile-options))
  :config
  (use-package company-irony
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))
  (use-package flycheck-irony
    :config
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  (use-package irony-eldoc
    :hook (irony-mode . irony-eldoc)))

(use-package clang-format
  :config
  (setq-default clang-format-style "Google")
  (defun my-before-save ()
    (when (eq major-mode 'c++-mode)
      (clang-format-buffer)))
  :hook
  (before-save . my-before-save))

(provide 'init-c)

;;; init-c.el ends here
