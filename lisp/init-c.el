;;; init-c.el --- C/C++ settings
;;; Commentary:
;;; Code:

;; CMake
(use-package cmake-mode)

;; Style:
(defun my-c-mode-hook ()
  "My `c-mode' hook."
  (c-set-style "gnu")
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'brace-list-intro '+))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; Flex & Bison
(define-derived-mode flex-bison-mode c-mode "Flex/Bison")
(add-to-list 'auto-mode-alist '("\\.y\\'" . flex-bison-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-bison-mode))

;; Cuda:
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

(use-package irony
  :hook ((c++-mode . my-irony-mode-on)
	 (c-mode . my-irony-mode-on)
	 (objc-mode . my-irony-mode-on)
	 (irony-mode . irony-cdb-autosetup-compile-options))
  :config
  (defun my-irony-mode-on ()
    (when (member major-mode irony-supported-major-modes)
      (irony-mode)))
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
