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
    :after company
    :config
    (add-to-list 'company-backends 'company-irony))
  (use-package flycheck-irony
    :hook (flycheck-mode . flycheck-irony-setup))
  (use-package irony-eldoc
    :hook (irony-mode . irony-eldoc)))

(use-package google-c-style
  :hook ((c++-mode . google-set-c-style)
         (c++-mode . google-make-newline-indent)))

(provide 'init-c)

;;; init-c.el ends here
