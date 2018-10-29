;;; init-edit.el --- Basic editor settings
;;; Commentary:
;;; Code:


(setq-default
 column-number-mode t
 make-backup-files nil
 mouse-yank-at-point t
 scroll-preserve-screen-position 'always
 show-trailing-whitespace t)


(fset 'yes-or-no-p 'y-or-n-p)


;;; Session settings.
(desktop-save-mode 1)


;;; Set font size
(set-face-attribute 'default nil :height 150)


;;; Settings about parenthesis.
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'show-paren-mode)


;;; Sometimes we don't show trailing whitespace.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
(add-hook hook (lambda () (setq show-trailing-whitespace nil))))


;;; Clean up whitespace.
(use-package whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode))


;;; Settings about auto complete.
(use-package company
  :hook (after-init . global-company-mode))


;;; Settings about syntax checking.
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :init
  (use-package flycheck-color-mode-line
    :init
    (eval-after-load "flycheck"
      '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))))


;;; Minibuffer completion.
(use-package ivy
  :config
  (ivy-mode 1))


(provide 'init-edit)


;;; init-edit.el ends here
