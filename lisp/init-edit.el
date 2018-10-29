;;; init-edit.el --- Basic editor settings
;;; Commentary:
;;; Code:


(setq-default
 column-number-mode t
 make-backup-files nil
 mouse-yank-at-point t
 scroll-preserve-screen-position 'always
 show-trailing-whitespace t)


;;; GUI settings.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))


;;; Use y-or-n.
(fset 'yes-or-no-p 'y-or-n-p)


;;; Session settings.
(desktop-save-mode 1)
(savehist-mode 1)


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
  :hook (after-init . global-company-mode)
  :config
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode)))


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
  :init
  (use-package amx)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (use-package counsel
    :config
    (global-set-key (kbd "C-s") 'swiper)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)))


(provide 'init-edit)


;;; init-edit.el ends here
