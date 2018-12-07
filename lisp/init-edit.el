;;; init-edit.el --- Basic editor settings
;;; Commentary:
;;; Code:


(setq-default
 dired-dwim-target t
 column-number-mode t
 make-backup-files nil
 mouse-yank-at-point t
 scroll-preserve-screen-position 'always
 show-trailing-whitespace t)

;;; Kill Ring & Undo Tree
(use-package browse-kill-ring)
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;;; Show line numbers.
(add-hook 'prog-mode-hook 'linum-mode)

;;; Imenu
(add-hook 'prog-mode-hook 'imenu-add-menubar-index)

;;; Use solarized-dark theme.
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

;;; GUI settings.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))


;;; Font settings.
(set-face-attribute 'default nil :font "Noto Sans Mono CJK SC" :height 150)


;;; Use y-or-n.
(fset 'yes-or-no-p 'y-or-n-p)

;;; Switch window.
(use-package switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window))

(defun my-split-window-below ()
  "Split the selected window into two windows, one above the other."
  (interactive)
  (set-window-buffer (split-window-below) (other-buffer)))

(defun my-split-window-right ()
  "Split the selected window into two side-by-side windows."
  (interactive)
  (set-window-buffer (split-window-below) (other-buffer)))

(global-set-key (kbd "C-x 2") 'my-split-window-below)
(global-set-key (kbd "C-x 3") 'my-split-window-right)


;;; Session settings.
(desktop-save-mode 1)
(savehist-mode 1)


;;; Settings about parenthesis.
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'show-paren-mode)


;;; Sometimes we don't show trailing white space.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))


;;; Clean up white space.
(use-package whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode))

;;; Spell checking.
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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
