;;; init-edit.el --- Basic editor settings
;;; Commentary:
;;; Code:

(setq-default
 indent-tabs-mode nil
 dired-dwim-target t
 column-number-mode t
 make-backup-files nil
 mouse-yank-at-point t
 scroll-preserve-screen-position 'always
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil)

(prefer-coding-system 'utf-8-unix)

(use-package delight)


(use-package eldoc
  :delight)


;;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;; gpg
(setq-default epa-pinentry-mode 'loopback)


;;; Whitespace
(use-package whitespace
  :delight
  :hook ((text-mode . ws-text-hook)
         (prog-mode . ws-prog-mode-hook)
         (go-mode . ws-go-mode-hook)
         (rust-mode . ws-rust-mode-hook))
  :config
  (defun ws-text-hook ()
    (setq whitespace-style '(face tabs trailing))
    (set-face-attribute 'whitespace-trailing nil :foreground "#FFF")
    (whitespace-mode t))
  (defun ws-prog-mode-hook ()
    (whitespace-mode 0)
    (setq whitespace-style '(face tabs lines-tail trailing))
    (set-face-attribute 'whitespace-line nil :background "#FFF")
    (set-face-attribute 'whitespace-trailing nil :foreground "#FFF")
    (whitespace-mode 1))
  (defun ws-go-mode-hook ()
    (whitespace-mode 0)
    (setq whitespace-style '(face trailing))
    (whitespace-mode 1))
  (defun ws-rust-mode-hook ()
    (whitespace-mode 0)
    (setq-local whitespace-line-column 100)
    (whitespace-mode 1)))


;;; Auto revert.
(add-hook 'after-init-hook 'global-auto-revert-mode)


;;; Kill Ring & Undo Tree
(use-package browse-kill-ring)
(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))


;;; Show line numbers.
(add-hook 'prog-mode-hook 'linum-mode)


;;; Imenu
(add-hook 'prog-mode-hook
          `(lambda ()
             (condition-case nil (imenu-add-menubar-index) (error nil))))


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
(when (find-font (font-spec :name "Noto Sans Mono CJK SC"))
  (set-face-attribute 'default nil :font "Noto Sans Mono CJK SC"))
(set-face-attribute 'default nil :height 150)

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
  (set-window-buffer (split-window-right) (other-buffer)))

(global-set-key (kbd "C-x 2") 'my-split-window-below)
(global-set-key (kbd "C-x 3") 'my-split-window-right)


;;; Session settings.
(desktop-save-mode 1)
(savehist-mode 1)


;;; Highlight escape sequences
(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))


;;; Prettify symbols.
(add-hook 'after-init-hook 'global-prettify-symbols-mode)


;;; Settings about parenthesis.
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'show-paren-mode)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


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
  :delight
  :hook (after-init . global-whitespace-cleanup-mode))


;;; Spell checking.
(use-package flyspell
  :delight
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (setq-default ispell-program-name "hunspell"))


;;; Snippet.
(use-package yasnippet
  :delight yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (latex-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (declare-function yas-reload-all "yasnippet.el")
  (yas-reload-all))


;;; Settings about auto complete.
(use-package company
  :delight
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode)))


;;; Settings about syntax checking.
(use-package flycheck
  :delight
  :hook (after-init . global-flycheck-mode)
  :init
  (use-package flycheck-color-mode-line
    :hook (flycheck-mode . flycheck-color-mode-line-mode)))


;;; Minibuffer completion.
(use-package ivy
  :delight
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
