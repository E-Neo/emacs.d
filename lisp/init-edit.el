;;; init-edit.el --- Basic editor settings
;;; Commentary:
;;; Code:

(setq-default
 indent-tabs-mode nil
 dired-dwim-target t
 column-number-mode t
 confirm-kill-emacs 'yes-or-no-p
 make-backup-files nil
 mouse-yank-at-point t
 scroll-preserve-screen-position 'always
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil)


(add-to-list 'auto-mode-alist '("\\.m\\'" . mercury-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))


(prefer-coding-system 'utf-8-unix)


(use-package async)


(use-package delight)


(use-package eldoc
  :delight)


;;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (setq-default exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;; gpg
(setq-default epa-pinentry-mode 'loopback)


;;; Indent
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq-default highlight-indent-guides-method 'column))


;;; Whitespace
(use-package whitespace
  :delight
  :hook ((text-mode . ws-text-hook)
         (prog-mode . ws-prog-mode-hook)
         (go-mode . ws-go-mode-hook)
         (rust-mode . ws-rust-mode-hook))
  :config
  (defun ws-text-hook ()
    (whitespace-mode 0)
    (setq whitespace-style '(face tabs trailing))
    (whitespace-mode 1))
  (defun ws-prog-mode-hook ()
    (whitespace-mode 0)
    (setq whitespace-style '(face tabs lines-tail trailing))
    (set-face-attribute 'whitespace-line nil
                        :background (face-attribute 'default :foreground))
    (whitespace-mode 1))
  (defun ws-go-mode-hook ()
    (whitespace-mode 0)
    (setq whitespace-style '(face trailing))
    (whitespace-mode 1))
  (defun ws-rust-mode-hook ()
    (whitespace-mode 0)
    (setq-local whitespace-line-column 100)
    (whitespace-mode 1)))


(use-package which-func
  :config
  (which-function-mode t))

;;; Auto revert.
(add-hook 'after-init-hook 'global-auto-revert-mode)


;;; Kill Ring & Undo Tree
(use-package browse-kill-ring)
(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode)
  (setq-default undo-tree-auto-save-history nil))


;;; Show line numbers.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


;;; Imenu
(add-hook 'prog-mode-hook
          `(lambda ()
             (condition-case nil (imenu-add-menubar-index) (error nil))))


(use-package windresize)


(use-package nerd-icons-dired
  :hook
  ;; Need "Symbols Nerd Font"
  (dired-mode . nerd-icons-dired-mode))


(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (use-package doom-modeline
    :hook
    (after-init . doom-modeline-mode)
    (after-init . display-time-mode)
    :init
    (setq display-time-24hr-format t)
    (setq display-time-default-load-average nil)
    :config
    (doom-modeline-def-modeline 'myline
      '(bar workspace-name window-number modals matches buffer-info remote-host
            buffer-position word-count parrot selection-info)
      '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug
                    repl lsp minor-modes input-method indent-info
                    buffer-encoding major-mode process vcs check time hud))
    (defun setup-custom-modeline ()
      (doom-modeline-set-modeline 'myline 'default))
    (add-hook 'doom-modeline-mode-hook 'setup-custom-modeline)
    (add-hook 'find-file-hook 'setup-custom-modeline))

  (defun dark ()
    "Load dark theme."
    (interactive)
    (let ((height (face-attribute 'default :height)))
      (load-theme 'doom-Iosvkem t)
      (set-face-attribute 'default nil :height height)))

  (defun light ()
    "Load light theme."
    (interactive)
    (let ((height (face-attribute 'default :height)))
      (load-theme 'doom-one-light t)
      (set-face-attribute 'default nil :height height)))

  (dark))


;;; GUI settings.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
(setq scroll-conservatively 10000)


;;; Font settings.
;; default font
(when (member "Hack Nerd Font Mono" (font-family-list))
  (set-frame-font "Hack Nerd Font Mono" t t))
;; set font size
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

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


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


;;; Tree-sitter
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))


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


;;; LaTeX fragments.
(use-package texfrag)


;;; Settings about auto complete.
(use-package company
  :delight
  :hook ((after-init . global-company-mode))
  :config
  (setq-default company-idle-delay 0.1)
  (setq-default company-minimum-prefix-length 2)
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode)))


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


;;; Input methods.
(with-temp-buffer
  (activate-input-method "TeX")
  (let ((quail-current-package (assoc "TeX" quail-package-alist)))
    (quail-define-rules ((append . t))
                        ("\\land" ?∧)
                        ("\\lor" ?∨))))


;;; Show file path.
(defun path ()
  "Show file path of the current buffer."
  (interactive)
  (message buffer-file-name))


(provide 'init-edit)

;;; init-edit.el ends here
