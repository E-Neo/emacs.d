;;; init.el --- Emacs init file
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(require 'init-package)

(require 'init-edit)
(require 'init-org)
(require 'init-git)
(require 'init-apps)
(require 'init-lsp)
(require 'init-ai)

;;; Change custom file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defun display-startup-echo-area-message ()
  (message "init-time %s" (emacs-init-time)))

(provide 'init)

;;; init.el ends here
