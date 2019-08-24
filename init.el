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

;;; Change custom file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)

;;; init.el ends here
