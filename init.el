;;; init.el --- Emacs init file
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


(require 'init-package)

(require 'init-edit)
(require 'init-org)
(require 'init-git)

(require 'init-c)
(require 'init-python)
(require 'init-haskell)
(require 'init-ocaml)
(require 'init-antlr)
(require 'init-database)

(require 'init-apps)


;;; Change custom file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)


;;; init.el ends here
