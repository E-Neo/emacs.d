;;; init-package.el --- Package settings
;;; Commentary:
;;; Code:


(require 'package)


(setq package-archives '(("melpa" . "https://elpa.emacs-china.org/melpa/")
			 ("gnu" . "https://elpa.emacs-china.org/gnu/")
			 ("org" . "https://elpa.emacs-china.org/org/")))
(package-initialize)


(if (package-installed-p 'use-package)
    (eval-when-compile (require 'use-package))
  (package-install 'use-package))
(setq use-package-always-ensure t)


(provide 'init-package)


;;; init-package.el ends here
