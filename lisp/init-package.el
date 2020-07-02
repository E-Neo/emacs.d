;;; init-package.el --- Package settings
;;; Commentary:
;;; Code:

(require 'package)


(setq package-archives
      '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
        ("gnu" . "https://mirrors.163.com/elpa/gnu/")
        ("org" . "https://mirrors.163.com/elpa/org/")))
(package-initialize)


(if (package-installed-p 'use-package)
    (eval-when-compile (require 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)


(provide 'init-package)

;;; init-package.el ends here
