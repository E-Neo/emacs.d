;;; init-package.el --- Package settings
;;; Commentary:
;;; Code:

(require 'package)

;; (setq package-check-signature nil)
(setq package-archives
      '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
(package-initialize)


(if (package-installed-p 'use-package)
    (eval-when-compile (require 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)


(use-package elpa-mirror)


(provide 'init-package)

;;; init-package.el ends here
