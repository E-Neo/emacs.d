;;; init-package.el --- Package settings
;;; Commentary:
;;; Code:

(require 'package)


(add-to-list 'package-archives
             '( "melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/") t)
(package-initialize)


(if (package-installed-p 'use-package)
    (eval-when-compile (require 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)


(provide 'init-package)

;;; init-package.el ends here
