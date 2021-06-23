;;; init-package.el --- Package settings
;;; Commentary:
;;; Code:

(require 'package)


(setq package-archives
      '(("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/melpa-stable/")))
(package-initialize)


(if (package-installed-p 'use-package)
    (eval-when-compile (require 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)


(provide 'init-package)

;;; init-package.el ends here
