;;; init-package.el --- Package settings
;;; Commentary:
;;; Code:

(require 'package)


(setq package-archives
      '(("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/melpa-stable/")
        ("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa" . 0)))
(package-initialize)


(if (package-installed-p 'use-package)
    (eval-when-compile (require 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)


(provide 'init-package)

;;; init-package.el ends here
