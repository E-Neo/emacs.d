;;; init-apps.el --- Add more fun
;;; Commentary:
;;; Code:

(require 'dired-aux)

(add-to-list 'dired-compress-files-alist
             '("\\.tar\\.lz4\\'" . "tar -cf - %i | lz4 - > %o"))


(use-package emms)
(use-package vterm)

(provide 'init-apps)

;;; init-apps.el ends here
