;;; init-apps.el --- Add more fun
;;; Commentary:
;;; Code:

(require 'dired-aux)

(add-to-list 'dired-compress-files-alist
             '("\\.tar\\.lz4\\'" . "tar -cf - %i | lz4 - > %o"))


(use-package emms
  :config
  (emms-standard)
  (emms-default-players))


(use-package leetcode
  :config
  (setq-default leetcode-prefer-language "cpp"))


(provide 'init-apps)

;;; init-apps.el ends here
