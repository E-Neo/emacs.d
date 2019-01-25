;;; init-apps.el --- Add more fun
;;; Commentary:
;;; Code:

(use-package emms
  :config
  (emms-standard)
  (emms-default-players))


(use-package pyim
  :config
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))
  (setq default-input-method "pyim"))


(provide 'init-apps)

;;; init-apps.el ends here
