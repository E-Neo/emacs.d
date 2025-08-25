;;; init-git.el --- Git settings
;;; Commentary:
;;; Code:

(use-package magit
  :defer t
  :config
  (use-package diff-hl
    :defer t
    :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
           (magit-post-refresh . diff-hl-magit-post-refresh)
           (after-init . global-diff-hl-mode))))


(provide 'init-git)

;;; init-git.el ends here
