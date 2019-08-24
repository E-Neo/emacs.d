;;; init-rust.el --- Support for the Rust language
;;; Commentary:
;;; Code:

(use-package rust-mode
  :config
  (use-package racer
    :hook ((rust-mode . racer-mode)
           (racer-mode . eldoc-mode)
           (racer-mode . company-mode)))
  (use-package flycheck-rust
    :config
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(provide 'init-rust)
;;; init-rust.el ends here
