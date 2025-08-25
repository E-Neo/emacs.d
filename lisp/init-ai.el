;;; init-ai.el --- AI powered Emacs
;;; Commentary:
;;; Code:


(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-default-chat-mode 'architect))


(provide 'init-ai)

;;; init-ai.el ends here
