;;; init-ai.el --- AI powered Emacs
;;; Commentary:
;;; Code:


(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-default-chat-mode 'architect))

(use-package gptel
  :hook ((gptel-mode . gptel-highlight-mode))
  :bind (("C-c <return>" . gptel-send)
         ("C-c C-<return>" . gptel-menu)
         ("C-c C-g" . gptel-abort))
  :config
  (defun ai--load-skill (name)
    (let ((file-path (file-name-concat user-emacs-directory
                                       "skills/"
                                       (symbol-name name)
                                       "SKILL.md")))
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string))))

  (setq-default gptel-model 'deepseek-ai/deepseek-v3.2
                gptel-backend
                (gptel-make-openai "Open Source Models"
                  :host "integrate.api.nvidia.com"
                  :endpoint "/v1/chat/completions"
                  :key #'gptel-api-key-from-auth-source
                  :stream t
                  :models '(deepseek-ai/deepseek-v3.2
                            qwen/qwen3-coder-480b-a35b-instruct)))

  (gptel-make-preset 'shell
    :backend "Open Source Models"
    :model 'qwen/qwen3-coder-480b-a35b-instruct
    :system (lambda () (ai--load-skill 'shell))))

(provide 'init-ai)

;;; init-ai.el ends here
