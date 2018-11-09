;;; init-ocaml.el --- Ocaml development environment
;;; Commentary:

;; Make sure you have 'opam', 'merlin' and 'utop' installed!

;;; Code:

(use-package tuareg
  :init
  (add-hook 'tuareg-mode-hook
	    (lambda()
	      (when (functionp 'prettify-symbols-mode)
		(prettify-symbols-mode))))

  ;; config merlin
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      ;; Register Merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      ;; Automatically start it in OCaml buffers
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)))

  ;; config utop
  (defcustom utop-command "opam exec -- utop -emacs"
    "The command to execute for utop."
    :type 'string
    :group 'utop)
  ;; Add the opam lisp dir to the emacs load path
  (add-to-list
   'load-path
   (replace-regexp-in-string
    "\n" "/share/emacs/site-lisp"
    (shell-command-to-string "opam config var prefix")))
  ;; Automatically load utop.el
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode))

(provide 'init-ocaml)

;;; init-ocaml.el ends here
