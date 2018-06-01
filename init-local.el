;;; init-local.el -- local config
;;; Commentary:
;;; Code:

;; C/C++
(when (maybe-require-package 'irony)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (when (maybe-require-package 'company-irony)
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))
  (when (maybe-require-package 'flycheck-irony)
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  (when (maybe-require-package 'irony-eldoc)
    (add-hook 'irony-mode-hook #'irony-eldoc)))
(when (maybe-require-package 'google-c-style)
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-make-newline-indent))

;; Python
(defun workon (virtualenv)
  "Workon VIRTUALENV."
  (interactive
   (list (read-directory-name "workon: " "~/.virtualenvs/")))
  (pythonic-activate virtualenv))

;; OCaml
(load "/home/e-neo/.opam/4.06.1/share/emacs/site-lisp/tuareg-site-file")
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    ;; Automatically load utop.el
    (autoload 'utop "utop" "Toplevel for OCaml" t)
    ;; Use the opam installed utop
    (setq utop-command "opam config exec -- utop -emacs")))

;; Racket
(maybe-require-package 'racket-mode)

;; EMMS
(when (maybe-require-package 'emms)
  (require 'emms-setup)
  (emms-all)
  (emms-default-players))

;; Org mode
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("latexmk -pdflatex='xelatex -shell-escape -interaction nonstopmode' -pdf -f %f && latexmk -bibtex -c"))

(provide 'init-local)
;;; init-local.el ends here
