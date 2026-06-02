;;; fsharp-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(straight-use-package
 '(fsharp-mode :host github :repo "fsharp/fsharp-mode"))
(setq fsharp-indent-offset 2
      inferior-fsharp-program "fsharpi --readline-")
(add-hook 'fsharp-mode-hook #'highlight-indentation-mode)

(provide 'fsharp-editor)
;;; fsharp-editor.el ends here
