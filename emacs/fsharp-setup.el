;;; fsharp-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'use-package)

(use-package fsharp-mode
  :ensure t
  :custom ((fsharp-indent-offset 2)
	   (inferior-fsharp-program "fsharpi --readline-"))
  :hook ((fsharp-mode . highlight-indentation-mode)))

(provide 'fsharp-editor)
;;; fsharp-editor.el ends here
