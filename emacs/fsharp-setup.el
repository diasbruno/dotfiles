;;; fsharp-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(use-package fsharp-mode
  
  :custom ((fsharp-indent-offset 2)
	   (inferior-fsharp-program "fsharpi --readline-"))
  :hook ((fsharp-mode . highlight-indentation-mode)))

(provide 'fsharp-editor)
;;; fsharp-editor.el ends here
