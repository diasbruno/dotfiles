;;; haskell-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(use-package haskell-mode
  :ensure t)

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp" "--verbose")))
  (add-to-list 'eglot-server-programs '(fsharp-mode . ("FSharpLanguageServer"))))

(provide 'haskell-setup)
;;; haskell-setup.el ends here
