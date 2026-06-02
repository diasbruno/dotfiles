;;; haskell-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(straight-use-package 'haskell-mode)

(straight-use-package 'lsp-haskell)
(setf lsp-haskell-server-path "haskell-language-server-wrapper")

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(provide 'haskell-setup)
;;; haskell-setup.el ends here
