;;; haskell-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'use-package)

(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :config (progn
            (setf lsp-haskell-server-path "haskell-language-server-wrapper")))

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(provide 'haskell-setup)
;;; haskell-setup.el ends here
