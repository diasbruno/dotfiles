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
  :ensure t)

(provide 'haskell-setup)
;;; haskell-setup.el ends here
