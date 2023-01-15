;;; go-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'use-package)

(use-package go-mode
  :ensure t)

(use-package golint
  :ensure t)

(provide 'go-setup)
;;; go-setup.el ends here
