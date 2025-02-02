;;; lisp-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'use-package)

(use-package sly
  :ensure t
  :config
  (require 'sly-autoloads)
  (sly-setup)
  (require 'sly-stickers))

(add-to-list 'load-path "~/Programming/sly-stepper")

(use-package sly-stepper
  :after sly
  :config (require 'sly-stepper-autoloads))

(use-package geiser
  :ensure t)

(use-package geiser-gambit
  :ensure t
  :after geiser)

(provide 'lisp-setup)
;;; lisp-setup.el ends here
