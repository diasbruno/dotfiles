;;; lisp-setup.el -- My emacs configuration.  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(use-package sly
  
  :config
  (require 'sly-autoloads)
  (sly-setup)
  (require 'sly-stickers))

(add-to-list 'load-path "~/Programming/sly-stepper")

(use-package sly-stepper
  :after sly
  :config (require 'sly-stepper-autoloads))

(use-package geiser
  )

(use-package geiser-gambit
  
  :after geiser)

(provide 'lisp-setup)
;;; lisp-setup.el ends here
