;;; lisp-setup.el -- My emacs configuration.  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(straight-use-package 'sly)
(require 'sly-autoloads)
(sly-setup)
(require 'sly-stickers)

(add-to-list 'load-path "~/Programming/sly-stepper")

(straight-use-package 'sly-stepper)
(with-eval-after-load 'sly
  (require 'sly-stepper-autoloads))

(straight-use-package 'geiser)

(straight-use-package 'geiser-gambit)

(provide 'lisp-setup)
;;; lisp-setup.el ends here
