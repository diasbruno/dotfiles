;;; go-setup.el -- My emacs configuration.  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(straight-use-package
 '(go-mode :host github :repo "dominikh/go-mode.el"))

(straight-use-package
 '(golint :host github :repo "golang/lint"))

(provide 'go-setup)
;;; go-setup.el ends here
