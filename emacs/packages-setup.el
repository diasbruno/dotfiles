;;; packages-setup.el -- Setup the neccessary stuff for 'package.  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'package)

(let ((urls '(("melpa" . "http://melpa.org/packages/")
              ("melpa-stable" . "https://stable.melpa.org/packages/")
	      ("org" . "http://orgmode.org/elpa/"))))
  (mapc (lambda (source)
	  (add-to-list 'package-archives source t))
	urls))

(unless (package-installed-p 'use-package)
  ;(package-install 'use-package)
  )

(provide 'packages-setup)
;;; packages-setup.el ends here
