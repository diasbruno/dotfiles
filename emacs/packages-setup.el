;;; packages-setup.el -- Setup the neccessary stuff for 'package.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'package)

(let ((urls '(("melpa" . "http://melpa.org/packages/")
	      ("org" . "http://orgmode.org/elpa/"))))
  (mapc (lambda (source)
	  (add-to-list 'package-archives source t))
	urls))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(require 'use-package)

(provide 'packages-setup)
;;; packages-setup.el ends here
