;;; lisp-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'use-package)

(use-package slime
  :ensure t)

(use-package slite
  :load-path "~/quicklisp/dists/quicklisp/software/slite-20221106-git"
  :init (progn
	  (setf slite-slime-impl :slime
		inferior-lisp-program "/nix/store/rxwj1gsj9w6z9mmqlfix1slrfyij4b99-sbcl-2.3.6/bin/sbcl")))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode-hook . #'paredit-mode)))

(use-package slime-star
  :load-path "~/Programming/slime-star"
  :config (progn
	    ;; Add slime-star to slime-contribs:
	    (setq slime-contribs '(slime-fancy slime-star))
	    (slime-setup)))

(use-package clojure-snippets
  :ensure t)

(provide 'lisp-setup)
;;; lisp-setup.el ends here
