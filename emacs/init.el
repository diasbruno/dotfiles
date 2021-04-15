;;; init.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(let ((langs '(packages editor
	       lisp haskell smalltalk ccpp
	       fsharp swift erlang java
	       python javascript)))
  (mapc (lambda (file)
	  (load-file (concat "/usr/local/src/dotfiles/emacs/"
			     (symbol-name file)
			     "-setup.el")))
	langs))

(provide 'init)
;;; init.el ends here
