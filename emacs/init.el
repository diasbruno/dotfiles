;;; init.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(load-file "/usr/local/src/dotfiles/emacs/packages-setup.el")
(load-file "/usr/local/src/dotfiles/emacs/editor-setup.el")

(let ((langs '(packages editor
	       lisp haskell smalltalk ccpp
	       fsharp erlang java
	       python javascript)))
  (mapc (lambda (file)
	  (load-file (concat "/usr/local/src/dotfiles/emacs/"
			     (symbol-name file)
			     "-setup.el")))
	langs))

(provide 'init)
;;; init.el ends here
