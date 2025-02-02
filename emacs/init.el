;;; init.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(defvar *dotfiles-path* "~/Programming/dotfiles")

(defun global-key-bind (key fn)
  "Define a global bind for KEY to execute FN."
  (global-set-key (kbd key) fn))

(defvar *diasbruno/required-configurations*
  '(packages editor lisp))

(defvar *diasbruno/language-configurations*
  '(haskell smalltalk ccpp fsharp swift erlang java python javascript zig))


(cl-map nil
	(lambda (name)
	  (load (concat *dotfiles-path* "/emacs/" (symbol-name name) "-setup.el")))
	(append nil *diasbruno/required-configurations* *diasbruno/language-configurations*))

(provide 'init)
;;; init.el ends here
