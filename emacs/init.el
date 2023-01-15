;;; init.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(defun global-key-bind (key fn)
  "Define a global bind for KEY to execute FN."
  (global-set-key (kbd key) fn))

(defvar *diasbruno/emacs-configurations-path*
  "/usr/local/src/dotfiles/emacs/")

(defvar *diasbruno/required-configurations*
  '(packages editor lisp))

(defvar *diasbruno/language-configurations*
  '(haskell smalltalk ccpp fsharp swift
	    erlang java python javascript))

(defun diasbruno/configuration-file-name (sym)
  "Make the configuration file name from SYM."
  (concat *diasbruno/emacs-configurations-path*
	  (symbol-name sym)
	  "-setup.el"))

(defun diasbruno/load-configuration (sym)
  "Load the configuration for SYM."
  (load-file (diasbruno/configuration-file-name sym)))

(mapc #'diasbruno/load-configuration
      *diasbruno/required-configurations*)

(defun diasbruno/generate-loader (sym)
  "Generate loader for a language with a SYM."
  (lambda ()
    (diasbruno/load-configuration sym)))

(defmacro diasbruno/load-declarations (sym)
  "SYM."
  (let ((name (make-symbol (concat (symbol-name sym) "-lang"))))
    `(defun ,name ()
       (diasbruno/configuration-file-name ',sym))))

(eval-and-compile
  (diasbruno/load-declarations haskell)
  (diasbruno/load-declarations java)
  (diasbruno/load-declarations javascript))

(provide 'init)
;;; init.el ends here
