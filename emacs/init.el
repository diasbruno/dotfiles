;;; init.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(defvar *dotfiles-path* "~/Programming/dotfiles")

(defun file-from-dotfiles (path)
  "Given a PATH return a file from the dotfiles path."
  (concat *dotfiles-path* path))

(defun global-key-bind (key fn)
  "Define a global bind for KEY to execute FN."
  (global-set-key (kbd key) fn))

(defvar *diasbruno/required-configurations*
  '(packages editor lisp))

(defvar *diasbruno/language-configurations*
  '(haskell smalltalk ccpp fsharp swift
	    erlang java python javascript))

(defun diasbruno/configuration-from-symbol (sym)
  "Make the configuration file name from SYM."
  (file-from-dotfiles (concat "/emacs/" (symbol-name sym) "-setup.el")))

(defun diasbruno/load-configuration (sym)
  "Load the configuration for SYM."
  (load-file (diasbruno/configuration-from-symbol sym)))

(mapc #'diasbruno/load-configuration
      *diasbruno/required-configurations*)

(mapc #'diasbruno/load-configuration
      *diasbruno/language-configurations*)

(defun diasbruno/generate-loader (sym)
  "Generate loader for a language with a SYM."
  (lambda ()
    (diasbruno/load-configuration sym)))

(provide 'init)
;;; init.el ends here
