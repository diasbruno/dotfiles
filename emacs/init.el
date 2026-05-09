;;; init.el -- My emacs configuration.  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(defvar *dotfiles-path* "/usr/local/src/dotfiles")

(defun global-key-bind (key fn)
  "Define a global bind for KEY to execute FN."
  (global-set-key (kbd key) fn))

(defvar *diasbruno/required-configurations*
  '(packages editor))

(cl-map nil
	(lambda (name)
	  (load (concat *dotfiles-path* "/emacs/" (symbol-name name) "-setup.el")))
	*diasbruno/required-configurations*)

(defun diasbruno/lazy-load-language (language hooks)
  "Set up lazy loading of LANGUAGE setup file when any of HOOKS fires.
The setup file is loaded once on first mode activation; hooks are removed
only after a successful load so that a failed load can be retried."
  (let* ((setup-file (concat *dotfiles-path* "/emacs/" (symbol-name language) "-setup.el"))
         (loader-sym (intern (format "diasbruno/load-%s-setup" language)))
         (loaded-sym (intern (format "diasbruno/%s-setup-loaded-p" language)))
         (loader-fn (lambda ()
                      (unless (symbol-value loaded-sym)
                        (set loaded-sym t)
                        (condition-case err
                            (progn
                              (load setup-file)
                              (dolist (hook hooks)
                                (remove-hook hook loader-sym)))
                          (error
                           (set loaded-sym nil)
                           (signal (car err) (cdr err))))))))
    (set loaded-sym nil)
    (fset loader-sym loader-fn)
    (dolist (hook hooks)
      (add-hook hook loader-sym))))

(diasbruno/lazy-load-language 'lisp       '(lisp-mode-hook emacs-lisp-mode-hook))
(diasbruno/lazy-load-language 'haskell    '(haskell-mode-hook haskell-literate-mode-hook))
(diasbruno/lazy-load-language 'smalltalk  '(smalltalk-mode-hook))
(diasbruno/lazy-load-language 'ccpp       '(c-mode-hook c++-mode-hook))
(diasbruno/lazy-load-language 'fsharp     '(fsharp-mode-hook))
(diasbruno/lazy-load-language 'swift      '(swift-mode-hook))
(diasbruno/lazy-load-language 'erlang     '(erlang-mode-hook))
(diasbruno/lazy-load-language 'java       '(java-mode-hook))
(diasbruno/lazy-load-language 'python     '(python-mode-hook))
(diasbruno/lazy-load-language 'javascript '(js-mode-hook js2-mode-hook typescript-mode-hook))
(diasbruno/lazy-load-language 'zig        '(zig-mode-hook))

(provide 'init)
;;; init.el ends here
