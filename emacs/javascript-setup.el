;;; javascript-setup --- javascript editor.  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;


(load "/usr/local/src/js-eval.el/js-eval.el")

(global-key-bind (kbd "C-c j e") #'js-eval-eval-expression)
(global-key-bind (kbd "C-c j r") #'js-eval-eval-region)
(global-key-bind (kbd "C-c j i") #'js-eval)
(global-key-bind (kbd "C-c j t") #'js-eval-quit)

(straight-use-package
 '(eslint-fix :host github :repo "codesuki/eslint-fix"))

(straight-use-package
 '(rjsx-mode :host github :repo "felipeochoa/rjsx-mode"))
(setq js-indent-level 2)

(straight-use-package
 '(typescript-mode :host github :repo "emacs-typescript/typescript.el"))
(setq typescript-indent-level 2)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'eslint-fix)

(global-key-bind (kbd "C-c d") 'eslint-fix)

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(defun vue-js-indent ()
  "Take the overlay of the vue and indent according to its current mode."
  (with-current-buffer (current-buffer)
    (let* ((leave (point))
	   (found-overlay (mmm-overlay-at (point)))
	   (start-overlay (overlay-start found-overlay))
	   (end-overlay (overlay-end found-overlay))
	   (use-mode (overlay-get found-overlay 'mmm-mode))
	   (text (buffer-substring-no-properties start-overlay
						 end-overlay))
	   text-indented)
      (with-temp-buffer
	(progn
	  (funcall use-mode)
	  (insert text)
	  (let ((indenter-region (buffer-local-value
				  'indent-region-function
				  (current-buffer))))
	    (funcall indenter-region (point-min) (point-max))
	    (setq text-indented
		  (buffer-substring-no-properties (point-min)
						  (point-max))))))
      (kill-region start-overlay end-overlay)
      (insert text-indented)
      (goto-char leave)
      (mmm-parse-buffer))))

(defun vue-js-indentation ()
  "Indent."
  (interactive)
  (vue-js-indent))

(straight-use-package
 '(vue-mode :host github :repo "AdamNiederer/vue-mode"))
(setq js-indent-level 2
      js2-indent-level 2
      vue-html-extra-indent 2)
(global-set-key (kbd "C-c t") #'vue-js-indentation)
(add-hook 'vue-mode-hook #'lsp)

(straight-use-package
 '(scss-mode :host github :repo "antonj/Scss-mode"))
(setq css-indent-offset 2)

(straight-use-package
 '(sass-mode :host github :repo "nex3/sass-mode"))
(setq css-indent-offset 2)

(provide 'javascript-setup)
;;; javascript-setup.el ends here
