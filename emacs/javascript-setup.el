;;; javascript-setup --- javascript editor.
;;;
;;; Commentary:
;;;
;;; Code:
;;;

(require 'use-package)

(load "~/Programming/js-eval.el/js-eval.el")

(global-key-bind (kbd "C-c j e") #'js-eval-eval-expression)
(global-key-bind (kbd "C-c j r") #'js-eval-eval-region)
(global-key-bind (kbd "C-c j i") #'js-eval)
(global-key-bind (kbd "C-c j t") #'js-eval-quit)

(use-package eslint-fix
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :custom ((js-indent-level 2)))

(use-package typescript-mode
  :ensure t
  :custom ((typescript-indent-level 2))
  :hook ((typescript-mode . lsp)
	 (typescript-mode . eslint-fix)))

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

(use-package vue-mode
  :ensure t
  :custom ((js-indent-level 2)
	   (js2-indent-level 2)
	   (vue-html-extra-indent 2))
  :bind (("C-c t" . #'vue-js-indentation))
  :hook (vue-mode . lsp))

(use-package scss-mode
  :ensure t
  :custom (css-indent-offset 2))

(use-package sass-mode
  :ensure t
  :custom (css-indent-offset 2))

(provide 'javascript-setup)
;;; javascript-setup.el ends here
