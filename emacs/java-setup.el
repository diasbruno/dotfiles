;;; java-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(defvar lombok-jar nil)

(straight-use-package
 '(kotlin-mode :host github :repo "Emacs-Kotlin/kotlin-mode"))

(straight-use-package
 '(flycheck-kotlin :host github :repo "whirm/flycheck-kotlin"))
(setq flycheck-kotlin-ktlint-executable "/dias/ktlint/ktlint/build/run/ktlint")
(add-hook 'kotlin-mode-hook #'flycheck-kotlin-setup)

(straight-use-package
 '(lsp-java :host github :repo "emacs-lsp/lsp-java"))
(require 'dap-java)

  ;; lsp-java-vmargs
  ;;	(list "-noverify"
  ;;	      "-Xmx2G"
  ;;	      "-XX:+UseG1GC"
  ;;	      "-XX:+UseStringDeduplication"
  ;;	      (concat "-javaagent:" lombok-jar)
  ;;	      (concat "-Xbootclasspath/a:" lombok-jar))


(setq lsp-file-watch-ignored
	'(".idea" ".ensime_cache" ".eunit" "node_modules"
	  ".git" ".hg" ".fslckout" "_FOSSIL_"
	  ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
	  "build")

	lsp-java-import-order '["" "java" "javax" "#"]
	;; Don't organize imports on save
	lsp-java-save-action-organize-imports nil)

(add-hook 'java-mode-hook #'lsp)

(provide 'java-setup)
;;; java-setup.el ends here
