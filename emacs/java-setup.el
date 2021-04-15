;;; java-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'use-package)

(defvar lombok-jar nil)

(use-package kotlin-mode
  :ensure t)

(use-package flycheck-kotlin
  :ensure t
  :hook (kotlin-mode . #'flycheck-kotlin-setup)
  :custom (flycheck-kotlin-ktlint-executable "/dias/ktlint/ktlint/build/run/ktlint")
  :after flycheck-mode kotlin-mode)

(use-package dap-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package lsp-java
  :config
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

  :demand t
  :hook (java-mode . lsp))

(provide 'java-setup)
;;; java-setup.el ends here
