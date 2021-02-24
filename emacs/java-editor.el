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
  :init
  (defun jmi/java-mode-config ()
    (setq-local tab-width 4
		c-basic-offset 4)
    (toggle-truncate-lines 1)
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4)
    (lsp))

  :config
  ;; Enable dap-java
  (require 'dap-java)

  ;; Support Lombok in our projects, among other things
  (setq lsp-java-vmargs
	(list "-noverify"
	      "-Xmx2G"
	      "-XX:+UseG1GC"
	      "-XX:+UseStringDeduplication"
	      (concat "-javaagent:" jmi/lombok-jar)
	      (concat "-Xbootclasspath/a:" jmi/lombok-jar))

	lsp-file-watch-ignored
	'(".idea" ".ensime_cache" ".eunit" "node_modules"
	  ".git" ".hg" ".fslckout" "_FOSSIL_"
	  ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
	  "build")

	lsp-java-import-order '["" "java" "javax" "#"]
	;; Don't organize imports on save
	lsp-java-save-action-organize-imports nil

	;; Formatter profile
	lsp-java-format-settings-url
	(concat "file://" jmi/java-format-settings-file))

  :demand t)
