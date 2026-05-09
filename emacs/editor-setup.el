;;; editor-setup.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

;;;###autoload
(defun setup-for-image ()
  "Setup the current buffer for image."
  (set-window-margins (get-buffer-window) 24 24)
  (text-scale-set 3))

;;;###autoload
(defun insert-html-tag (tag-name)
  "Insert an HTML TAG-NAME with user-defined attributes at the current point in the buffer.
   If a region is selected, wrap the region with the HTML tag, except for self-closing tags."
  (interactive (list (read-string "Enter tag name: "))) ;; Prompt for tag name
  (let* ((void-elements '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "source" "track" "wbr"))
         (is-void (member tag-name void-elements))
         (attributes '())
         (continue t))
    ;; Gather attributes
    (while continue
      (let ((attr-name (read-string "Enter attribute name (leave blank to finish): ")))
        (if (string-empty-p attr-name)
            (setq continue nil) ;; Stop if no attribute name is provided
          (let ((attr-value (read-string (format "Enter value for attribute '%s': " attr-name))))
            (push (format "%s=\"%s\"" attr-name attr-value) attributes)))))
    ;; Construct the start tag
    (let ((start-tag (if attributes
                         (format "<%s %s>" tag-name (string-join (reverse attributes) " "))
                       (format "<%s>" tag-name))))
      (if is-void
          ;; Handle void tags
          (insert (concat (string-trim-right start-tag ">") " />"))
        ;; Handle normal tags
        (let ((end-tag (format "</%s>" tag-name)))
          (if (use-region-p)
              ;; If a region is selected, wrap it with the tags
              (let ((region-start (region-beginning))
                    (region-end (region-end)))
                (save-excursion
                  (goto-char region-end)
                  (insert end-tag)
                  (goto-char region-start)
                  (insert start-tag)))
            ;; If no region is selected, insert empty tags at point
            (insert start-tag end-tag)
            (backward-char (length end-tag))))))))

(global-key-bind "C-c t" #'insert-html-tag)
(global-key-bind "C-<" #'undo)
(global-key-bind "C->" #'undo-redo)

(use-package combyier
  :load-path "/usr/local/src/combyier"
  :config (require 'combyier))

;; theme

(add-to-list 'load-path "/usr/local/src/siena-dusk-theme")
(require 'siena-dusk-theme)
(load-theme 'siena-dusk t nil)

;; ligatures and math synbols

(use-package pretty-mode
  
  :config (turn-on-pretty-mode))

;; completions

(use-package vertico
  
  :config (vertico-mode))

(use-package ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
                  (make-llm-ollama
                   :chat-model "codellama" :embedding-model "codellama")))

;; diagram and uml
(use-package plantuml-mode
  )

;; pomodoro
(use-package pomm
  )

(set-scroll-bar-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(setf indent-tabs-mode nil)

(use-package dashboard
  
  :config (dashboard-setup-startup-hook))

;; edior config
(use-package editorconfig
  
  :config
  (editorconfig-mode 1))

;; git stuff

(use-package magit
  
  :bind (("C-c m" . magit-status)))

(use-package rainbow-delimiters
  
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; (rainbow-delimiters-depth-1-face ((t (:foreground "#1b81e8"))))
;; (rainbow-delimiters-depth-2-face ((t (:foreground "#ffffff"))))
;; (rainbow-delimiters-depth-3-face ((t (:foreground "#888888"))))
;; (rainbow-delimiters-depth-4-face ((t (:foreground "#e8591b"))))
;; (rainbow-delimiters-depth-5-face ((t (:foreground "#1b81e8"))))
;; (rainbow-delimiters-depth-6-face ((t (:foreground "#ffffff"))))
;; (rainbow-delimiters-depth-7-face ((t (:foreground "#888888"))))
;; (rainbow-delimiters-depth-8-face ((t (:foreground "#e8591b"))))
;; (rainbow-delimiters-depth-9-face ((t (:foreground "#1b81e8"))))
;; (rainbow-delimiters-unmatched-face ((t (:foreground "#e91b23"))))
;; (rainbow-delimiters-mismatched-face ((t (:foreground "yellow"))))

(use-package yafolding
  
  :bind (("C-c f f" . yafolding-toggle-element)
             ("C-c f g" . yafolding-toggle-all)))

(use-package yasnippet
  
  :config (progn
            (push "~/Programming/dotfiles/emacs/snippets" yas-snippet-dirs)
            (yas-global-mode t)))

;; display binding for commands.
(use-package which-key
  
  :config (which-key-mode t))

;; enable multiple cursors.
(use-package multiple-cursors
  
  :bind (("C-c [" . 'mc/mark-previous-like-this)
         ("C-c ]" . 'mc/mark-next-like-this)))

;; enable jump between windows.
(use-package ace-window
  
  :bind (("C-c w" . 'ace-window)))

(use-package window-layout
  )

;; elfeed
(use-package elfeed
  )

(use-package elfeed-org
  
  :config (progn
            (elfeed-org)
            (setq rmh-elfeed-org-files (list "~/Documents/feeds.org"))))

;; just like tmux.
(use-package zoom-window
  
  :bind (("C-c z z" . zoom-window-zoom)))

;; completion system.

(use-package company
  
  :hook (after-init . global-company-mode))

(use-package flycheck
  
  :config (global-flycheck-mode t))

;; display the diff on each changed line.

(use-package diff-hl
  
  :config (global-diff-hl-mode))

(use-package hl-line
  
  :config (global-hl-line-mode))

(use-package hl-todo
  
  :config (global-hl-todo-mode))

(use-package hl-indent
  
  :config (hl-indent-mode))

;; manage projects.
(use-package projectile
  
  :bind (("C-c p k" . projectile-kill-buffers)
         ("C-c p t" . projectile-run-shell)
         ("C-c p d" . projectile-dired)
         ("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file)
         ("C-c p g" . projectile-grep)
         ("C-c p s" . projectile-run-shell))
  :config (projectile-mode))

;; nix package manager
(use-package nix-mode
  )

(use-package nix-sandbox
  
  :after 'nix-mode)

;; enable paredit when possible.
(use-package paredit
  
  :hook ((lisp-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)))


;; file formats

(use-package yaml-mode
  )

(use-package json-mode
  
  :custom (js-indent-level 2))

;; text window margins

(use-package olivetti
  )

(use-package restclient
  
  :mode (("\\.http\\'" . restclient-mode)))

;; debugger

(use-package realgud
  )

(use-package dockerfile-mode
  )

;; structural search and rewrite of code

(use-package comby
  )

;; works with pull request from svc sites

(use-package forge
  )

(use-package lsp-mode
  
  :bind (("C-c l r" . #'lsp-restart-workspace)
         ("C-c l d" . #'lsp-shutdown-workspace)
         ("C-c l l" . #'lsp-mode)))

(use-package dap-mode
  
  :config
  (dap-mode t)
  (dap-ui-mode t))

(add-to-list 'load-path "/usr/local/src/combobulate")

(require 'combobulate)

;; structural editing

;; custom key bindings

;; auto-save
(setf backup-directory-alist '(("." . "/usr/local/src/emacs/tmp/auto-save")))

;; key bindings.
(global-key-bind (kbd "C-c q") 'whitespace-cleanup)

;; key bindings.
(global-key-bind (kbd "C-c .") 'projectile-compile-project)

(use-package rfc-mode)

(setq visible-bell t)

(use-package cursor-agent
  
  :load-path "/usr/local/src/cursor-agent.el/"
  :config
  (require 'cursor-agent)
  (setq cursor-agent-default-model "composer-1"))

(use-package esc-mode
  
  :load-path "/usr/local/src/emacs-esc/"
  :config
  (require 'esc-mode))

(provide 'editor-setup)
;;; editor-setup.el ends here
