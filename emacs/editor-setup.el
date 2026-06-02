;;; editor-setup.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


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

(add-to-list 'load-path "/usr/local/src/combyier")
(require 'combyier)

;; theme

(add-to-list 'load-path "/usr/local/src/siena-dusk-theme")
(require 'siena-dusk-theme)
(load-theme 'siena-dusk t nil)

;; ligatures and math synbols

(straight-use-package
 '(pretty-mode :host github :repo "akatov/pretty-mode"))
(turn-on-pretty-mode)

;; completions

(straight-use-package
 '(vertico :host github :repo "minad/vertico"))
(vertico-mode)

(straight-use-package
 '(ellama :host github :repo "s-kostyaev/ellama"))
(setopt ellama-language "English")
(require 'llm-ollama)
(setopt ellama-provider
                (make-llm-ollama
                 :chat-model "codellama" :embedding-model "codellama"))

;; diagram and uml
(straight-use-package
 '(plantuml-mode :host github :repo "skuro/plantuml-mode"))

;; pomodoro
(straight-use-package
 '(pomm :host github :repo "SqrtMinusOne/pomm.el"))

(set-scroll-bar-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(setf indent-tabs-mode nil)

(straight-use-package
 '(dashboard :host github :repo "emacs-dashboard/emacs-dashboard"))
(dashboard-setup-startup-hook)

;; edior config
(straight-use-package
 '(editorconfig :host github :repo "editorconfig/editorconfig-emacs"))
(editorconfig-mode 1)

;; git stuff

(straight-use-package
 '(magit :host github :repo "magit/magit"))
(global-set-key (kbd "C-c m") #'magit-status)

(straight-use-package
 '(rainbow-delimiters :host github :repo "Fanael/rainbow-delimiters"))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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

(straight-use-package
 '(yafolding :host github :repo "zenozeng/yafolding.el"))
(global-set-key (kbd "C-c f f") #'yafolding-toggle-element)
(global-set-key (kbd "C-c f g") #'yafolding-toggle-all)

(straight-use-package
 '(yasnippet :host github :repo "joaotavora/yasnippet"))
(push "~/Programming/dotfiles/emacs/snippets" yas-snippet-dirs)
(yas-global-mode t)

;; display binding for commands.
(straight-use-package
 '(which-key :host github :repo "justbur/emacs-which-key"))
(which-key-mode t)

;; enable multiple cursors.
(straight-use-package
 '(multiple-cursors :host github :repo "magnars/multiple-cursors.el"))
(global-set-key (kbd "C-c [") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c ]") #'mc/mark-next-like-this)

;; enable jump between windows.
(straight-use-package
 '(ace-window :host github :repo "abo-abo/ace-window"))
(global-set-key (kbd "C-c w") #'ace-window)

(straight-use-package
 '(window-layout :host github :repo "kiwanami/emacs-window-layout"))

;; elfeed
(straight-use-package
 '(elfeed :host github :repo "skeeto/elfeed"))

(straight-use-package
 '(elfeed-org :host github :repo "remyhonig/elfeed-org"))
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/Documents/feeds.org"))

;; just like tmux.
(straight-use-package
 '(zoom-window :host github :repo "syohex/emacs-zoom-window"))
(global-set-key (kbd "C-c z z") #'zoom-window-zoom)

;; completion system.

(straight-use-package
 '(company :host github :repo "company-mode/company-mode"))
(add-hook 'after-init-hook #'global-company-mode)

(straight-use-package
 '(flycheck :host github :repo "flycheck/flycheck"))
(global-flycheck-mode t)

;; display the diff on each changed line.

(straight-use-package
 '(diff-hl :host github :repo "dgutov/diff-hl"))
(global-diff-hl-mode)

(require 'hl-line)
(global-hl-line-mode)

(straight-use-package
 '(hl-todo :host github :repo "tarsius/hl-todo"))
(global-hl-todo-mode)

(straight-use-package
 '(hl-indent :host github :repo "ikirill/hl-indent"))
(hl-indent-mode)

;; manage projects.
(straight-use-package
 '(projectile :host github :repo "bbatsov/projectile"))
(global-set-key (kbd "C-c p k") #'projectile-kill-buffers)
(global-set-key (kbd "C-c p t") #'projectile-run-shell)
(global-set-key (kbd "C-c p d") #'projectile-dired)
(global-set-key (kbd "C-c p p") #'projectile-switch-project)
(global-set-key (kbd "C-c p f") #'projectile-find-file)
(global-set-key (kbd "C-c p g") #'projectile-grep)
(global-set-key (kbd "C-c p s") #'projectile-run-shell)
(projectile-mode)

;; nix package manager
(straight-use-package
 '(nix-mode :host github :repo "NixOS/nix-mode"))

(straight-use-package
 '(nix-sandbox :host github :repo "travisbhartwell/nix-emacs"))

;; enable paredit when possible.
(straight-use-package
 '(paredit :host github :repo "emacsmirror/paredit"))
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)


;; file formats

(straight-use-package
 '(yaml-mode :host github :repo "yoshiki/yaml-mode"))

(straight-use-package
 '(json-mode :host github :repo "json-emacs/json-mode"))
(setq js-indent-level 2)

;; text window margins

(straight-use-package
 '(olivetti :host github :repo "rnkn/olivetti"))

(straight-use-package
 '(restclient :host github :repo "pashky/restclient.el"))
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; debugger

(straight-use-package
 '(realgud :host github :repo "realgud/realgud"))

(straight-use-package
 '(dockerfile-mode :host github :repo "spotify/dockerfile-mode"))

;; structural search and rewrite of code

(straight-use-package
 '(comby :host github :repo "antirez/comby"))

;; works with pull request from svc sites

(straight-use-package
 '(forge :host github :repo "magit/forge"))

(straight-use-package
 '(lsp-mode :host github :repo "emacs-lsp/lsp-mode"))
(global-set-key (kbd "C-c l r") #'lsp-restart-workspace)
(global-set-key (kbd "C-c l d") #'lsp-shutdown-workspace)
(global-set-key (kbd "C-c l l") #'lsp-mode)

(straight-use-package
 '(dap-mode :host github :repo "emacs-lsp/dap-mode"))
(dap-mode t)
(dap-ui-mode t)

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

(straight-use-package
 '(rfc-mode :host github :repo "galdor/rfc-mode"))

(setq visible-bell t)

(add-to-list 'load-path "/usr/local/src/cursor-agent.el/")
(require 'cursor-agent)
(setq cursor-agent-default-model "composer-1")

(add-to-list 'load-path "/usr/local/src/emacs-esc/")
(require 'esc-mode)

(provide 'editor-setup)
;;; editor-setup.el ends here
