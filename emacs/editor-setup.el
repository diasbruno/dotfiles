;;; editor-setup.el --- Summary
;;; Commentary:
;;; Code:

(require 'use-package)

(load-file (file-from-dotfiles "/emacs/bw-theme.el"))

(use-package bw-theme
  :custom
  (bw-foreground "#ffffff")
  (bw-background "#191a1e")
  :config
  (load-theme 'bw t))

(set-scroll-bar-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(setf indent-tabs-mode nil)

;; auto-save
(setf backup-directory-alist '(("." . "/tmp/.emacs.d/auto-save")))

;; key bindings.
(global-key-bind (kbd "C-c q") 'whitespace-cleanup)

;; key bindings.
(global-key-bind (kbd "C-c .") 'projectile-compile-project)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; git stuff

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode))
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#1b81e8"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "#ffffff"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "#888888"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "#e8591b"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "#1b81e8"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "#ffffff"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "#888888"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "#e8591b"))))
  (rainbow-delimiters-depth-9-face ((t (:foreground "#1b81e8"))))
  (rainbow-delimiters-unmatched-face ((t (:foreground "#e91b23"))))
  (rainbow-delimiters-mismatched-face ((t (:foreground "yellow")))))

(use-package yafolding
  :ensure t
  :bind (("C-c f f" . yafolding-toggle-element)
	 ("C-c f g" . yafolding-toggle-all)))

(use-package yasnippet
  :ensure t
  :config (progn
            (push (file-from-dotfiles "/emacs/snippets") yas-snippet-dirs)
            (yas-global-mode t)))

;; display binding for commands.
(use-package which-key
  :ensure t
  :config (which-key-mode t))

;; enable multiple cursors.
(use-package multiple-cursors
  :ensure t
  :bind (("C-c [" . 'mc/mark-previous-like-this)
	 ("C-c ]" . 'mc/mark-next-like-this)))

;; enable jump between windows.
(use-package ace-window
  :ensure t
  :bind (("C-c w" . 'ace-window)))

(use-package window-layout
  :ensure t)

;; elfeed
(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config (progn
            (elfeed-org)
            (setq rmh-elfeed-org-files (list "~/Documents/feeds.org"))))

(use-package elfeed-dashboard
  :ensure t
  :config
  (progn
    (setq elfeed-dashboard-file "~/Documents/elfeed-dashboard.org")
    ;; update feed counts on elfeed-quit
    (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links)))

;; just like tmux.
(use-package zoom-window
  :ensure t
  :bind (("C-c z z" . zoom-window-zoom)))

;; completion system.

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

;; display the diff on each changed line.

(use-package diff-hl
  :ensure t
  :config (global-diff-hl-mode))

;; manage projects.
(use-package projectile
  :ensure t
  :bind (("C-c p k" . projectile-kill-buffers)
         ("C-c p t" . projectile-run-shell)
         ("C-c p d" . projectile-dired)
         ("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file)
         ("C-c p g" . projectile-grep)
         ("C-c p s" . projectile-run-shell))
  :init (projectile-mode))

;; nix package manager

(use-package nix-mode
  :ensure t)

(use-package nix-sandbox
  :ensure t
  :after 'nix-mode)

;; enable paredit when possible.
(use-package paredit
  :ensure t
  :hook ((lisp-mode . enable-paredit-mode)
	 (emacs-lisp-mode . enable-paredit-mode)))

;; yaml mode

(use-package yaml-mode
  :ensure t)

;; json mode

(use-package json-mode
  :ensure t
  :custom (js-indent-level 2))

(use-package restclient
    :ensure t
    :mode (("\\.http\\'" . restclient-mode)))

(use-package realgud
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package comby
  :ensure t)

(use-package forge
  :ensure t)

(use-package lsp-mode
  :ensure t
  :bind (("C-c l r" . #'lsp-restart-workspace)
	 ("C-c l d" . #'lsp-shutdown-workspace)
	 ("C-c l l" . #'lsp-mode)))

(provide 'editor-setup)
;;; editor-setup.el ends here
