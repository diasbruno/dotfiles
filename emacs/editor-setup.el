;;; editor-setup.el --- Summary
;;; Commentary:
;;; Code:

(require 'use-package)

;; (load-file "/usr/local/src/dotfiles/emacs/bw-theme.el")

;; (use-package bw-theme
;;   :custom
;;   (bw-foreground "#ffffff")
;;   (bw-background "#191a1e")
;;   :config
;;   (load-theme 'bw t))

(use-package badwolf-theme
  :ensure t
  :init (load-theme 'badwolf t))

(set-scroll-bar-mode nil)

(setq tool-bar-mode -1)

;; auto-save
(setq backup-directory-alist '(("." . "/tmp/.emacs.d/auto-save")))

(defun global-key-bind (key fn)
  "Define a global bind for KEY to execute FN."
  (global-set-key (kbd key) fn))

;; key bindings.
(global-key-bind (kbd "C-c q") 'whitespace-cleanup)

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
	 ("C-c f F" . yafolding-toggle-all)))

(use-package yasnippet
  :ensure t
  :config (progn
	    (push "/usr/local/src/dotfiles/emacs/snippets" yas-snippet-dirs)
	    (yas-global-mode t)))

;; display binding for commands.
(use-package which-key
  :ensure t
  :config (which-key-mode t))

;; enable multiple cursors.
(use-package multiple-cursors
  :ensure t
  :bind (("M-[" . 'mc/mark-previous-like-this)
	 ("M-]" . 'mc/mark-next-like-this)))

;; enable jump between windows.
(use-package ace-window
  :ensure t
  :bind (("C-c w" . 'ace-window)))

;; just like tmux.
(use-package zoom-window
  :ensure t
  :bind (("C-c z" . zoom-window-zoom)))

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
	 ("C-c p f" . projectile-find-file))
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

(use-package docker-tramp
  :ensure t)

(provide 'editor-setup)
;;; editor-setup.el ends here
