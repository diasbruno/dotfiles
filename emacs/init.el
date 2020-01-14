;;; init.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(setq package-enable-at-startup nil)

(require 'package)
(package-initialize)

(mapc (lambda (source)
        (add-to-list 'package-archives source t))
      '(("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; auto-save
(setq backup-directory-alist `(("." . "/dias/.emacs.d/auto-save")))

(defun global-key-bind (key fn)
  "Define a global bind for KEY to execute FN."
  (global-set-key (kbd key) fn))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(require 'use-package)

;; system packages.
(use-package system-packages
  :ensure t)

(use-package yafolding
  :ensure t
  :bind (("C-c f" . yafolding-toggle-element)
         ("C-c F" . yafolding-toggle-all)))

(use-package yasnippet
  :ensure t
  :config (progn
            (push "/dias/dotfiles/snippets" yas-snippet-dirs)
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
  :bind (("M-o" . 'ace-window)))

;; just like tmux.
(use-package zoom-window
  :ensure t
  :bind (("C-M-o" . zoom-window-zoom)
         ("C-M-p" . zoom-window-next)))

;; enable to jump on a buffer.
(use-package ivy
  :ensure t
  :custom ((ivy-use-virtual-buffers t)
           (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

(use-package counsel
  :ensure t
  :bind (("C-c s" . swiper)
         ("C-c x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

;; manage projects.
(use-package projectile
  :ensure t
  :custom ((projectile-completion-system 'ivy))
  :bind (("C-c p k" . projectile-kill-buffers)
         ("C-c p t" . projectile-run-shell)
         ("C-c p d" . projectile-dired))
  :config (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :bind ("C-c p f" . counsel-projectile-find-file)
  ("C-c p p" . counsel-projectile-switch-project)
  ("C-c p s" . counsel-projectile-grep))

(use-package counsel-tramp
  :ensure t)

;; ansible
(use-package ansible
  :ensure t)

;; git stuff
(use-package magit
  :ensure t
  :bind (("C-c m" . 'magit-status)))

;; display the diff on each changed line.
(use-package diff-hl
  :ensure t
  :config (global-diff-hl-mode))

;; completion system.
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package realgud
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package docker-tramp
  :ensure t)

(use-package sourcetrail
  :ensure t)

;; languages

;; yaml mode

(use-package yaml-mode
  :ensure t)

;; json mode

(use-package json-mode
  :ensure t
  :custom (js-indent-level 2))

;; yacc/lexx/bison mode

(use-package bison-mode
  :ensure t)

;; haskell mode

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . interactive-haskell-mode))
  :bind (("C-c C-z" . #'haskell-process-load)
         ("C-." . #'haskell-process-cabal-build))
  :custom ((haskell-process-suggest-remove-import-lines t)
           (haskell-process-auto-import-loaded-modules t)
           (haskell-process-log t))
  :config
  (add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends)))))

;; c/c++ mode

(use-package irony
  :ensure t
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))

(use-package company-irony
  :ensure t
  :config (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; (e)lisp mode

;; enable paredit when possible.
(use-package paredit
  :ensure t
  :hook ((lisp-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)))

(use-package sly
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; erlang/elixir/lfe mode

(defvar *erlang-asdf* "/dias/asdf/installs/erlang/22.2")
(add-to-list 'exec-path (concat *erlang-asdf* "/bin"))

(use-package erlang
  :load-path "/dias/asdf/installs/erlang/22.2/lib/tools-3.3/emacs"
  :custom ((erlang-root-dir *erlang-asdf*)
           (erlang-man-root-dir (concat *erlang-asdf* "/man"))
           (erlang-indent-level 2)
           (inferior-erlang-machine-options '("-sname" "emacs"))
           (inferior-erlang-prompt-timeout t))
  :config (progn
            (require 'erlang-start)))

(use-package alchemist
  :ensure t)

(use-package lfe-mode
  :ensure t)

;; python mode

(use-package elpy
  :ensure t)

;; css mode

(use-package scss-mode
  :ensure t
  :custom (css-indent-offset 2))

(use-package sass-mode
  :ensure t
  :custom (css-indent-offset 2))

;; javascript mode

(use-package indium
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :custom ((js2-indent-level 2)))

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

(use-package typescript-mode
  :ensure t
  :custom ((typescript-indent-level 2)))

(use-package vue-mode
  :ensure t
  :custom ((js-indent-level 2)
           (js2-indent-level 2)
           (vue-html-extra-indent 2))
  :bind (("C-c t" . #'vue-js-indentation)))

;; theme

(load-file "/dias/dotfiles/emacs/bw-theme.el")
(use-package bw-theme
  :custom ((bw-foreground "white")
           (bw-background "black"))
  :config (load-theme 'bw t))

;; key bindings.
(global-key-bind (kbd "C-c q") 'whitespace-cleanup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bw-background "black")
 '(bw-foreground "white")
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("48169b8c406d2633ded74ce35ae5797ca9821e76079702030414a29190592d27" "59befc7efc9cd5a90529cfc1e5929d627a7f5c816aa5a8bdad080c4392b3cb1d" default)))
 '(erlang-indent-level 2)
 '(erlang-man-root-dir "/dias/asdf/installs/erlang/22.2/man" t)
 '(erlang-root-dir "/dias/asdf/installs/erlang/22.2")
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flymake-no-changes-timeout nil)
 '(flymake-start-syntax-check-on-newline nil)
 '(haskell-compile-cabal-build-alt-command "stack clean -s && stack build --ghc-option=-ferror-spans")
 '(haskell-compile-cabal-build-command "stack build --ghc-option=-ferror-spans")
 '(haskell-process-auto-import-loaded-modules t t nil "Customized with use-package haskell-mode")
 '(haskell-process-log t t nil "Customized with use-package haskell-mode")
 '(haskell-process-suggest-remove-import-lines t t nil "Customized with use-package haskell-mode")
 '(indent-tab-mode nil)
 '(indent-tabs-mode nil)
 '(inferior-erlang-machine-options (quote ("-sname" "emacs")) t)
 '(inferior-erlang-prompt-timeout t t)
 '(inhibit-startup-screen t)
 '(ivy-re-builders-alist (quote ((t . ivy--regex-fuzzy))) t)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(js2-indent-level 2 t)
 '(json-mode-auto-mode-list (quote (".babelrc" ".bowerrc" "composer.lock" ".eslintrc")))
 '(lsp-haskell-process-path-hie "/home/dias/.local/bin/hie")
 '(lsp-haskell-process-wrapper-function (quote identity))
 '(lsp-prefer-flymake :none)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (scss-mode sass-mode typescript-mode rainbow-delimiters rainbow-delimiters-mode company-ghc yaml-mode flycheck-hdevtools elpy sourcetrail bison-mode edts js3-mode json-mode indium counsel-gtags company-lsp lsp-ui lsp-haskell lsp-mode lsp attrap dockerfile-mode docker-tramp counsel-tramp counsel-projectile vue-mode rjsx-mode cloud-theme sly realgud company-irony-c-headers irony-eldoc flycheck-irony company-irony irony company-c-headers helm-ggtags ggtags helm-projectile helm-system-packages helm company-distel lfe-mode flycheck company-erlang erlang zoom-window yasnippet yafolding which-key use-package system-packages projectile paredit multiple-cursors magit diff-hl ansible alchemist ace-window)))
 '(projectile-completion-system (quote ivy))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(vue-modes
   (quote
    ((:type template :name nil :mode vue-html-mode)
     (:type template :name html :mode vue-html-mode)
     (:type template :name jade :mode jade-mode)
     (:type template :name pug :mode pug-mode)
     (:type template :name slm :mode slim-mode)
     (:type template :name slim :mode slim-mode)
     (:type script :name nil :mode js2-mode)
     (:type script :name js :mode js2-mode)
     (:type script :name es6 :mode js2-mode)
     (:type script :name babel :mode js2-mode)
     (:type script :name coffee :mode coffee-mode)
     (:type script :name ts :mode typescript-mode)
     (:type script :name typescript :mode typescript-mode)
     (:type script :name tsx :mode typescript-tsx-mode)
     (:type style :name nil :mode css-mode)
     (:type style :name css :mode css-mode)
     (:type style :name stylus :mode stylus-mode)
     (:type style :name less :mode less-css-mode)
     (:type style :name postcss :mode css-mode)
     (:type style :name scss :mode scss-mode)
     (:type style :name sass :mode sass-mode)
     (:type i18n :name nil :mode json-mode)
     (:type i18n :name json :mode json-mode)
     (:type i18n :name yaml :mode yaml-mode)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
