;;; erlang-setup.el -- Erlang/elixir/lfe modes.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'use-package)

(use-package erlang
  
  :custom ((erlang-indent-level 2)
           (inferior-erlang-machine-options '("-sname" "emacs"))
           (inferior-erlang-prompt-timeout t)))

(defun enable-paredit ()
  "Enable paredit-mode."
  (call-interactively #'paredit-mode))

(use-package lfe-mode
  
  :custom ((indent-tabs-mode nil))
  :hook ((lfe-mode-hook . #'enable-paredit)))

(use-package elixir-yasnippets
  )

(require 'dap-elixir)

(use-package elixir-ts-mode
  
  :config
  (add-to-list 'auto-mode-alist '("\\.exs?" . elixir-ts-mode)))

(use-package mix
  )

(setf lsp-elixir-server-command '("/usr/local/src/elixir-ls/scripts/language_server.sh"))
; (setf lsp-elixir-server-command '("launch.sh"))

(use-package esc-elixir
  :load-path "/usr/local/src/emacs-esc/"
  :config
  (require 'esc-elixir))

(provide 'erlang-setup)
;;; erlang-setup.el ends here
