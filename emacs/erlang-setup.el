;;; erlang-setup.el -- Erlang/elixir/lfe modes.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(straight-use-package
 '(erlang :host github :repo "erlang/otp"
          :files ("lib/tools/emacs/*.el")))
(setq erlang-indent-level 2
      inferior-erlang-machine-options '("-sname" "emacs")
      inferior-erlang-prompt-timeout t)

(defun enable-paredit ()
  "Enable paredit-mode."
  (call-interactively #'paredit-mode))

(straight-use-package
 '(lfe-mode :host github :repo "lfe-lang/lfe-mode"))
(setq indent-tabs-mode nil)
(add-hook 'lfe-mode-hook #'enable-paredit)

(straight-use-package
 '(elixir-yasnippets :host github :repo "smerriman/elixir-yasnippets"))

(require 'dap-elixir)

(straight-use-package
 '(elixir-ts-mode :host github :repo "wkirschbaum/elixir-ts-mode"))
(add-to-list 'auto-mode-alist '("\\.exs?" . elixir-ts-mode))

(straight-use-package
 '(mix :host github :repo "ayrat555/mix.el"))

(setf lsp-elixir-server-command '("/usr/local/src/elixir-ls/scripts/language_server.sh"))
; (setf lsp-elixir-server-command '("launch.sh"))

(add-to-list 'load-path "/usr/local/src/emacs-esc/")
(require 'esc-elixir)

(provide 'erlang-setup)
;;; erlang-setup.el ends here
