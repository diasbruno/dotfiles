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
  :ensure t
  :custom ((erlang-indent-level 2)
           (inferior-erlang-machine-options '("-sname" "emacs"))
           (inferior-erlang-prompt-timeout t)))

(use-package alchemist
  :ensure t)

(defun enable-paredit ()
  "Enable paredit-mode."
  (call-interactively #'paredit-mode))

(use-package lfe-mode
  :ensure t
  :custom ((indent-tabs-mode nil))
  :hook ((lfe-mode-hook . #'enable-paredit)))

(provide 'erlang-setup)
;;; erlang-setup.el ends here
