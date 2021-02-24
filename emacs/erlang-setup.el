;;; erlang-setup.el -- Erlang/elixir/lfe modes.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;

(require 'use-package)

(defvar *erlang-asdf* "/dias/asdf/installs/erlang/22.2")

(add-to-list 'exec-path (concat *erlang-asdf* "/bin"))

;; (use-package erlang
;;   :load-path "/dias/asdf/installs/erlang/22.2/lib/tools-3.3/emacs"
;;   :custom ((erlang-root-dir *erlang-asdf*)
;;            (erlang-man-root-dir (concat *erlang-asdf* "/man"))
;;            (erlang-indent-level 2)
;;            (inferior-erlang-machine-options '("-sname" "emacs"))
;;            (inferior-erlang-prompt-timeout t))
;;   :config (require 'erlang-start))

(use-package alchemist
  :ensure t)

(use-package lfe-mode
  :ensure t)

(provide 'erlang-setup)
;;; erlang-setup.el ends here
