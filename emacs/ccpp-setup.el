;;; ccpp-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(add-to-list 'load-path "~/Programming/c3-ts-mode/")

(require 'c3-ts-mode)
(setf lsp-c3-c3-language-server-path "/Users/dias/Programming/c3-lsp/server/bin/c3lsp")

(provide 'ccpp-setup)
;;; ccpp-setup.el ends here
