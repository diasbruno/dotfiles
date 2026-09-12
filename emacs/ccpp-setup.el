;;; ccpp-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


;; (add-to-list 'load-path "~/programming/c3-ts-mode/")

;; (require 'c3-ts-mode)
;; (setf lsp-c3-c3-language-server-path "/users/dias/programming/c3-lsp/server/bin/c3lsp")

(straight-use-package
 '(shader-mode :host github :repo "midnightSuyama/shader-mode"))

(straight-use-package
 '(clang-format :host github :repo "sonatard/clang-format"))

(require 'clang-format)
(setf clang-format-binary "/nix/store/j6rw6rmqj0gpck8ydrm8phz64kkfrzk9-clang-tools-21.1.8/bin/clang-format")

(provide 'ccpp-setup)
;;; ccpp-setup.el ends here
