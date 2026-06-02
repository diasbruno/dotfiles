;;; swift-setup.el -- My emacs configuration.
;;;
;;; Commentary:
;;;
;;; Nothing to see here.
;;;
;;; Code:
;;;


(straight-use-package
 '(lsp-sourcekit :host github :repo "emacs-lsp/lsp-sourcekit"))
(with-eval-after-load 'lsp-mode
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(straight-use-package
 '(swift-mode :host github :repo "swift-emacs/swift-mode"))

(provide 'swift-setup)
;;; swift-setup.el ends here
