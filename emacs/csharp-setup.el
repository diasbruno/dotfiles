(straight-use-package
 '(omnisharp :host github :repo "OmniSharp/omnisharp-emacs"))
(with-eval-after-load 'company
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))
