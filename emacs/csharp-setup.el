(straight-use-package 'omnisharp)
(with-eval-after-load 'company
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))
