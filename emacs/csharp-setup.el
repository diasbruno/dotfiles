(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))
