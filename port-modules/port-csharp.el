;; -*- lexical-binding: t; -*-

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package omnisharp
  :after csharp-mode
  :hook (csharp-mode . configure-omnisharp)
  :config
  (defun configure-omnisharp ()
    (omnisharp-mode)
    (add-to-list 'company-backends 'company-omnisharp)
    (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
    (local-set-key (kbd "C-c C-c") 'recompile)))

(provide 'port-csharp)
