;; -*- lexical-binding: t; -*-

(use-package typescript-mode
  :ensure-system-package
  (typescript-language-server . "npm i -g typescript-language-server")
  :hook
  (typescript-mode . lsp-deferred)
  :custom
  (typescript-indent-level 2))

(provide 'port-typescript)
