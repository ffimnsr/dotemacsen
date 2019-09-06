;; -*- lexical-binding: t; -*-

(use-package typescript-mode
  :ensure-system-package
  (typescript-language-server . "npm i -g typescript-language-server")
  :hook
  ;; (js2-mode . prettier-js-mode)
  (typescript-mode . lsp-deferred)
  :custom
  (typescript-indent-level 2))

(provide 'port-typescript)
