;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands company-ui)

(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates t)
  (company-lsp-async t))

(provide 'port-language-server)
