;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (add-to-list 'exec-path "~/.emacs.d/etc/elixir-ls")
  :custom
  (lsp-enable-snippet nil)
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  (lsp-xml-jar-file "~/.emacs.d/etc/org.eclipse.lsp4xml-0.3.0-uber.jar"))

(use-package lsp-ui
  :commands lsp-ui
  :config
  (lsp-ui-imenu-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable nil))

(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates t)
  (company-lsp-async t))

(provide 'port-language-server)
