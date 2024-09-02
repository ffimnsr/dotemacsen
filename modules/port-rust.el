;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :hook
  (rust-mode . flycheck-mode)
  (rust-mode . lsp-deferred))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package cargo
  :after rust-mode)

(provide 'port-rust)
