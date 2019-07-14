;; -*- lexical-binding: t; -*-

(use-package elixir-mode
  :mode "\\.exs?\\'"
  :hook
  (elixir-mode . flycheck-mode)
  (elixir-mode . highlight-indent-guides-mode)  
  (before-save . whitespace-cleanup))

(use-package flycheck-credo
  :after elixir-mode
  :hook
  (flycheck-mode . flycheck-credo-setup))

;; TODO: Disabled due to LSP
;; (use-package alchemist
;;   :diminish alchemist-mode
;;   :after elixir-mode)

(provide 'port-elixir)
