;; -*- lexical-binding: t; -*-

(use-package elixir-mode
  :mode "\\.exs?\\'"
  :hook
  (elixir-mode . flycheck-mode)
  (elixir-mode . highlight-indent-guides-mode)
  (before-save . whitespace-cleanup))

(provide 'port-elixir)
