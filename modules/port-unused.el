;; -*- lexical-binding: t; -*-

;; Enable window undo and redo using C-c <left>
(use-feature winner
  :config (winner-mode 1))

(use-feature flymake
  :custom
  (flymake-start-syntax-check-on-newline nil))

(provide 'port-unused)
