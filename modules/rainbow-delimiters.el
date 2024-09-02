;; -*- lexical-binding: t; -*-

;; Highlights parentheses, brackets, and braces according to their depth.

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom
  (rainbow-delimiters-max-face-count 6))

(provide 'rainbow-delimiters-config)
