
;; -*- lexical-binding: t; -*-

(use-package go-mode
  :hook
  (before-save . gofmt-before-save)
  :custom
  (gofmt-command "goimports"))

(provide 'port-golang)
