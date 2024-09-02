;; -*- lexical-binding: t; -*-

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-mode
  :custom
  (diff-font-lock-prettify t))

(use-package smerge-mode)

(provide 'port-diff)
