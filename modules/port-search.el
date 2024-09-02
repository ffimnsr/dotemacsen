;; -*- lexical-binding: t; -*-

(use-package ag
  :ensure-system-package ag)

(use-package rg
  :ensure-system-package rg)

(use-package wgrep-ag
  :after ag
  :custom
  (wgrep-auto-save-buffer t))

(provide 'port-search)
