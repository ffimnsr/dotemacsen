;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :hook
  (c-mode . (lambda () (c-set-style "bsd")))
  (java-mode . (lambda () (c-set-style "bsd")))
  :config
  (setq tab-width 2)
  (setq c-basic-offset 2))

(provide 'port-cplusplus)
