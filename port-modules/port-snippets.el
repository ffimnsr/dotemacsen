;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :custom
  (yas-wrap-around-region t)
  :mode ("\\.yasnippet\\'" . snippet-mode)
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'port-snippets)
