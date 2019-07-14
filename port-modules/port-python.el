;; -*- lexical-binding: t; -*-

(use-package python
  :mode "\\.py\\'"
  :interpreter "python")

(use-package elpy
  :after python
  :config
  (progn
    (elpy-enable)))

(provide 'port-python)
