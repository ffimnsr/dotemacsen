;; -*- lexical-binding: t; -*-

(use-package js
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2))

(use-package js2-mode
  :after js
  :mode "\\.js\\'"
  :interpreter "node"
  :bind
  (:map js-mode-map
        ("," . self-with-space)
        ("=" . pad-equals)
        (":" . self-with-space))
  :custom
  (js2-concat-multiline-strings 'eol)
  (js2-mode-assume-strict t)
  (js2-strict-trailing-comma-warning nil)
  (js2-strict-inconsistent-return-warning nil)
  :config
  (setenv "NODE_NO_READLINE" "1"))

(use-package rjsx-mode
  :after js2-mode
  :mode
  ("\\.jsx\\'" "\\.tjs\\'")
  :config
  (setq-mode-local rjsx-mode emmet-expand-jsx-className? t)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))

(provide 'port-javascript)
