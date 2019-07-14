;; -*- lexical-binding: t; -*-

(use-package emmet-mode
  :hook sgml-mode
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package web-mode
  :mode
  ("\\.eex\\'" "\\.tsx\\'")
  :hook
  (web-mode . emmet-mode)
  (web-mode . highlight-indent-guides-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-element-highlight t)
  :config
  (setq-mode-local web-mode emmet-expand-jsx-className? nil)  
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package css-mode
  :hook
  (css-mode . emmet-mode)
  :custom
  (css-indent-offset 2))

(use-package scss-mode
  :hook
  (scss-mode . emmet-mode))

(provide 'port-web)
