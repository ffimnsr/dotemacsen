;; -*- lexical-binding: t; -*-

(use-package sh-script
  :mode
  ("\\.bashrc\\'" "\\.bash_profile\\'" "\\.env\\'")
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))

(provide 'port-shell)
