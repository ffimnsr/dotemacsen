;; -*- lexical-binding: t; -*-

(use-package shell
  :custom
  (explicit-shell-file-name (getenv "SHELL"))
  :config
  (defun make-shell-command-behave-interactively (f &rest args)
    (let ((shell-command-switch "-ic"))
      (apply f args)))
  (advice-add 'shell-command :around #'make-shell-command-behave-interactively)
  (advice-add 'start-process-shell-command :around #'make-shell-command-behave-interactively))

(use-package sh-script
  :mode
  ("\\.bashrc\\'" "\\.bash_profile\\'" "\\.env\\'")
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))

(provide 'port-shell)
