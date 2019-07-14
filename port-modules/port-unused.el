;; -*- lexical-binding: t; -*-

;; Enable window undo and redo using C-c <left>
(use-feature winner
  :config (winner-mode 1))

(use-feature compile
  :custom
  (compile-command "make")
  (compilation-always-kill t)
  (compilation-read-commmand nil)             ; Disable confirmation of compile command
  (compilation-ask-about-save nil))

(use-package ansi-color
  :hook
  (compilation-filter . colorize-compilation-buffer)
  :config
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(use-feature flymake
  :custom
  (flymake-start-syntax-check-on-newline nil))

(provide 'port-unused)
