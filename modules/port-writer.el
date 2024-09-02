;; -*- lexical-binding: t; -*-x

(use-package writegood-mode
  :diminish writegood-mode
  :hook (text-mode . writegood-mode)
  :bind
  ("C-c g" . writegood-mode)
  ("C-c C-g g" . writegood-grade-level)
  ("C-c C-g e" . writegood-reading-ease))

(use-package adoc-mode
  :hook (adoc-mode . writegood-mode)
  :mode "\\.adoc\\'")

(flycheck-define-checker proselint
                         "A linter for prose."
                         :command ("proselint" source-inplace)
                         :error-patterns
                         ((warning line-start (file-name) ":" line ":" column ": "
                                   (id (one-or-more (not (any " "))))
                                   (message) line-end))
                         :modes (text-mode adoc-mode))

(add-to-list 'flycheck-checkers 'proselint)

(provide 'port-writer)
