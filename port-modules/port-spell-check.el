;; -*- lexical-binding: t; -*-

;; Spell checking
(use-package flyspell
  :hook
  ((org-mode markdown-mode git-commit-mode) . flyspell-mode))

(use-package flyspell-correct-ivy
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-previous-word-generic)))

(provide 'port-spell-check)
