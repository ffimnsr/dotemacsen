;; Traverse file by charaters
(use-package avy
  :custom
  (avy-style 'de-bruijn)
  :bind
  ([remap goto-char] . avy-goto-char-2)
  ([remap goto-line] . avy-goto-line)
  :config
  (avy-setup-default))

(provide 'avy-config)
