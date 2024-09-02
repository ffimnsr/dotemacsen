;; Allows you to define key-chord bindings for use-package declarations in the same manner as the :bind keyword.
;; https://github.com/jwiegley/use-package?tab=readme-ov-file#use-package-chords
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(provide 'use-package-chords-config)
