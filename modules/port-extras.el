;; -*- lexical-binding: t; -*-

;; Code folding
(use-package origami
  :bind
  ("<f8>" . origami-toggle-node)
  :init
  (global-origami-mode 1))

;; Toggle between single quote and double quotes
(use-package toggle-quotes
  :bind
  ("C-'" . toggle-quotes))

;; Similar to surround plugin
(use-package embrace
  :bind
  ("C-," . embrace-commander))

;; Multiple cursors
(use-package multiple-cursors
  :bind ("C-S-c C-S-c" . mc/edit-lines))

;; Enable opening very large file
(use-package vlf
  :config
  (require 'vlf-setup))

;; Enable fill 85 column
;; (use-package fill-column-indicator)

(provide 'port-extras)
