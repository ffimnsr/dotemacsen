;; -*- lexical-binding: t; -*-

(use-package bm
  :bind
  ("<f2>" . bm-next)
  ("S-<f2>" . bm-previous)
  ("C-<f2>" . bm-toggle)
  :custom
  (temporary-bookmark-p t)
  (bm-repository-file "~/.emacs.d/var/bm-repository")
  (bm-buffer-persistence nil))

(provide 'port-bookmarks)
