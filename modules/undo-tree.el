
;; Visualize the whole undo history in buffer as a tree, and you can access anywhere in it.
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :bind
  ("C-/" . undo-tree-undo)
  ("C-S-/" . undo-tree-redo)
  :custom
  ;; Show relative times in the undo tree visualizer.
  (undo-tree-visualizer-timestamps nil)
  ;; Show diffs when browsing through the undo tree.
  (undo-tree-visualizer-diff nil)
  ;; Save history to a file.
  (undo-tree-auto-save-history nil))

(provide 'undo-tree-config)
