;; -*- lexical-binding: t; -*-

(use-package ivy
  :diminish ivy-mode
  :custom
  (ivy-extra-directories nil)
  (ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (counsel-rg . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-format-function #'ivy-format-function-arrow)
  :init
  (ivy-mode)
  :config
  ;; Can not exit minibuffer - https://github.com/abo-abo/swiper/issues/1953
  (defvar ivy-recursive-restore-in-progress nil)
  (defun ivy-note-when-inside-recursive-restore (orig-fun &rest args)
    (let ((ivy-recursive-restore-in-progress t))
      (apply orig-fun args)))
  (defun ivy-no-read-while-exiting-recursion (orig-fun &rest args)
    (if ivy-recursive-restore-in-progress
        (error "Cannot use `ivy-read' while restoring recursive state")
      (apply orig-fun args)))
  (advice-add 'ivy-recursive-restore :around #'ivy-note-when-inside-recursive-restore)
  (advice-add 'ivy-read :around #'ivy-no-read-while-exiting-recursion))

(use-package ivy-hydra
  :after ivy)

(use-package ivy-xref
  :after ivy
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :bind
  ([remap execute-extended-command] . counsel-M-x))

(use-package swiper
  :bind
  ([remap isearch-forward] . swiper)
  ([remap isearch-backward] . swiper))

(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-setup-side-window-right)
  :config
  (which-key-mode))

(use-package imenu
  :custom
  (imenu-auto-rescan t))

(use-package imenu-anywhere
  :commands ivy-imenu-anywhere)

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-file nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-idle-delay 2))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :bind
  ("C-/" . undo-tree-undo)
  ("C-S-/" . undo-tree-redo)
  :custom
  (undo-tree-visualizer-timestamps nil)
  (undo-tree-visualizer-diff nil)
  (undo-tree-auto-save-history nil))

;; Highlight paired parenthesis
(use-package paren
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package electric
  :custom
  (electric-quote-string t)
  (electric-quote-context-sensitive t))

;; Display indent guides
(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode)

;; Traverse file by charaters
(use-package avy
  :custom
  (avy-style 'de-bruijn)
  :bind
  ([remap goto-char] . avy-goto-char-2)
  ([remap goto-line] . avy-goto-line)
  :config
  (avy-setup-default))

;; Free keys
(use-package free-keys)

(provide 'port-core)
