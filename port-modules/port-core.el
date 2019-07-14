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
  (ivy-mode))

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
  (exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package direnv
  :ensure-system-package direnv
  :custom
  (direnv-always-show-summary nil)
  :init
  (direnv-mode)
  (add-to-list 'direnv-non-file-modes 'comint-mode)
  (add-to-list 'direnv-non-file-modes 'shell-mode))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

(use-package alert
  :custom
  (alert-default-style (if is-macos 'osx-notifier 'message))
  :config
  (defun alert-after-finish-in-background (buf str)
    (unless (get-buffer-window buf 'visible)
      (alert str :buffer buf))))

(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-idle-delay 2))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

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

(use-package bash-completion
  :init
  (bash-completion-setup))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Display indent guides
(use-package highlight-indent-guides
 :diminish highlight-indent-guides-mode)

;; Traverse file by charaters
(use-package avy)

(provide 'port-core)
