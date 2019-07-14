;; -*- lexical-binding: t; -*-

(use-package typescript-mode
  :hook
  (typescript-mode . lsp-deferred)
  :custom
  (typescript-indent-level 2))

;; TODO: Disabled due to LSP
;; (use-package tide
;;   :diminish tide-mode
;;   :after typescript-mode
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)))

(provide 'port-typescript)
