;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :hook
  (rust-mode . flycheck-mode)
  (rust-mode . lsp-deferred))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package cargo
  :after rust-mode)

;; TODO: Disabled due to LSP
;; (use-package racer
;;   :diminish racer-mode
;;   :after (rust-mode company eldoc)
;;   :init
;;   (setq racer-rust-src-path
;;         (concat (getenv "HOME")
;;                 "/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
;;   (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
;;   :hook ((rust-mode . racer-mode)
;;          (racer-mode . company-mode)
;;          (racer-mode . eldoc-mode)))

(provide 'port-rust)
