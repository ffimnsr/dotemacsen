;; -*- lexical-binding: t; -*-

(when is-macos
  (add-to-list 'exec-path "/usr/local/bin")
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        mac-function-modifer 'control)
  (global-set-key "\M-`" 'other-frame))

(use-package terminal-here
  :if is-macos)

(use-package reveal-in-osx-finder
  :if is-macos)

(provide 'port-osx-utils)
