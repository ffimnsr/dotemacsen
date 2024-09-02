;; Displays available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :custom
  ;; Popup side window on bottom.
  (which-key-setup-side-window-bottom)
  :config
  (which-key-mode))

(provide 'which-key-config)
