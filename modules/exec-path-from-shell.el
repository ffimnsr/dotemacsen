
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :custom
  (exec-path-from-shell-check-startup-file nil)
  :config (exec-path-from-shell-initialize))

(provide 'exec-path-from-shell-config)
