;; -*- lexical-binding: t; -*-

(use-package magit
  :custom
  (magit-log-section-commit-count 0)
  (magit-branch-prefer-remote-upstream t)
  (magit-log-auto-more t)
  (magit-repository-directories projectile-known-projects)
  (magit-diff-refine-hunk 'all)
  (magit-no-confirm t)
  :config
  (global-magit-file-mode)
  (after alert
    (defun magit-process-alert-after-finish-in-background (f &rest args)
      (let* ((process (nth 0 args))
             (event (nth 1 args))
             (buf (process-get process 'command-buf))
             (buff-name (buffer-name buf)))
        (when (and buff-name (stringp event) (s-match "magit" buff-name) (s-match "finished" event))
          (alert-after-finish-in-background buf (concat (capitalize (process-name process)) " finished")))
        (apply f (list process event))))
    (advice-add 'magit-process-sentinel :around #'magit-process-alert-after-finish-in-background)))

(use-package magit-popup)

(use-package magit-todos
  :config
  (magit-todos-mode)
  :custom
  (magit-todos-max-items 30))

(use-feature vc-git
  :custom
  (vc-git-diff-switches '("--histogram")))

(use-feature vc-hooks
  :custom
  (vc-follow-symlinks t))

;; Helper to create a gist file
(use-package gist)

(provide 'port-vcs)
