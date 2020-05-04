;; -*- lexical-binding: t; -*-

(use-package projectile
  :diminish projectile-mode
  :custom
  (projectile-enable-caching nil)
  (projectile-verbose nil)
  (projectile-indexing-method 'alien)
  (projectile-git-command "git ls-files -zco --exclude-standard")
  (projectile-git-submodule-command nil)
  (projectile-generic-command "fd -H --exclude .git -t f -c never -d 9 -0")
  (projectile-completion-system 'ivy)
  (projectile-require-project-root t)
  (projectile-switch-project-action #'projectile-dired)
  :config
  (setq projectile-globally-ignored-files
        (append '("*.o" "*.so" "*.csv" "*.tsv" "*~" "*.orig" "*#" "*.log")
                projectile-globally-ignored-files))
  (projectile-mode)
  (projectile-cleanup-known-projects))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode)
  :bind
  ("M-t" . counsel-projectile-find-file)
  ("C-c p" . counsel-projectile-switch-project))

(provide 'port-workspace)
