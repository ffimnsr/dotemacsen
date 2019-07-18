;; -*- lexical-binding: t; -*-

(use-package projectile
  :diminish projectile-mode
  :custom
  (projectile-enable-caching nil)
  (projectile-verbose nil)
  (Projectile-Completion-System 'ivy)
  (projectile-require-project-root nil)
  :config
  (setq projectile-globally-ignored-files
        (append '("*.txt" "*.o" "*.so" "*.csv" "*.tsv" "*~" "*.orig" "*#" "*.log")
                projectile-globally-ignored-files))
  (projectile-mode)
  (projectile-cleanup-known-projects))

(use-package perspective
  :init
  (persp-mode 1)
  :config
  (persp-turn-off-modestring))

(use-package persp-projectile
  :after projectile)

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode)
  :bind
  ("M-t" . counsel-projectile-find-file)
  ("M-p" . counsel-projectile-switch-project))

(provide 'port-workspace)
