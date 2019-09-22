;; -*- lexical-binding: t; -*-

(use-package projectile
  :diminish projectile-mode
  :custom
  (projectile-enable-caching nil)
  (projectile-verbose nil)
  (projectile-indexing-method 'alien)
  (projectile-git-command "git ls-files -zco --exclude-from=.projectile --exclude-standard")
  (projectile-git-submodule-command nil)
  (projectile-generic-command "fd -H --exclude .git --ignore-file .projectile -t f -c never -d 9 -0")
  (projectile-completion-system 'ivy)
  (projectile-require-project-root t)
  (projectile-switch-project-action #'projectile-dired)
  :config
  (setq projectile-globally-ignored-files
        (append '("*.txt" "*.o" "*.so" "*.csv" "*.tsv" "*~" "*.orig" "*#" "*.log")
                projectile-globally-ignored-files))
  (projectile-mode)
  (projectile-cleanup-known-projects))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode)
  :bind
  ("M-t" . counsel-projectile-find-file)
  ("M-p" . counsel-projectile-switch-project))

;; Disabled due to high CPU usage
;; (use-package perspective
;;   :init
;;   (persp-mode 1)
;;   :config
;;   (persp-turn-off-modestring))

;; (use-package persp-projectile
;;   :after projectile)

;; ;; Patch and connect perspective and counsel
;; (with-eval-after-load 'perspective
;;   (defun ivy-switch-to-buffer ()
;;     "Switch to another buffer in the CURRENT PERSP."
;;     (interactive)
;;     (if (not (bound-and-true-p persp-mode))
;;         (ivy-switch-buffer)
;;       (setq this-command #'ivy-switch-buffer)
;;       (ivy-read "Switch to buffer: " (remove nil (mapcar 'buffer-name (persp-buffers (persp-curr))))
;;                 :keymap ivy-switch-buffer-map
;;                 :preselect (buffer-name (other-buffer (current-buffer)))
;;                 :action #'ivy--switch-buffer-action
;;                 :matcher #'ivy--switch-buffer-matcher
;;                 :caller 'ivy-switch-buffer)))
;;   (with-eval-after-load 'ivy
;;     (define-key ivy-mode-map (kbd "C-x b") 'ivy-switch-to-buffer))

;;   (defun ivy-switch-buffer-with-persp (&optional _)
;;     "Clone from persp-switch-to-buffer."
;;     (interactive)
;;     (let (buffer)
;;       (setq buffer (window-normalize-buffer-to-switch-to (read-buffer-to-switch "Switch to buffer: ")))
;;       (if (memq buffer (persp-buffers (persp-curr)))
;;           (switch-to-buffer buffer)
;;         (let ((other-persp (persp-buffer-in-other-p buffer)))
;;           (when (eq (car-safe other-persp) (selected-frame))
;;             (persp-switch (cdr other-persp)))
;;           (switch-to-buffer buffer)))))
;;   (with-eval-after-load 'ivy
;;     (ivy-add-actions
;;      'ivy-switch-buffer
;;      '(("p" ivy-switch-buffer-with-persp "persp-switch-to-buffer"))))

;;   ;; find file with perspective and projectile
;;   (defun counsel-find-file-action (x)
;;     "Find file X."
;;     (with-ivy-window
;;       (if (and counsel-find-file-speedup-remote
;;                (file-remote-p ivy--directory))
;;           (let ((find-file-hook nil))
;;             (find-file (expand-file-name x ivy--directory)))
;;         (if (and (bound-and-true-p persp-mode) (bound-and-true-p projectile-mode))
;;             (let (project-name (project-name-root (projectile-project-root (expand-file-name x))))
;;               (when project-name-root
;;                 (setq project-name (funcall projectile-project-name-function project-name-root))
;;                 (persp-switch project-name))))
;;         (find-file (expand-file-name x ivy--directory)))))

;;   ;; counsel-projectile with perspective
;;   (if (fboundp 'counsel-projectile)
;;       (defun counsel-projectile-switch-project-action (project)
;;         "Jump to a file or buffer in PROJECT."
;;         (if (bound-and-true-p persp-mode)
;;             (let* ((projectile-switch-project-action
;;                     (lambda ()
;;                       (counsel-projectile ivy-current-prefix-arg)))
;;                    (name (or projectile-project-name
;;                              (funcall projectile-project-name-function project)))
;;                    (persp (gethash name (perspectives-hash))))
;;               (cond
;;                ;; project-specific perspective already exists
;;                ((and persp (not (equal persp (persp-curr))))
;;                 (persp-switch name))
;;                ;; persp exists but not match with projectile-name
;;                ((and persp (not (equal persp name)))
;;                 (persp-switch name)
;;                 (counsel-projectile-switch-project-by-name project))
;;                ;; project-specific perspective doesn't exist
;;                ((not persp)
;;                 (persp-switch name)
;;                 (counsel-projectile-switch-project-by-name project))))
;;           (let ((projectile-switch-project-action
;;                  (lambda ()
;;                    (counsel-projectile ivy-current-prefix-arg))))
;;             (counsel-projectile-switch-project-by-name project))))))

;; (with-eval-after-load 'counsel-projectile
;;   (defun counsel-projectile-find-file-action-find-file-jump (file)
;;     "Call `counsel-find-file' from FILE's directory."
;;     (let* ((f (projectile-expand-root file))
;;            (default-directory (file-name-directory f)))
;;       (counsel-file-jump)))
;;   (ivy-add-actions
;;    'counsel-projectile
;;    '(("f" counsel-projectile-find-file-action-find-file-jump
;;       "counsel-file-jump")))
;;   (ivy-add-actions
;;    'counsel-projectile-find-file
;;    '(("f" counsel-projectile-find-file-action-find-file-jump
;;       "counsel-file-jump"))))

(provide 'port-workspace)
