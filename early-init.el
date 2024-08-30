;; -*- lexical-binding: t; -*-

(unless (>= emacs-major-version 26)
  (error "Emacs version 26 or higher is required, you're running %s"
         emacs-version))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package no-littering)
(use-package use-package-chords)
(use-package use-package-ensure-system-package)
(use-package use-package-hydra)
(use-package diminish)
(use-package bind-key)

(setq straight-vc-git-default-protocol 'ssh)
(setq straight-vc-git-force-protocol t)
