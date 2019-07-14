;;; init.el --- Initialization file for Emacs 25
;;; Commentary:
;;;   ffimnsr <ffimnsr@gmail.com>

;; -*- lexical-binding: t; -*-

(unless (>= emacs-major-version 26)
  (error "Emacs version 26 or higher is required, you're running %s"
         emacs-version))

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
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

(load (concat user-emacs-directory "lib.el"))

(defconst default-font-size 12)
(defconst is-macos (eq system-type 'darwin))
(defconst port-modules-dir (concat user-emacs-directory "port-modules"))

;; Load emacs core
(use-feature emacs
  :custom
  (gc-cons-threshold (* 42 1024 1024))
  (load-prefer-newer t)                      ; Load newer files
  (history-length 128)
  (history-delete-duplicates t)
  (echo-keystrokes 1e-6)
  (ns-use-native-fullscreen nil)
  (ns-function-modifier 'control)
  (ns-pop-up-frames nil)
  (create-lockfiles nil)
  (disabled-command-function nil)            ; Enable all commands by default
  (delete-by-moving-to-trash t)
  (inhibit-startup-screen t)                 ; Disable startup screen with graphics
  (inhibit-startup-echo-area-message "")     ; Disable startup echo messages
  (initial-scratch-message nil)              ; Blank scratch buffer
  (initial-major-mode 'fundamental-mode)
  (visible-bell nil)                         ; Disable visual bell graphics
  (ring-bell-function #'ignore)              ; Disable audio bell
  (kill-buffer-query-functions nil)
  (standard-indent 2)
  (enable-recursive-minibuffers t)
  (ad-redefinition-action 'accept)
  :config
  (setq-default cursor-in-non-selected-windows nil  ; Remove cursor in inactive windows
                cursor-type 'bar             ; Use block cursor on active window
                indent-tabs-mode nil         ; Use spaces instead of tabs
                line-spacing 1
                c-basic-offset 2
                tab-width 2                  ; Set tab width as four spaces is a tab
                fill-column 85
                truncate-lines t)

  ;; Disable yes-or-no
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Default to UTF-8g
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)

  ;; Unset C-z that freezes emacs gui
  (global-unset-key (kbd "C-z"))

  ;; Remove overwrite-mode key
  (global-unset-key (kbd "<insert>"))
  (global-unset-key (kbd "<insertchar>"))

  ;; Quickly switch to previous buffer
  (global-set-key (kbd "C-6") 'mode-line-other-buffer)

  ;; Quiet startup
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Run a daemon incase
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package dash             ;; List manipulation
  :config (dash-enable-font-lock))

(use-package s)               ;; String manipulation
(use-package f)               ;; File manipulation
(use-package async)
(use-package mode-local)
(use-package hydra)
(use-package restart-emacs)

;; Load emacs config for desktop
(when (display-graphic-p)
  (use-feature frame
    :custom
    (blink-cursor-blinks 0)
    (frame-title-format "Equivalent Exchange")
    :init
    ;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (blink-cursor-mode)

    (when window-system
      (mapc (lambda (mode)
              (when (fboundp mode)
                (apply mode '(-1))))
            '(tool-bar-mode menu-bar-mode scroll-bar-mode)))
    (defun garbage-collect-when-frame-is-unfocused ()
      (unless (frame-focus-state)
        (garbage-collect)))))

(use-feature files
  :custom
  (require-final-newline t)
  (confirm-kill-emacs nil)                    ; Do not confirm before ending emacs session
  (confirm-kill-process nil)
  (confirm-nonexistent-file-or-buffer nil)    ; Disable annoying confirmation for not exist
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (large-file-warning-threshold 50000000)
  (kept-new-versions 10)
  (kept-old-versions 0)
  :config
  ;; Revert buffers to reflect external file changes
  (global-auto-revert-mode t))

(use-package savehist
  :custom
  (savehist-additional-variables
   '(search-ring regexp-search-ring))
  :config
  (savehist-mode))

(use-package saveplace
  :init (save-place-mode))

(use-package recentf
  :custom
  (recentf-auto-cleanup 200)
  (recentf-max-saved-items 200)
  :config
  (recentf-mode)
  (advice-add 'recentf-cleanup :around #'inhibit-message-in-minibuffer))

(use-feature simple
  :config
  (column-number-mode)
  (defun move-beginning-of-line-or-indentation (f &rest args)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (apply f args))))
  (advice-add 'backward-kill-word :around #'delete-region-instead-of-kill-region)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation)
  (advice-add 'beginning-of-visual-line :around #'move-beginning-of-line-or-indentation))

;; Easily move between panes
(use-feature windmove
  :config
  (windmove-default-keybindings 'meta))

;; Make buffer names distinguishable
(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

;; Make dired useable
(use-feature dired
  :custom
  (dired-create-destination-dirs t)
  (dired-dwim-target t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-use-ls-dired nil)
  (dired-auto-revert-buffer t))

;; Set side padding size
(use-feature fringe
  :config
  (fringe-mode '(1 . 1)))

;; Sub-word traversing
(use-package subword
  :diminish subword-mode
  :init (global-subword-mode))

;; Delete on type inside highlight
(use-package delsel
  :diminish delete-selection-mode
  :init (delete-selection-mode))

(use-package scratch
  :bind
  ("S-N" . scratch))

;; Load packages
(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/port-modules/"))

(require 'port-core)
(require 'port-ui)
(require 'port-osx-utils)
(require 'port-workspace)
(require 'port-vcs)
(require 'port-company)
;; (require 'port-prescient)
;; (require 'port-diff)
;; (require 'port-bookmarks)
(require 'port-search)
(require 'port-restclient)
(require 'port-language-server)
(require 'port-flycheck)
;; (require 'port-snippets)
;; (require 'port-other)
;; (require 'port-shell)
;; (require 'port-cplusplus)
;; (require 'port-csharp)
;; (require 'port-golang)
(require 'port-rust)
(require 'port-web)
(require 'port-javascript)
(require 'port-typescript)
;; (require 'port-solidity)
;; (require 'port-elixir)
;; (require 'port-dart)
;; (require 'port-python)
;; (require 'port-ui-ligatures)
;; (require 'port-image)
;; (require 'port-org)
;; (require 'port-postscript)
;; (require 'port-writer)
;; (require 'port-spell-check)
;; (require 'port-email)
(require 'port-recorded-macros)
;; (require 'port-unused)

(garbage-collect)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-title-format "Equivalent Exchange" t))
(put 'erase-buffer 'disabled nil)

;;; init.el ends here
