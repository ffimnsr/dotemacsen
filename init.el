;;; init.el --- Initialization file for Emacs 25
;;; Commentary:
;;;   ffimnsr <ffimnsr@gmail.com>

;; -*- lexical-binding: t; -*-

(unless (>= emacs-major-version 29)
  (error "Emacs version 29 or higher is required, you're running %s"
         emacs-version))

;;; Code:

(setq gc-cons-threshold (* 100 1024 1024))

(setq straight-use-package-by-default t)

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

;; Load packages
(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/modules/"))

(use-package hydra)                           ; Make bindings that stick around.
(use-package restart-emacs)                   ; Restart emacs from within emacs

(require 'diminish-config)
(require 'bind-key-config)
(require 'key-chord-config)
(require 'no-littering-config)
(require 'use-package-ensure-config)
(require 'use-package-ensure-system-package-config)
(require 'use-package-chords-config)
(require 'use-package-hydra-config)

(load (concat user-emacs-directory "lib.el"))

(defconst default-font-size 12)
(defconst is-macos (eq system-type 'darwin))
(defconst modules-dir (concat user-emacs-directory "modules"))

;; Load emacs core
(use-feature emacs
  :custom
  (load-prefer-newer t)                      ; Load newer files
  (history-length 128)                       ; Set history length
  (history-delete-duplicates t)              ; Delete duplicate history
  (echo-keystrokes 1e-6)                     ; Show keystrokes immediately
  (ns-use-native-fullscreen nil)
  (ns-function-modifier 'control)
  (ns-pop-up-frames nil)
  (create-lockfiles nil)                     ; Disable lock files
  (disabled-command-function nil)            ; Enable all commands by default
  (delete-by-moving-to-trash t)              ; Move deleted files to trash
  (inhibit-startup-screen t)                 ; Disable startup screen with graphics
  (inhibit-startup-echo-area-message nil)    ; Disable startup echo messages
  (initial-scratch-message nil)              ; Blank scratch buffer
  (initial-buffer-choice nil)                ; Blank initial buffer
  (cursor-in-non-selected-windows nil)       ; Hide cursor in inactive windows
  (initial-major-mode 'text-mode)            ; Text mode is the initial mode
  (default-major-mode 'text-mode)            ; Text mode is the default mode
  (visible-bell nil)                         ; Disable visual bell graphics
  (ring-bell-function #'ignore)              ; Disable audio bell
  (frame-title-format nil)                   ; Disable frame title
  (use-file-dialog nil)                      ; Disable file dialog
  (use-dialog-box nil)                       ; Disable dialog box
  (pop-up-windows nil)                       ; Disable pop-up windows
  (font-lock-maximum-decoration nil)         ; Disable font-lock
  (font-lock-maximum-size nil)               ; Disable font-lock limit
  (auto-fill-mode nil)                       ; Disable auto-fill line break space points
  (fill-column 80)                           ; Fill column at 80
  (truncate-lines t)                         ; Truncate lines
  (kill-buffer-query-functions nil)          ; Disable kill buffer query
  (standard-indent 2)                        ; Set standard indent to 2 spaces
  (enable-recursive-minibuffers nil)         ; Disable recursive minibuffers
  (ad-redefinition-action 'accept)           ; Silence redefinition warnings
  :hook
  (after-init . (lambda () (setq gc-cons-threshold (* 42 1024 1024))))
  :config
  (setq-default cursor-type 'bar             ; Use block cursor on active window
                indent-tabs-mode nil         ; Use spaces instead of tabs
                line-spacing 1               ; Set line spacing
                tab-width 2)                 ; Set tab width as four spaces is a tab

  ;; Enable alternatives to yes-or-no
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Default all buffer to UTF-8
  (prefer-coding-system          'utf-8-unix)
  (set-default-coding-systems    'utf-8-unix)
  (set-terminal-coding-system    'utf-8)
  (set-keyboard-coding-system    'utf-8)
  (set-buffer-file-coding-system 'utf-8-unix)

  ;; Disable C-z that freezes emacs gui
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

(use-package dash                             ; List manipulation library
  :config (dash-enable-font-lock))

(use-package s)                               ; String manipulation
(use-package f)                               ; File manipulation
(use-package async)
(use-package mode-local)
(use-package ffap)

;; Load emacs config for desktop
(use-feature frame
  :if (display-graphic-p)
  :custom
  (blink-cursor-blinks 0)
  :hook
  (focus-out . garbage-collect)
  :init
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  (when is-macos
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

  (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-10"))
  (blink-cursor-mode)

  (when window-system
    (set-frame-font "FiraCode Nerd Font-10")
    (when is-macos
      ;; Emacs-mac
      ;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-composition-mode-in-emacs-mac-port
      (prettify-symbols-mode))
    ;; Disable toolbar, menubar, and scrollbar
    (mapc (lambda (mode)
            (when (fboundp mode)
              (apply mode '(-1))))
          '(tool-bar-mode menu-bar-mode scroll-bar-mode))))

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
  (global-auto-revert-mode t)                 ; Revert buffers to reflect external file changes
  (defun find-file-maybe-make-directories ()
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (make-directory dir t))))
  (push #'find-file-maybe-make-directories find-file-not-found-functions))

(use-feature recentf
  :custom
  (recentf-auto-cleanup 200)
  (recentf-max-saved-items 200)
  :config
  (recentf-mode)
  (advice-add 'recentf-cleanup :around #'inhibit-message-in-minibuffer))

(use-feature simple
  :custom
  (set-mark-command-repeat-pop t)
  (save-interprogram-paste-before-kill t)
  (async-shell-command-buffer 'new-buffer)
  :config
  (column-number-mode)
  (defun kill-or-join-line (f &rest args)
    (if (not (eolp))
        (apply f args)
      (delete-indentation 1)
      (when (and (eolp) (not (eq (point) (point-max))))
        (kill-or-join-line f args))))
  (defun move-beginning-of-line-or-indentation (f &rest args)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (apply f args))))
  (advice-add 'backward-kill-word :around #'delete-region-instead-of-kill-region)
  (advice-add 'kill-line :around #'kill-or-join-line)
  (advice-add 'kill-visual-line :around #'kill-or-join-line)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation)
  (advice-add 'beginning-of-visual-line :around #'move-beginning-of-line-or-indentation))

;; [Built-in] Easily move between panes
(use-feature windmove
  :config
  (windmove-default-keybindings 'meta))

;; [Built-in] Make buffer names distinguishable
(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

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
(use-feature subword
  :diminish subword-mode
  :init (global-subword-mode))

;; Delete on type inside highlight
(use-package delsel
  :diminish delete-selection-mode
  :init (delete-selection-mode))

(use-feature comint
  :bind
  (:map comint-mode-map
        ("RET" . comint-return-dwim)
        ("C-r" . comint-history-isearch-backward-regexp))
  :custom
  (comint-prompt-read-only t)
  :config
  (setq-default comint-input-ignoredups t
                comint-scroll-show-maximum-output nil
                comint-output-filter-functions
                '(ansi-color-process-output
                  comint-truncate-buffer
                  comint-watch-for-password-prompt))
  (defun turn-on-comint-history (history-file)
    (setq comint-input-ring-file-name history-file)
    (comint-read-input-ring 'silent))
  (def comint-return-dwim
    (cond
     ((comint-after-pmark-p)
      (comint-send-input))
     ((ffap-url-at-point)
      (browse-url (ffap-url-at-point)))
     ((ffap-file-at-point)
      (find-file (ffap-file-at-point)))
     (t (comint-next-prompt 1))))
  (defun write-input-ring-for-shell-modes ()
    (when (-any? #'derived-mode-p '(comint-mode term-mode))
      (comint-write-input-ring)))
  (defun write-input-ring-for-all-shell-modes ()
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (write-input-ring-for-shell-modes))))
  (add-hook 'kill-buffer-hook #'write-input-ring-for-shell-modes)
  (add-hook 'kill-emacs-hook #'write-input-ring-for-all-shell-modes))

(use-feature compile
  :custom
  (compilation-always-kill t)                 ; Kill compilation process before starting another
  (compilation-read-commmand nil)             ; Disable confirmation of compile command
  (compilation-ask-about-save nil)            ; Disable save confirmation
  :init
  (add-hook 'compilation-finish-function #'alert-after-finish-in-background))

(use-feature ansi-color
  :hook
  (compilation-filter . colorize-compilation-buffer)
  :config
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(use-package shell
  :custom
  (explicit-shell-file-name (getenv "SHELL"))
  :config
  (defun make-shell-command-behave-interactively (f &rest args)
    (let ((shell-command-switch "-ic"))
      (apply f args)))
  (advice-add 'shell-command :around #'make-shell-command-behave-interactively)
  (advice-add 'start-process-shell-command :around #'make-shell-command-behave-interactively)
  (add-λ 'shell-mode-hook
    (turn-on-comint-history (getenv "HISTFILE"))))

(use-package term
  :bind
  (:map term-raw-mapp
        ([remap term-send-input] . term-return-dwim))
  :custom
  (term-input-ring-file-name (getenv "HISTFILE"))
  :config
  (def term-return-dwim
    (cond
     ((term-after-pmark-p)
      (term-send-input))
     ((ffap-url-at-point)
      (browse-url (ffap-url-at-point)))
     ((ffap-file-at-point)
      (find-file (ffap-file-at-point)))
     (t (term-next-prompt 1)))))

(require 'exec-path-from-shell-config)
(require 'which-key-config)
(require 'rainbow-delimiters-config)
(require 'undo-tree-config)

(garbage-collect)
(put 'erase-buffer 'disabled nil)

;;; init.el ends here
