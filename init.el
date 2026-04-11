;;; init.el --- Initialization file for Emacs 30+ -*- lexical-binding: t; -*-
;;; Commentary:
;;;   ffimnsr <ffimnsr@gmail.com>

(unless (>= emacs-major-version 30)
  (error "Emacs version 30 or higher is required, you're running %s"
         emacs-version))

;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(defvar ff/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(check-on-save find-when-checking)
      straight-cache-autoloads t)

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

(straight-use-package 'use-package)

(load-file (expand-file-name "lib.el" user-emacs-directory))

(defconst modules-dir (concat user-emacs-directory "modules"))

;; Load packages
(add-to-list 'load-path modules-dir)

(use-package gcmh                                ; Garbage collector magic hack
  :diminish gcmh-mode
  :custom
  (gcmh-high-cons-threshold (* 256 1024 1024))
  :hook (after-init . gcmh-mode))
(use-package hydra                              ; Make bindings that stick around
  :defer t)
(use-package restart-emacs                      ; Restart emacs from within emacs
  :commands (restart-emacs))
(use-package diminish)                           ; Diminish minor modes
(use-package no-littering)                       ; Keep emacs.d clean
(use-package which-key                           ; Display available keybindings in popup
  :diminish which-key-mode
  :defer 1
  :config
  ;; Popup side window on bottom.
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;; Find file (or url) at point
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/ffap.el
(use-feature ffap)

(use-feature emacs
  :custom
  (load-prefer-newer t)                      ; Load newer files
  (history-length 128)                       ; Set history length
  (history-delete-duplicates t)              ; Delete duplicate history
  (echo-keystrokes 1e-6)                     ; Show keystrokes immediately
  (create-lockfiles nil)                     ; Disable lock files
  (disabled-command-function nil)            ; Enable all commands by default
  (delete-by-moving-to-trash t)              ; Move deleted files to trash
  (inhibit-default-init t)                   ; Disable default init
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
  (font-lock-maximum-decoration t)           ; Enable full font-lock decoration
  (font-lock-maximum-size nil)               ; No font-lock size limit
  (auto-fill-mode nil)                       ; Disable auto-fill line break space points
  (fill-column 80)                           ; Fill column at 80
  (truncate-lines t)                         ; Truncate lines
  (kill-buffer-query-functions nil)          ; Disable kill buffer query
  (enable-recursive-minibuffers nil)         ; Disable recursive minibuffers
  (ad-redefinition-action 'accept)           ; Silence redefinition warnings
  (read-process-output-max (* 8 1024 1024))  ; Increase process output
  (process-adaptive-read-buffering nil)      ; Disable adaptive read buffering
  (use-short-answers t)                      ; Use short answers
  :hook
  (after-init . (lambda ()
                  (setq gc-cons-threshold (* 128 1024 1024)
                        gc-cons-percentage 0.1
                        file-name-handler-alist ff/file-name-handler-alist)))
  :config
  (setq-default cursor-type 'bar             ; Use block cursor on active window
                indent-tabs-mode nil         ; Use spaces instead of tabs
                line-spacing 1               ; Set line spacing
                tab-width 2)                 ; Set tab width as four spaces is a tab
  (setq-default standard-indent 2
                c-basic-offset 2
                js-indent-level 2
                typescript-indent-level 2
                rust-indent-offset 2
                css-indent-offset 2
                sh-basic-offset 2
                yaml-indent-offset 2)
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local indent-tabs-mode nil)
              (setq-local tab-width 2)))

  ;; Disable built-in games and toy commands.
  (dolist (game-cmd '(5x5 blackbox bubbles doctor dunnet gomoku hanoi life
                          mpuz pong snake solitaire tetris zone))
    (put game-cmd 'disabled t))

  ;; Default all buffer to UTF-8
  (prefer-coding-system          'utf-8-unix)
  (set-default-coding-systems    'utf-8-unix)
  (set-terminal-coding-system    'utf-8)
  (set-keyboard-coding-system    'utf-8)
  (set-buffer-file-coding-system 'utf-8-unix)

  ;; Disable C-z that freezes emacs gui
  (global-unset-key (kbd "C-z"))

  ;; Disable overwrite-mode mode
  (put 'overwrite-mode 'disabled t)
  (global-unset-key (kbd "<insert>"))
  (global-unset-key (kbd "<insertchar>"))

  ;; Quickly switch to previous buffer
  (global-set-key (kbd "C-6") 'mode-line-other-buffer)

  ;; Don't disable narrowing commands
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  ;; Don't disable case-change functions
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; Enable erase-buffer command
  (put 'erase-buffer 'disabled nil)

  ;; Quiet startup
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Running as server process
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/server.el
(use-feature server
  :config
  ;; Start server if not running
  (unless (server-running-p)
    (server-start)))

;; File input and output commands for Emacs
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/files.el
(use-feature files
  :custom
  (require-final-newline t)                  ; Require newline at end of file
  (confirm-kill-emacs nil)                   ; Do not confirm before ending emacs session
  (confirm-kill-process nil)                 ; Do not confirm before killing process
  (confirm-nonexistent-file-or-buffer nil)   ; Disable annoying confirmation for not exist
  (large-file-warning-threshold 50000000)    ; Disable large file warning
  (kept-new-versions 10)                     ; Keep 10 new versions
  (kept-old-versions 0)                      ; Keep 0 old versions
  (global-auto-revert-non-file-buffers t)    ; Auto revert non-file buffers
  (auto-revert-verbose nil)                  ; Disable auto revert verbose
  :config
  ;; Revert buffers to reflect external file changes
  (global-auto-revert-mode t)

  ;; Create parent directories if they don't exist
  (defun find-file-maybe-make-directories ()
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (make-directory dir t))))
  (push #'find-file-maybe-make-directories find-file-not-found-functions))

;; Keep track of recently opened files
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/recentf.el
(use-feature recentf
  :custom
  (recentf-auto-cleanup 'never)              ; Cleanup on idle timer instead
  (recentf-max-saved-items 200)              ; Maximum 200 files
  :config
  (recentf-mode 1)
  (run-with-idle-timer 30 t #'recentf-cleanup)
  (advice-add 'recentf-cleanup :around #'inhibit-message-in-minibuffer))

;; Basic editing commands for Emacs
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/simple.el
(use-feature simple
  :bind
  ("C-/" . undo-only)
  ("C-S-/" . undo-redo)
  :custom
  (set-mark-command-repeat-pop t)            ; Repeat pop mark command
  (save-interprogram-paste-before-kill t)    ; Save clipboard contents before killing
  (async-shell-command-buffer 'new-buffer)   ; Run async shell command in new buffer
  :hook
  (after-init . transient-mark-mode)
  :config
  (column-number-mode t)                     ; Show column number in mode line
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

;; Scroll lock scrolling
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/scroll-lock.el
(use-feature scroll-lock
  :custom
  (scroll-preserve-screen-position t))       ; Preserve screen position

;; Directional window-selection routines
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/windmove.el
(use-feature windmove
  :config
  (windmove-default-keybindings 'meta))

;; Restore old window configurations
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/winner.el
(use-feature winner
  :bind
  ("C-c <down>" . ff/toggle-current-window-dedication)
  ("<f7>"  . ff/split-window)
  ("C-x 1" . ff/toggle-delete-other-windows)
  ("C-x |" . ff/split-window-horizontally-instead)
  ("C-x _" . ff/split-window-vertically-instead)
  :hook
  (after-init . winner-mode)
  :config
  (defun ff/toggle-delete-other-windows ()
    "Delete other windows in frame if any, or restore previous window config."
    (interactive)
    (if (and winner-mode (equal (selected-window) (next-window)))
        (winner-undo)
      (delete-other-windows)))
  (defun ff/split-window-horizontally-instead ()
    "Kill any other windows and re-split such that the current window is on the top half of the frame."
    (interactive)
    (let* ((next-win (next-window))
           (other-buffer (unless (eq next-win (selected-window))
                           (window-buffer next-win))))
      (delete-other-windows)
      (split-window-horizontally)
      (when other-buffer
        (set-window-buffer (next-window) other-buffer))))
  (defun ff/split-window-vertically-instead ()
    "Kill any other windows and re-split such that the current window is on the left half of the frame."
    (interactive)
    (let* ((next-win (next-window))
           (other-buffer (unless (eq next-win (selected-window))
                           (window-buffer next-win))))
      (delete-other-windows)
      (split-window-vertically)
      (when other-buffer
        (set-window-buffer (next-window) other-buffer))))
  (defun ff/split-window()
    "Split the window to see the most recent buffer in the other window.
    Call a second time to restore the original window configuration."
    (interactive)
    (if (eq last-command 'ff/split-window)
        (progn
          (jump-to-register :ff/split-window)
          (setq this-command 'ff/unsplit-window))
      (window-configuration-to-register :ff/split-window)
      (switch-to-buffer-other-window nil)))
  (defun ff/toggle-current-window-dedication ()
    "Toggle whether the current window is dedicated to its current buffer."
    (interactive)
    (let* ((window (selected-window))
          (was-dedicated (window-dedicated-p window)))
      (set-window-dedicated-p window (not was-dedicated))
      (message "Window %sdedicated to %s"
              (if was-dedicated "no longer " "")
              (buffer-name)))))

;; Unique buffer names dependent on file name
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/uniquify.el
(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)      ; Uniquify buffer name style
  (uniquify-separator " • ")                 ; Uniquify separator
  (uniquify-after-kill-buffer-p t)           ; Uniquify after kill buffer
  (uniquify-ignore-buffers-re "^\\*"))       ; Uniquify ignore buffers

;; Directory-browsing commands
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/dired.el
(use-feature dired
  :custom
  (dired-create-destination-dirs t)          ; Create destination directories
  (dired-dwim-target t)                      ; Dired do-what-I-mean target
  (dired-recursive-deletes 'always)          ; Recursive deletes
  (dired-recursive-copies 'always)           ; Recursive copies
  (dired-use-ls-dired nil)                   ; Use ls for dired
  (dired-auto-revert-buffer t))              ; Auto revert dired buffer

;; Sub-word traversing
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/progmodes/subword.el
(use-feature subword
  :diminish subword-mode
  :hook (prog-mode . subword-mode))

;; Delete selection if you insert
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/delsel.el
(use-feature delsel
  :diminish delete-selection-mode
  :config (delete-selection-mode))

;; Say farewell to performance problems with minified code
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/so-long.el
(use-feature so-long
  :hook
  (after-init . global-so-long-mode))

;; Display line numbers in the left margin
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/display-line-numbers.el
(use-feature display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 4))

;; Display fill column indicator
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/display-fill-column-indicator.el
(use-feature display-fill-column-indicator
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :custom
  (display-fill-column-indicator-character ?\u2502)
  (display-fill-column-indicator-column 80))

;; General command interpreter in a window stuff
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/comint.el
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
  (defun comint-return-dwim ()
    (cond
      ((comint-after-pmark-p) (comint-send-input))
      ((ffap-url-at-point) (browse-url (ffap-url-at-point)))
      ((ffap-file-at-point) (find-file (ffap-file-at-point)))
      (t (comint-next-prompt 1))))
  (defun write-input-ring-for-shell-modes ()
    (when (or (derived-mode-p 'comint-mode)
              (derived-mode-p 'term-mode))
      (comint-write-input-ring)))
  (defun write-input-ring-for-all-shell-modes ()
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (write-input-ring-for-shell-modes))))
  (add-hook 'kill-buffer-hook #'write-input-ring-for-shell-modes)
  (add-hook 'kill-emacs-hook #'write-input-ring-for-all-shell-modes))

;; Run compiler as inferior of Emacs
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/progmodes/compile.el
(use-feature compile
  :custom
  (compilation-always-kill t)                 ; Kill compilation process before starting another
  (compilation-read-command nil)              ; Disable confirmation of compile command
  (compilation-ask-about-save nil)            ; Disable save confirmation
  :hook (compilation-finish-functions . #'alert-after-finish-in-background))

;; General command interpreter in a window stuff
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/term.el
(use-feature term
  :bind
  (:map term-raw-map
        ([remap term-send-input] . term-return-dwim))
  :custom
  (term-input-ring-file-name (getenv "HISTFILE"))
  :config
  (defun term-return-dwim ()
    (cond
      ((term-after-pmark-p) (term-send-input))
      ((ffap-url-at-point) (browse-url (ffap-url-at-point)))
      ((ffap-file-at-point) (find-file (ffap-file-at-point)))
      (t (term-next-prompt 1)))))

;; Window maker and Command loop
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/electric.el
(use-feature electric
  :hook
  (after-init . electric-indent-mode)
  :custom
  (electric-quote-string t)
  (electric-quote-context-sensitive t))

;; Automatic parenthesis pairing
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/elec-pair.el
(use-feature elec-pair
  :hook
  (after-init . electric-pair-mode))

;; Highlight matching paren
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/paren.el
(use-feature paren
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode))

;; Highlight current line globally for better terminal readability.
;; https://github.com/emacs-mirror/emacs/blob/emacs-30.2/lisp/hl-line.el
(use-feature hl-line
  :hook
  (after-init . global-hl-line-mode))

;; Framework for mode-specific buffer indexes
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/imenu.el
(use-feature imenu
  :custom
  (imenu-auto-rescan t))

;; Show function arglist or variable docstring in echo area
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/emacs-lisp/eldoc.el
(use-feature eldoc
  :diminish eldoc-mode
  :hook
  (after-init . global-eldoc-mode))

;; A comprehensive visual interface to diff & patch
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/vc/ediff.el
(use-feature ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; High-contrast terminal colors.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Themes.html
(use-feature faces
  :config
  (unless (display-graphic-p)
    (load-theme 'modus-vivendi t)
    (setq frame-background-mode 'dark)
    (set-face-attribute 'default nil :foreground "white" :background "black")
    (set-face-attribute 'mode-line nil :foreground "black" :background "white" :box nil)
    (set-face-attribute 'mode-line-inactive nil :foreground "white" :background "brightblack" :box nil)
    (set-face-attribute 'minibuffer-prompt nil :foreground "cyan" :weight 'bold)))

;; Useful for traversing files by characters
;; https://github.com/abo-abo/avy
(use-package avy
  :commands (avy-goto-char-2 avy-goto-line)
  :custom
  (avy-style 'de-bruijn)
  :bind
  ([remap goto-char] . avy-goto-char-2)
  ([remap goto-line] . avy-goto-line)
  :config
  (avy-setup-default))

;; Expand region increases the selected region by semantic units
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

;; Highlight indentation guides
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :commands (highlight-indent-guides-mode)
  :bind ("C-c i" . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  :diminish highlight-indent-guides-mode)

;; Save minibuffer history between sessions.
;; https://github.com/emacs-mirror/emacs/blob/emacs-30.2/lisp/savehist.el
(use-feature savehist
  :hook
  (after-init . savehist-mode))

;; Built-in minibuffer completion UI.
;; https://github.com/emacs-mirror/emacs/blob/emacs-30.2/lisp/icomplete.el
(use-feature icomplete
  :hook
  (after-init . fido-vertical-mode))

;; Built-in inline completion preview.
;; https://github.com/emacs-mirror/emacs/blob/emacs-30.2/lisp/completion-preview.el
(use-feature completion-preview
  :hook
  (prog-mode . completion-preview-mode))

;; Built-in diagnostics backend.
;; https://github.com/emacs-mirror/emacs/blob/emacs-30.2/lisp/progmodes/flymake.el
(use-feature flymake
  :custom
  (flymake-no-changes-timeout 0.3))

;; Built-in LSP client for Emacs.
;; https://www.gnu.org/software/emacs/manual/html_mono/eglot.html
(use-feature eglot
  :hook
  ((js-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (typescript-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil))

;; JavaScript and JSON base modes.
;; https://github.com/emacs-mirror/emacs/blob/emacs-30.2/lisp/progmodes/js.el
(use-feature js
  :mode
  (("\\.m?js\\'" . js-mode)
   ("\\.cjs\\'" . js-mode)
   ("\\.json\\'" . js-json-mode)))

;; TypeScript fallback mode.
;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :commands (typescript-mode)
  :defer t)

;; YAML fallback mode.
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :commands (yaml-mode)
  :defer t)

;; TOML fallback mode (built-in).
;; https://github.com/emacs-mirror/emacs/blob/emacs-30.2/lisp/progmodes/conf-mode.el
(use-feature conf-mode
  :mode ("\\.toml\\'" . conf-toml-mode))

;; Rust major mode fallback when tree-sitter grammar is not installed.
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :commands (rust-mode)
  :defer t)

;; Prefer built-in rust-ts-mode when tree-sitter grammar is available.
;; https://github.com/emacs-mirror/emacs/blob/emacs-30.2/lisp/progmodes/rust-ts-mode.el
(use-feature treesit
  :config
  (when (and (fboundp 'js-ts-mode)
             (treesit-ready-p 'javascript t))
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode)))
  (when (and (fboundp 'json-ts-mode)
             (treesit-ready-p 'json t))
    (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode)))
  (when (and (fboundp 'typescript-ts-mode)
             (treesit-ready-p 'typescript t))
    (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode)))
  (if (and (fboundp 'tsx-ts-mode)
           (treesit-ready-p 'tsx t))
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))
  (when (treesit-ready-p 'rust t)
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))
  (when (and (fboundp 'yaml-ts-mode)
             (treesit-ready-p 'yaml t))
    (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))
  (when (and (fboundp 'toml-ts-mode)
             (treesit-ready-p 'toml t))
    (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))))

;;; init.el ends here
