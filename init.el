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

(straight-use-package 'use-package)

(load (concat user-emacs-directory "lib.el"))

(defconst default-font-size 12)
(defconst is-macos (eq system-type 'darwin))
(defconst modules-dir (concat user-emacs-directory "modules"))

;; Load packages
(mapc (apply-partially 'add-to-list 'load-path)
      '(modules-dir))

(use-package hydra)                              ; Make bindings that stick around
(use-package restart-emacs)                      ; Restart emacs from within emacs
(use-package bind-key)                           ; Bind keys easily
(use-package key-chord)                          ; Allow use of key chords
(use-package diminish)                           ; Diminish minor modes
(use-package no-littering)                       ; Keep emacs.d clean
(use-package use-package-ensure-system-package)  ; Ensure system binaries exist alongside your package declarations
(use-package use-package-hydra)                  ; Add the :hydra keyword to the use-package macro
(use-package use-package-chords)                 ; Define key-chord bindings for use-package declarations
(use-package which-key                           ; Display available keybindings in popup
  :diminish which-key-mode
  :custom
  ;; Popup side window on bottom.
  (which-key-setup-side-window-bottom)
  :config
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

  (when is-macos
    (setq ns-use-native-fullscreen nil)
    (setq ns-function-modifier 'control)
    (setq ns-pop-up-frames nil)
    (setq mac-option-modifier 'super
          mac-command-modifier 'meta
          mac-function-modifer 'control)    
    (add-to-list 'exec-path "/usr/local/bin")
    (global-set-key "\M-`" 'other-frame))

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

  ;; Enable narrow-to-region command
  (put 'narrow-to-region 'disabled nil)

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

;; Multi-frame management independent of window systems
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/frame.el
(use-feature frame
  :if (display-graphic-p)
  :custom
  (blink-cursor-blinks 0)
  :hook
  (focus-out . garbage-collect)
  :init
  ;; Set initial frame size to maximized
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; Change frame appearance in macOS
  (when is-macos
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
  ;; Set default font
  (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-10"))
  ;; Set cursor blink mode
  (blink-cursor-mode t)
  ;; Set font face
  (set-frame-font "FiraCode Nerd Font-10")
  ;; Disable toolbar, menubar, and scrollbar
  (mapc (lambda (mode)
    (when (fboundp mode)
      (apply mode '(-1))))
    '(tool-bar-mode menu-bar-mode scroll-bar-mode)))

;; File input and output commands for Emacs
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/files.el
(use-feature files
  :custom
  (require-final-newline t)                  ; Require newline at end of file
  (confirm-kill-emacs nil)                   ; Do not confirm before ending emacs session
  (confirm-kill-process nil)                 ; Do not confirm before killing process
  (confirm-nonexistent-file-or-buffer nil)   ; Disable annoying confirmation for not exist
  (backup-directory-alist                    ; Backup files to temp directory
    `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms            ; Auto save files to temp directory
    `((".*" ,temporary-file-directory t)))
  (large-file-warning-threshold 50000000)    ; Disable large file warning
  (kept-new-versions 10)                     ; Keep 10 new versions
  (kept-old-versions 0)                      ; Keep 0 old versions
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
  (recentf-auto-cleanup 200)                 ; Auto cleanup after 200 files
  (recentf-max-saved-items 200)              ; Maximum 200 files
  :config
  (recentf-mode 1)
  (advice-add 'recentf-cleanup :around #'inhibit-message-in-minibuffer))

;; Basic editing commands for Emacs
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/simple.el
(use-feature simple
  :custom
  (set-mark-command-repeat-pop t)            ; Repeat pop mark command
  (save-interprogram-paste-before-kill t)    ; Save clipboard contents before killing
  (async-shell-command-buffer 'new-buffer)   ; Run async shell command in new buffer
  :config
  (column-number-mode t)                       ; Show column number in mode line
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

;; Directional window-selection routines
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/windmove.el
(use-feature windmove
  :config
  (windmove-default-keybindings 'meta))

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

;; Fringe setup and control
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/fringe.el
(use-feature fringe
  :config
  ;; Set side padding size
  (fringe-mode '(1 . 1)))

;; Sub-word traversing
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/progmodes/subword.el
(use-feature subword
  :diminish subword-mode
  :init (global-subword-mode))

;; Delete selection if you insert
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/delsel.el
(use-feature delsel
  :diminish delete-selection-mode
  :init (delete-selection-mode))

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
  (def comint-return-dwim
    (cond
      ((comint-after-pmark-p) (comint-send-input))
      ((ffap-url-at-point) (browse-url (ffap-url-at-point)))
      ((ffap-file-at-point) (find-file (ffap-file-at-point)))
      (t (comint-next-prompt 1))))
  (defun write-input-ring-for-shell-modes ()
    (when (-any? #'derived-mode-p '(comint-mode term-mode))
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
  (compilation-read-commmand nil)             ; Disable confirmation of compile command
  (compilation-ask-about-save nil)            ; Disable save confirmation
  :hook (compilation-finish-functions . #'alert-after-finish-in-background))

;; General command interpreter in a window stuff
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/term.el
(use-feature term
  :bind
  (:map term-raw-mapp
        ([remap term-send-input] . term-return-dwim))
  :custom
  (term-input-ring-file-name (getenv "HISTFILE"))
  :config
  (def term-return-dwim
    (cond
      ((term-after-pmark-p) (term-send-input))
      ((ffap-url-at-point) (browse-url (ffap-url-at-point)))
      ((ffap-file-at-point) (find-file (ffap-file-at-point)))
      (t (term-next-prompt 1)))))

;; Window maker and Command loop
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/electric.el
(use-feature electric
  :custom
  (electric-quote-string t)
  (electric-quote-context-sensitive t))

;; Automatic parenthesis pairing
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/elec-pair.el
(use-feature elec-pair
  :init (electric-pair-mode))

;; Highlight matching paren
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/paren.el
(use-feature paren
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode))

;; Framework for mode-specific buffer indexes
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/imenu.el
(use-feature imenu
  :custom
  (imenu-auto-rescan t))

;; Support for visiting image files
;; https://github.com/emacs-mirror/emacs/blob/emacs-29.4/lisp/image-mode.el
(use-feature image-mode
  :hook
  (image-mode . show-image-dimensions-in-mode-line)
  :custom
  (image-animate-loop t)
  :config
  (defun show-image-dimensions-in-mode-line ()
    (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
           (width (car image-dimensions))
           (height (cdr image-dimensions)))
      (setq mode-line-buffer-identification
            (format "%s %dx%d" (propertized-buffer-identification "%12b") width height)))))  

;; Git Porcelain inside Emacs
;; https://github.com/magit/magit
(use-package magit
  :straight (magit :type git :host github :repo "magit/magit" :branch "main")
  :custom
  (magit-log-section-commit-count 0)
  (magit-branch-prefer-remote-upstream t)
  (magit-log-auto-more t)
  (magit-diff-refine-hunk 'all)
  (magit-no-confirm t))

;; Use gruvbox-theme
;; https://github.com/Greduan/emacs-theme-gruvbox
(use-package gruvbox-theme
  :config
  (if (daemonp)
    (add-hook 'after-make-frame-functions
      (lambda (frame)
        (select-frame frame)
        (load-theme 'gruvbox t)))
    (load-theme 'gruvbox t)))     

;; Useful for traversing files by characters
;; https://github.com/abo-abo/avy
(use-package avy
  :custom
  (avy-style 'de-bruijn)
  :bind
  ([remap goto-char] . avy-goto-char-2)
  ([remap goto-line] . avy-goto-line)
  :config
  (avy-setup-default))

;; Visualize the whole undo history in buffer as a tree, and you can access anywhere in it.
;; https://github.com/emacsmirror/undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :bind
  ("C-/" . undo-tree-undo)
  ("C-S-/" . undo-tree-redo)
  :custom
  ;; Show relative times in the undo tree visualizer.
  (undo-tree-visualizer-timestamps nil)
  ;; Show diffs when browsing through the undo tree.
  (undo-tree-visualizer-diff nil)
  ;; Save history to a file.
  (undo-tree-auto-save-history nil))

;; Emacs rainbow delimiters mode
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom
  (rainbow-delimiters-max-face-count 6))

;; Expand region increases the selected region by semantic units
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

;; Highlight indentation guides
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  :diminish highlight-indent-guides-mode)

;; Use ripgrep in Emacs
;; https://github.com/dajva/rg.el
(use-package rg
  :ensure-system-package rg)

;; Generic completion mechanism for Emacs
;; https://github.com/abo-abo/swiper
(use-package ivy
  :diminish ivy-mode
  :custom
  (ivy-extra-directories nil)
  (ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (counsel-rg . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-format-function #'ivy-format-function-arrow)
  :hook (after-init . ivy-mode)
  :config
  ;; Can not exit minibuffer - https://github.com/abo-abo/swiper/issues/1953
  (defvar ivy-recursive-restore-in-progress nil)
  (defun ivy-note-when-inside-recursive-restore (orig-fun &rest args)
    (let ((ivy-recursive-restore-in-progress t))
      (apply orig-fun args)))
  (defun ivy-no-read-while-exiting-recursion (orig-fun &rest args)
    (if ivy-recursive-restore-in-progress
        (error "Cannot use `ivy-read' while restoring recursive state")
      (apply orig-fun args)))
  (advice-add 'ivy-recursive-restore :around #'ivy-note-when-inside-recursive-restore)
  (advice-add 'ivy-read :around #'ivy-no-read-while-exiting-recursion))

;; Ivy hydra provides additional keybindings for ivy
;; https://github.com/abo-abo/swiper
(use-package ivy-hydra
  :after ivy)

;; Ivy avy provides avy commands to ivy minibuffer
;; https://github.com/abo-abo/swiper
(use-package ivy-avy
  :after ivy)

;; Use Ivy as the interface to select from xref candidates
;; https://github.com/alexmurray/ivy-xref
(use-package ivy-xref
  :after ivy
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Ensures that any Emacs command using completing-read-function uses ivy for completion
;; https://github.com/abo-abo/swiper
(use-package counsel
  :after ivy
  :bind
  ([remap execute-extended-command] . counsel-M-x))

;; Swiper is an alternative to isearch that uses Ivy to show an overview of all matches
;; https://github.com/abo-abo/swiper
(use-package swiper
  :after ivy
  :bind
  ([remap isearch-forward] . swiper)
  ([remap isearch-backward] . swiper))

;; Provides navigation for imenu tags across all buffers
;; https://github.com/vspinu/imenu-anywhere
(use-package imenu-anywhere
  :after ivy
  :commands ivy-imenu-anywhere)

;; On the fly syntax checking for GNU Emacs
;; https://github.com/flycheck/flycheck
(use-package flycheck
  :diminish flycheck-mode
  :hook
  (after-init . global-flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".flycheck"))

;; ;; Modular in-buffer completion framework for Emacs
;; ;; https://github.com/company-mode/company-mode
(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-require-match nil)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-transformers '(company-sort-prefer-same-case-prefix))
  (company-dabbrev-minimum-length 2)
  (company-dabbrev-code-modes t)
  (company-dabbrev-code-everywhere t)
  (company-backends '(company-capf company-files (company-dabbrev-code company-etags) company-dabbrev))
  :bind
  ("C-:" . company-complete)
  ([remap completion-at-point] . company-manual-begin)
  ([remap complete-symbol] . company-manual-begin))

;; LSP mode for Emacs
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-snippet nil)
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil))

;; LSP UI tools
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :config
  (lsp-ui-imenu-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable nil))

;; LSP ivy integration
;; https://github.com/emacs-lsp/lsp-ivy
(use-package lsp-ivy
  :after lsp-mode
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))

;; Enable exec-path-from-shell
;; https://github.com/purcell/exec-path-from-shell
;; (use-package exec-path-from-shell
;;   :if (memq window-system '(mac ns x))
;;   :ensure t
;;   :custom
;;   (exec-path-from-shell-check-startup-file nil)
;;   :config 
;;   (add-to-list 'exec-path-from-shell-variables "GOPATH")
;;   (exec-path-from-shell-initialize))

(garbage-collect)

;;; init.el ends here
