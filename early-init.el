;;; early-init.el --- Early initialization for Emacs 30+ -*- lexical-binding: t; -*-

;; Keep package.el from initializing before straight/use-package.
(setq package-enable-at-startup nil)

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; early-init.el ends here
