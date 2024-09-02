;; -*- lexical-binding: t; -*-

;; Use monokai theme and set it
(use-package monokai-theme
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'monokai t)))
    (load-theme 'monokai t))
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line           nil :overline   line)
    (set-face-attribute 'mode-line-inactive  nil :overline   line)
    (set-face-attribute 'mode-line-inactive  nil :underline  line)
    (set-face-attribute 'mode-line           nil :box        nil)
    (set-face-attribute 'mode-line-inactive  nil :box        nil)
    (set-face-attribute 'mode-line-highlight nil :box        nil)))

(provide 'port-ui)
