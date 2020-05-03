;; -*- lexical-binding: t; -*-

(require 'subr-x)
(use-package git)

(defun org-git-version ()
  "Fix check `org-mode' release version."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "Fix check `org-mode' release version."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(use-package org
  :straight org-plus-contrib
  :custom
  (org-replace-disputed-keys t)
  (org-support-shift-select 'always)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-latex-listings 'minted)
  (org-latex-packages-alist '(("" "minted")))
  :hook
  (org-mode . visual-line-mode)
  :config
  (setq org-todo-keywords
        '((sequence "⚑ TODO(t)" "⌛ IN-PROGRESS(p)" "|" "✔ DONE(d)" "☂ DEFERRED(e)" "✘ CANCELLED(c)")))
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)))
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (advice-add 'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window))

;;(use-package org-bullets
;;  :after org
;;  :hook
;;  (org-mode . org-bullets-mode 1))

(provide 'port-org)
