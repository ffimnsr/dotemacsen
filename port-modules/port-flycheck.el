;; -*- lexical-binding: t; -*-

(use-package flycheck
  :defer 5
  :diminish flycheck-mode
  :hook
  (after-init . global-flycheck-mode)
  (flycheck-mode . use-eslint-from-node-modules)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist emacs-lisp-checkdoc)))
  (defun use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

(use-package flycheck-pos-tip
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(provide 'port-flycheck)
