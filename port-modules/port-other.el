;; -*- lexical-binding: t; -*-

;; (use-package dockerfile-mode)

;; (use-package csv-mode
;;   :mode "\\.csv\\'")

(use-package json-mode
  :mode "\\.json\\'"
  :hook
  (json-mode . (lambda ()
                 (make-local-variable 'js-indent-level)
                 (setq js-indent-level 2))))

;; (use-package graphql-mode
;;   :mode "\\.graphql\\'")

;; (use-package toml-mode
;;   :mode "\\.toml\\'")

;; (use-package yaml-mode
;;   :mode "\\.yaml\\'")

;; (use-package gitattributes-mode)
;; (use-package gitconfig-mode)
;; (use-package gitignore-mode)

;; (use-package mmm-mode
;;   :custom
;;   (mmm-submode-decoration-level 0))

;; (use-package vue-mode)

;; (use-package ruby-mode
;;   :mode "\\.rb\\'"
;;   :interpreter "ruby")

;; (use-package php-mode
;;   :mode "\\.php\\'"
;;   :interpreter "php")

;; (use-package lua-mode
;;   :mode "\\.lua\\'"
;;   :interpreter "lua")

;; (use-package kotlin-mode
;;   :mode "\\.kt\\'")

;; (use-package swift-mode
;;   :mode "\\.swift\\'")

;; (use-package scala-mode
;;   :mode "\\.s\\(cala\\|bt\\)\\'")

(use-package markdown-mode
  :ensure-system-package
  (marked . "npm i -g marked")
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :hook
  (markdown-mode . visual-line-mode)  
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "marked")
  (markdown-indent-on-enter nil))

(provide 'port-other)
