;; -*- lexical-binding: t; -*-

(use-package restclient
  :mode
  ("\\.rct\\'" . restclient-mode)
  ("\\.http\\'" . restclient-mode))

;; (use-package know-your-http-well)
;; (use-package company-restclient
;;   :config
;;   (add-to-list 'company-backends 'company-restclient))

(provide 'port-restclient)
