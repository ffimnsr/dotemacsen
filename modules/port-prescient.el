;; -*- lexical-binding: t; -*-

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :after prescient
  :custom
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :after prescient
  :config
  (company-prescient-mode))

(provide 'port-prescient)
