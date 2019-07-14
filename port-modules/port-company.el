;; -*- lexical-binding: t; -*-

(use-package company
  :diminish company-mode
  :init
  (global-company-mode)
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

(provide 'port-company)
