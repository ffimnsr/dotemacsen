;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-tools-install :no-query))

(provide 'postscript)
