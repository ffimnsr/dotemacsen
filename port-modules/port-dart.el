;; -*- lexical-binding: t; -*-

(defun project-try-dart (dir)
  "Check if the directory is dart project."
  (let ((project (or (locate-dominating-file dir "pubspec.yaml")
                     (locate-dominating-file dir "BUILD"))))
    (if project
        (cons 'dart project)
      (cons 'transient dir))))
(add-hook 'project-find-functions #'project-try-dart)
(cl-defmethod project-roots ((project (head dart)))
  (list (cdr project)))

(use-package dart-mode
  :ensure-system-package dart
  :custom
  (dart-enable-analysis-server nil))

(provide 'port-dart)
