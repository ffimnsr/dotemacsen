;; -*- lexical-binding: t; -*-

;; TODO: Disabled as Emacs NS is not friendly with ligatures
;; (def install-fira-code-font
;;   (let* ((font-name "FiraCode-Retina.ttf")
;;          (font-url
;;           (format "https://github.com/tonsky/FiraCode/blob/master/distr/ttf/%s?raw=true" font-name))
;;          (font-dest
;;           (cl-case window-system
;;             (x  (concat (or (getenv "XDG_DATA_HOME")
;;                             (concat (getenv "HOME") "/.local/share"))
;;                         "/fonts/"))
;;             (mac (concat (getenv "HOME") "/Library/Fonts/" ))
;;             (ns (concat (getenv "HOME") "/Library/Fonts/" )))))
;;     (unless (file-directory-p font-dest) (mkdir font-dest t))
;;     (url-copy-file font-url (expand-file-name font-name font-dest) t)
;;     (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
;;     (shell-command-to-string (format "fc-cache -f -v"))
;;     (message "Successfully install `fira-code' font to `%s'!" font-dest)))

;; (def setup-fira-code-font
;;   (unless (member "Fira Code" (font-family-list))
;;     (install-fira-code-font))
;;   (add-to-list 'default-frame-alist `(font . ,(concat "Fira Code Retina-" (number-to-string default-font-size))))
;;   (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                  (35 . ".\\(?:###\\|##\\|[#(?[_{]\\)")
;;                  (36 . ".\\(?:>\\)")
;;                  (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                  (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                  (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                  (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                  (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                  (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                  (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                  (48 . ".\\(?:x[a-zA-Z]\\)")
;;                  (58 . ".\\(?:::\\|[:=]\\)")
;;                  (59 . ".\\(?:;;\\|;\\)")
;;                  (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                  (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                  (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                  (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                  (91 . ".\\(?:]\\)")
;;                  (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                  (94 . ".\\(?:=\\)")
;;                  (119 . ".\\(?:ww\\)")
;;                  (123 . ".\\(?:-\\)")
;;                  (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                  (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
;;     (dolist (char-regexp alist)
;;       (set-char-table-range composition-function-table (car char-regexp)
;;                             `([,(cdr char-regexp) 0 font-shape-gstring])))))

;; (add-hook 'emacs-startup-hook #'setup-fira-code-font)

(provide 'port-ui-ligatures)
