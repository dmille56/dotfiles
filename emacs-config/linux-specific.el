(use-package w3m)
(use-package vterm)

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "sensible-browser")

(setq org-agenda-files (list "~/Dropbox/org/todo.org"
                            "~/Dropbox/org/stuff.org"
                            "~/Dropbox/org/notes.org"))

(use-package direnv
 :config
 (direnv-mode))
