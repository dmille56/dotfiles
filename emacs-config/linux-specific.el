(use-package w3m :defer)
(use-package vterm :defer)

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "sensible-browser")

(if (eq my/config-machine 'pc)
    (setq org-agenda-files (list "~/Dropbox/org/todo.org"
                                 "~/Dropbox/org/stuff.org"
                                 "~/Dropbox/org/notes.org")))

(use-package direnv
 :defer
 :init
 (direnv-mode))
