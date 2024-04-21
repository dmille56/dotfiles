;;; linux-specific --- linux specfic emacs config
;;; Commentary:
;;; Code:

(use-package w3m :defer)
(use-package vterm :defer)

;; uncomment this to go back to using firefox instead of eaf-browser
;; (setq browse-url-browser-function 'browse-url-generic)
(setq-default browse-url-generic-program "sensible-browser")

(if (eq my/config-machine 'pc)
    (setq org-agenda-files (list "~/Dropbox/org/todo.org"
                                 "~/Dropbox/org/stuff.org"
                                 "~/Dropbox/org/notes.org")))

(use-package direnv
 :functions direnv-mode
 :defer
 :init
 (direnv-mode))

;; configure text to speech (tts)
(with-eval-after-load 'read-aloud
  (plist-put read-aloud-engines "my-tts" '(cmd "my-tts" args nil))
  (setq-default read-aloud-engine "my-tts"))

(provide 'linux-specific)
;;; linux-specific.el ends here
