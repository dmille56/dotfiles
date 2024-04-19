;;; windows-specific.el --- Windows specific emacs configuration
;;; Commentary:
;;; Code:

(setq-default browse-url-generic-program "firefox")

;; configure text to speech (tts)
(with-eval-after-load 'read-aloud
  (plist-put read-aloud-engines "jampal.en"
                 '(cmd "cscript"
                       args ("C:\\Program Files\\Jampal\\ptts.vbs")))

  (setq-default read-aloud-engine "jampal.en"))

(provide 'windows-specific)
;;; windows-specific.el ends here
