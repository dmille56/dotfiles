;; configure text to speech (tts)
(with-eval-after-load 'read-aloud
  (lax-plist-put read-aloud-engines "jampal.en"
                 '(cmd "cscript"
                       args ("C:\\Program Files\\Jampal\\ptts.vbs")))

  (setq read-aloud-engine "jampal.en"))
