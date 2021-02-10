;; configure text to speech (tts)
(lax-plist-put read-aloud-engines "jampal.en"
               '(cmd "cscript"
                     args ("C:\\Program Files\\Jampal\\ptts.vbs")) )

(setq read-aloud-engine "jampal.en")
