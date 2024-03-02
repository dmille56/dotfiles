;;; elfeed-config --- Summary

;; setup elfeed configuration

;;; Commentary:

;;; Code:

(setq-default elfeed-feeds-blogs
      '(
        ("https://stratechery.com/feed" blog tech)
        ;; ("http://www.paulgraham.com/rss.html" blog) ;; Paul Graham (Ycombinator founder)
        ("https://www.overcomingbias.com/feed" blog) ;; Robin Hanson (economist)
        ("https://betonit.substack.com/feed" blog) ;; Bryan Caplan (economist)
        ("https://astralcodexten.substack.com/feed" blog) ;; Scott Alexander
        ("https://daviddfriedman.substack.com/feed" blog) ;; David Friedman (economist)
        ("http://feeds.feedburner.com/MeltingAsphalt" blog) ;; Kevin Simler (Elephant in the Brain)
        ("https://graymirror.substack.com/feed" blog) ;; Curtis Yarvin
        ))

(setq-default elfeed-feeds-yt
      '(
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCfQgsKhHjSyRLOp9mnffqVg" yt fitness) ;; Renaissance Periodization
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCkZjTZNvuxq1CYMS3cwZa1Q" yt i) ;; Huberman Lab Clips
	("https://www.youtube.com/feeds/videos.xml?playlist_id=PLkL7BvJXiqSTWYYJtqjo-cKEcHd9g4g5J" yt i) ;; Chris Williamson Clips
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCV_zy48AlwwGpdJEka1ay7w" yt fitness) ;; Garage Gym Reviews
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXR5UyxWQdZ50pWyNn5FyoQ" yt fitness) ;; Connect The Watts
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCFGCfbYPyFpITa0mbwbTmhA" yt fitness i) ;; Kaizen DIY Gym
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCzN7S0O87X-Q1CJqyWnJ9mw" yt i)
        ("https://www.youtube.com/feeds/videso.xml?channel_id=UCsEPI9OwGEw5Lm0E7Paq62g" yt i)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCl8hzdP5wVlhuzNG3WCJa1w" yt nba nuggets sports) ;; Denver Nuggets yt channel
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCDfVqMBTxstChZ5YVrrXPPQ" yt nba nuggets sports) ;; Locked on Nuggets
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCew5br5cO1ZKO7Z_F1WA8Bg" yt nfl broncos sports) ;; That's Good Sports
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsy9I56PY3IngCf_VGjunMQ" yt) ;; Peter Zeihan
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkh_BQ1i34Y0Ij7QxCRwzrw" yt) ;; Boyscast Clips
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSduXBjCHkLoo_y9ss2xzXw" yt) ;; PsycHacks
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyaN6mg5u8Cjy2ZI4ikWaug" yt i) ;; My First Million
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" yt emacs linux) ;; DistroTube (linux/emacs tips)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCCRdRbI93UGW0AZttVH3SbA" yt emacs) ;; Gopar (emacs tips)
        ))

(setq-default elfeed-feeds-podcasts
      '(
        ("https://freakonomics.com/series/freakonomics-radio/rss" pc podcast freakonomics e)
        ))

(setq-default elfeed-feeds-misc
      '(
        ;; ("https://news.ycombinator.com/rss" hn hacker-news)
        ("https://hnrss.org/frontpage?points=50&comments=20" hn hacker-news)
        ;; ("https://www.reddit.com/r/haskell.rss" r programming haskell)
        ("https://www.reddit.com/r/Vitruvian_Form.rss" r fitness)
        ("https://ihrss.io/top/month" indiehackers)
        ))

(setq-default elfeed-all-the-feeds
      (append elfeed-feeds-blogs elfeed-feeds-yt elfeed-feeds-podcasts elfeed-feeds-misc))

(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :init
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "g c") 'elfeed-browsecomments-wrapper
    (kbd "g d") 'elfeed-open-in-chromium
    (kbd "g p") 'elfeed-play-mpv
    (kbd "g y") 'elfeed-download-yt
    (kbd "g e") 'elfeed-download-ytaudio
    (kbd "f i") (lambda () (interactive) (elfeed-search-set-filter "@1-week-ago -junk -short +unread -hn +i"))
    (kbd "f o") (lambda () (interactive) (elfeed-search-set-filter "@1-week-ago -junk -short +unread -hn -i"))
    (kbd "f h") (lambda () (interactive) (elfeed-search-set-filter "@1-week-ago -junk -short +unread +hn"))
    (kbd "r") 'elfeed-search-untag-all-unread
    )
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "g c") 'elfeed-browsecomments-wrapper
    (kbd "g d") 'elfeed-open-in-chromium
    (kbd "g p") 'elfeed-play-mpv
    )
  (setq elfeed-feeds elfeed-all-the-feeds)
  (setq-default elfeed-search-filter "@1-week-ago -junk -short +unread -hn +i")
  :config
  ;; Filter debates out
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "^Playing With Fire"
                                :entry-title "\\(Debate\\|DEBATE\\|Live\\|LIVE\\|Speed\\|SPEED\\|Debating\\|DEBATING\\)"
                                :add 'junk
                                :remove 'unread))

  ;; Filter out videos I don't care about
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "^ThatsGoodSports"
                                :entry-title "\\(winners . losers)\\|power rankings\\|preview\\|live\\|Live\\|LIVE\\)"
                                :add 'junk
                                :remove 'unread))

  ;; Filter non-blog posts
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "^Astral Codex Ten"
                                :entry-title "\\(Open Thread\\|Highlights From\\|Links For\\|Mantic Monday\\|Meetup\\)"
                                :add 'junk
                                :remove 'unread))

  ;; Filter Hiring Threads out
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "^Hacker News"
                                :entry-title "\\(Is Hiring\\|Launch HN\\)"
                                :add 'junk
                                :remove 'unread))

  ;; Filter Hiring Threads out
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "^Denver Nuggets"
                                :entry-title '(not "Game Recap")
                                :add 'junk
                                :remove 'unread))

  ;; Add short tag to youtube shorts
  (add-hook 'elfeed-new-entry-hook #'elfeed-tag-yt-short)
  )

(use-package mpv)

(use-package elfeed-tube
  :ensure t
  :after elfeed mpv
  :demand t
  :config
  (elfeed-tube-setup)
  
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :ensure t
  :after elfeed-tube
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

(defun elfeed-cur-entry ()
  (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))

(defun elfeed-open-in-chromium ()
  (interactive)
  "Opens the selected elfeed entry in chromium."
  (let* (
        (entry (elfeed-cur-entry))
        (link (elfeed-entry-link entry))
        )
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (browse-url-chromium link)))

(defun mpv-play-yt (link &optional format)
  (interactive)
  (let*
      (
       (format1 (if format format "18"))
       (formatArg (s-concat "--ytdl-format=" format1))
      )
    (print "Starting mpv...")
    (mpv-start formatArg link)))

(defun elfeed-play-mpv ()
  (interactive)
  "Play the selected elfeed entry using mpv."
  (let* (
        (entry (elfeed-cur-entry))
        (link (elfeed-entry-link entry))
        )
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (mpv-play-yt link)))

(defun elfeed-download-yt ()
  (interactive)
  "Downloads the selected elfeed entry using youtube-dl."
  (elfeed-download-ytlink (elfeed-entry-link (elfeed-cur-entry))))

(defun elfeed-download-ytaudio ()
  (interactive)
  "Downloads the selected elfeed entry using youtube-dl and gets the audio."
  (elfeed-download-ytaudiolink (elfeed-entry-link (elfeed-cur-entry))))

(defun elfeed-download-ytlink (link)
  "Downloads a LINK to a youtube video via youtube-dl."
  (let (
	(buffer-name (get-buffer-create "youtube-dl")))
    (switch-to-buffer buffer-name)
    (setq-local default-directory "~/Videos/youtube")
    (insert (concat "Downloading video to directory: " default-directory))
    (insert "\n")
    (setq-local buffer-read-only t)
    (make-process
     :name "youtube-dl"
     :buffer buffer-name
     :command (list "yt-dlp" "-f" "18" link)
     :stderr buffer-name
     )))

(defun elfeed-download-ytaudiolink (link)
  "Downloads the audio to a LINK to a youtube video via youtube-dl."
  (let (
	(buffer-name (get-buffer-create "youtube-dl-audio")))
    (switch-to-buffer buffer-name)
    (setq-local default-directory "~/Music/youtube")
    (insert (concat "Downloading mp3 to directory: " default-directory))
    (insert "\n")
    (setq-local buffer-read-only t)
    (make-process
     :name "youtube-dl"
     :buffer buffer-name
     :command (list "yt-dlp" "--extract-audio" "--audio-format" "mp3" link)
     :stderr buffer-name
     )))

(defun elfeed-browsecomments-wrapper ()
  "Open the comments link from the currently selected elfeed entry."
  (interactive)
  (elfeed-browsecomments (elfeed-cur-entry)))

(defun elfeed-browsecomments (entry)
  "Open the comments link from a feed ENTRY with 'browse-url'."
   (let* (
	 (content (elfeed-deref (elfeed-entry-content entry)))
	 (root (with-temp-buffer
		 (insert content)
		 (libxml-parse-html-region (point-min) (point-max))))
	 (commentNode (elfeed-getcommentnode root))
	 )
     (if commentNode (progn
		       (browse-url (dom-attr commentNode 'href))
		       (elfeed-untag entry 'unread)
		       (elfeed-search-update-entry entry))
       (message "Unable to load comments"))))

(defun elfeed-iscomment (node)
  "Check if a NODE is a comment link."
  (if (listp node)
      (let (
	    (nodeName (dom-tag node))
	    (nodeText (dom-text node))
	    (nodeLink (dom-attr node 'href))
	    )
	    (if
		(and
		 (string= nodeName "a")
		 nodeLink
                 (or
                  (string-match "comments" nodeText)
                  (string-match "https://news.ycombinator.com/item" nodeText))
                 ) node nil))
    nil))

(defun elfeed-getcommentnode (node)
  "Get a comment link from the root NODE."
  (cond
   ((listp node)
    (if (elfeed-iscomment node) node
      (let* (
	     (results (mapcar 'elfeed-getcommentnode (xml-node-children node)))
	     (filtered (remove nil results))
	     (result (car filtered)))
	result)))))

(defun youtube-dl-audio ()
  "Download the audio of a youtube video via youtube-dl."
  (interactive)
  (let ((link ""))
    (setq link (read-string "Enter youtube link: "))
    (elfeed-download-ytaudiolink link)))

(defun youtube-dl-video ()
  "Download the audio of a youtube video via youtube-dl."
  (interactive)
  (let ((link ""))
    (setq link (read-string "Enter youtube link: "))
    (elfeed-download-ytlink link)))

(defun yt-parse-duration (duration-str)
  "Parse a duration string in the format 'D:HH:MM:SS' and return the total number of seconds."
  (let* ((components (mapcar #'string-to-number (split-string duration-str ":")))
         (num-components (length components)))
    (cond
     ((= num-components 4)
      (+ (* (nth 0 components) 86400)
         (* (nth 1 components) 3600)
         (* (nth 2 components) 60)
         (nth 3 components)))
     ((= num-components 3)
      (+ (* (nth 0 components) 3600)
         (* (nth 1 components) 60)
         (nth 2 components)))
     ((= num-components 2)
      (+ (* (nth 0 components) 60)
         (nth 1 components)))
     ((= num-components 1)
      (nth 0 components))
     (t 0))))

(defun yt-video-is-short (url)
  "Check the duration of a YouTube video using yt-dlp."
  (let*
      (
       (duration-str (shell-command-to-string (concat "yt-dlp --get-duration '" url "'")))
       (duration (yt-parse-duration duration-str))
       )
    (if (boundp 'duration)
        (if (> duration 120) nil t)
      nil ;; If unable to parse return nil as fail-safe
      )
    )
  )

(defun elfeed-tag-yt-short (entry)
  (let*
      (
       (link (elfeed-entry-link entry))
       (is-youtube-link (string-match-p "youtube\\.com" link))
       (is-short (if is-youtube-link (yt-video-is-short link) nil))
       )
    (if is-short (elfeed-tag entry 'short))
    )
  )

(provide 'elfeed-config)
;;; elfeed-config.el ends here
