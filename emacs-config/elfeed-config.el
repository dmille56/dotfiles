;; install/setup elfeed

(setq elfeed-feeds-blogs
      '(
        ("https://stratechery.com/feed" blog tech)
        ("http://www.paulgraham.com/rss.html" blog) ;; Paul Graham (Ycombinator founder)
        ("https://www.overcomingbias.com/feed" blog) ;; Robin Hanson (economist)
        ("https://betonit.substack.com/feed" blog) ;; Bryan Caplan (economist)
        ("https://astralcodexten.substack.com/feed" blog) ;; Scott Alexander
        ("http://daviddfriedman.blogspot.com/atom.xml" blog) ;; David Friedman (economist)
        ("https://daviddfriedman.substack.com/feed" blog) ;; David Friedman (economist)
        ("http://feeds.feedburner.com/MeltingAsphalt" blog) ;; Kevin Simler (Elephant in the Brain)
        ("https://graymirror.substack.com/feed" blog) ;; Curtis Yarvin
        ))

(setq elfeed-feeds-yt
      '(
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCfQgsKhHjSyRLOp9mnffqVg" yt fitness) ;; Renaissance Periodization
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCkZjTZNvuxq1CYMS3cwZa1Q" yt) ;; Huberman lab clips
	("https://www.youtube.com/feeds/videos.xml?playlist_id=PLkL7BvJXiqSTWYYJtqjo-cKEcHd9g4g5J" yt) ;; Chris Williamson clips
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCV_zy48AlwwGpdJEka1ay7w" yt fitness) ;; Garage Gym Reviews
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCzN7S0O87X-Q1CJqyWnJ9mw" yt)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCl8hzdP5wVlhuzNG3WCJa1w" yt nba nuggets) ;; Denver Nuggets yt channel
        ))

(setq elfeed-feeds-podcasts
      '(
        ("https://freakonomics.com/series/freakonomics-radio/rss" pc podcast freakonomics)
        ))

(setq elfeed-feeds-misc
      '(
        ;; ("https://news.ycombinator.com/rss" hn hacker-news)
        ("https://hnrss.org/frontpage?points=50&comments=20" hn hacker-news)
        ;; ("https://www.reddit.com/r/haskell.rss" r programming haskell)
        ("https://www.reddit.com/r/Vitruvian_Form.rss" r fitness)
        ("https://www.denverstiffs.com/rss/current" nba nuggets)
	("https://growth.design/case-studies/rss" tech)
        ))

(setq elfeed-all-the-feeds
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
    (kbd "g e") 'elfeed-download-ytaudio)
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "g c") 'elfeed-browsecomments-wrapper
    (kbd "g d") 'elfeed-open-in-chromium
    (kbd "g p") 'elfeed-play-mpv)
  (setq elfeed-feeds elfeed-all-the-feeds)
  (setq-default elfeed-search-filter "@1-week-ago -junk +unread ")
  :config
  ;; Filter debates out
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "^Playing With Fire"
                                :entry-title "\\(Debate\\|DEBATE\\|Live\\|LIVE\\|Speed\\|SPEED\\)"
                                :add 'junk
                                :remove 'unread))

  ;; Filter Open Threads out
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-title "^Astral Codex Ten"
                                :entry-title "Open Thread"
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

(defun elfeed-hook-filterer (entry)
  ;; TODO: implement me
  )

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
