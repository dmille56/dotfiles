;; install/setup elfeed

(setq elfeed-feeds-blogs
      '(
        ("https://stratechery.com/feed" blog tech)
        ("http://www.paulgraham.com/rss.html" blog) ;; Paul Graham (Ycombinator founder)
        ("https://www.overcomingbias.com/feed" blog) ;; Robin Hanson (economist)
        ("https://betonit.substack.com/feed" blog) ;; Bryan Caplan (economist)
        ("https://astralcodexten.substack.com/feed" blog) ;; Scott Alexander
        ("http://daviddfriedman.blogspot.com/atom.xml" blog) ;; David Friedman (economist)
        ("http://feeds.feedburner.com/MeltingAsphalt" blog) ;; Kevin Simler (Elephant in the Brain)
        ("https://graymirror.substack.com/feed" blog)
        ))

(setq elfeed-feeds-podcasts
      '(
        ("http://feeds.feedburner.com/freakonomicsradio" pc podcast freakonomics)
        ("http://feeds.megaphone.fm/PPY4159411087" pc podcast nba nuggets)
        ))

(setq elfeed-feeds-misc
      '(
        ("https://news.ycombinator.com/rss" hn hacker-news)
        ("https://www.reddit.com/r/haskell.rss" programming haskell)
        ("https://www.denverstiffs.com/rss/current" nba nuggets)
        ))

(setq elfeed-all-the-feeds
      (append elfeed-feeds-blogs elfeed-feeds-podcasts elfeed-feeds-misc))

(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :init
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "g c") 'elfeed-browsecomments-search
    (kbd "g y") 'elfeed-download-yt)
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "g c") 'elfeed-browsecomments-search)
  (setq elfeed-feeds elfeed-all-the-feeds)
  (setq-default elfeed-search-filter "@1-week-ago +unread "))

(defun elfeed-download-yt ()
  "Downloads the selected elfeed entry."
  (elfeed-download-ytlink (elfeed-entry-link (elfeed-search-selected :single))))

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
     :command (list "youtube-dl" "-f" "18" link)
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
     :command (list "youtube-dl" "--extract-audio" "--audio-format" "mp3" link)
     :stderr buffer-name
     )))

(defun elfeed-browsecomments-search ()
  "Open the comments link from the currently selected elfeed entry."
  (interactive)
  (elfeed-browsecomments (elfeed-search-selected :single)))

(defun elfeed-browsecomments-show ()
  "Open the comments link from the currently selected elfeed entry."
  (interactive)
  (elfeed-browsecomments (elfeed-show-get-entry)))

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
		 (string-match "comments" nodeText)) node nil))
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
