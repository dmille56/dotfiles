;;; Youtube -- summary

;;; Commentary:

;;; modified from https://github.com/gRastello/youtube

(require 'cl-lib)
(require 'request)
(require 'json)
(require 'seq)

;;; Code:

(defvar youtube-api-url "https://invidious.xyz"
  "Url to an Invidious instance.")

(defvar youtube-videos '()
  "List of videos currently on display.")

(defvar youtube-search-term 'nil)

(defvar youtube-current-page 1)

(defvar youtube-published-date-time-string "%Y-%m-%d"
  "Time-string used to render the published date of the video.")

(defvar youtube-author-name-reserved-space 20
  "Number of characters reserved for the channel's name in the *youtube* buffer.
Note that there will always 3 extra spaces for eventual dots (for names that are
too long).")

(defvar youtube-title-video-reserved-space 100
  "Number of characters reserved for the video title in the *youtube* buffer.
Note that there will always 3 extra spaces for eventual dots (for names that are
too long).")

(defface youtube-video-published-face
  '((((class color) (background light)) (:foreground "#a0a"))
    (((class color) (background dark))  (:foreground "#7a7")))
  "Face used for the video published date.")

(defface youtube-channel-name-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used for channel names.")

(defface youtube-video-length-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used for the video length.")

(defface youtube-video-view-face
  '((((class color) (background light)) (:foreground "#00a"))
    (((class color) (background dark))  (:foreground "#aa7")))
  "Face used for the video views.")

(defun youtube-play (url)
  "Play URL with streamlink."
  (let* (
	 (buffer-name (get-buffer-create "youtube player"))
	 )
    (switch-to-buffer buffer-name)
    (setq-local buffer-read-only t)
    (make-process
     :name "streamlink"
     :buffer buffer-name
     :command (list "streamlink" "-p" "mpv" url "360p")
     :stderr buffer-name
     :sentinel #'youtube-play-sentinel)
    ))

(defun youtube--format-author (name)
  "Format a channel NAME to be inserted in the *youtube* buffer."
  (let* ((n (string-width name))
	 (extra-chars (- n youtube-author-name-reserved-space))
	 (formatted-string (if (<= extra-chars 0)
			       (concat name
				       (make-string (abs extra-chars) ?\ )
				       "   ")
			     (concat (seq-subseq name 0 youtube-author-name-reserved-space)
				     "..."))))
    (propertize formatted-string 'face 'youtube-channel-name-face)))

(defun youtube--format-title (title)
  "Format a video TITLE to be inserted in the *youtube* buffer."
  (let* ((n (string-width title))
	 (extra-chars (- n youtube-title-video-reserved-space))
	 (formatted-string (if (<= extra-chars 0)
			       (concat title
				       (make-string (abs extra-chars) ?\ )
				       "   ")
			     (concat (seq-subseq title 0 youtube-title-video-reserved-space)
				     "..."))))
    formatted-string))

(defun youtube--format-video-length (seconds)
  "Given an amount of SECONDS, format it nicely to be inserted in the *youtube* buffer."
  (let ((formatted-string (concat (format-seconds "%.2h" seconds)
				  ":"
				  (format-seconds "%.2m" (mod seconds 3600))
				  ":"
				  (format-seconds "%.2s" (mod seconds 60)))))
    (propertize formatted-string 'face 'youtube-video-length-face)))

(defun youtube--format-video-views (views)
  "Format video VIEWS to be inserted in the *youtube* buffer."
  (propertize (concat "[views:" (number-to-string views) "]") 'face 'youtube-video-view-face))

(defun youtube--format-video-published (published)
  "Format video PUBLISHED date to be inserted in the *youtube* buffer."
  (propertize (format-time-string youtube-published-date-time-string (seconds-to-time published))
	      'face 'youtube-video-published-face))

(defun youtube--insert-video (video)
  "Insert `VIDEO' in the current buffer."
  (insert (youtube--format-video-published (youtube-video-published video))
	  " "
	  (youtube--format-author (youtube-video-author video))
	  " "
	  (youtube--format-video-length (youtube-video-length video))
	  " "
	  (youtube--format-title (youtube-video-title video))
	  " "
	  (youtube--format-video-views (youtube-video-views video))))

(defun youtube--draw-buffer (videos)
  "Draws the youtube buffer i.e. clear everything and write down all VIDEOS in `youtube-videos'."
  (let ((inhibit-read-only t)
	(current-line      (line-number-at-pos)))
    (erase-buffer)
    (setf header-line-format (concat "Search results for "
    				     (propertize youtube-search-term 'face 'youtube-video-published-face)
    				     ", page "
    				     (number-to-string youtube-current-page)))
    (seq-do (lambda (v)
	      (youtube--insert-video v)
	      (insert "\n"))
	    videos)
    (goto-char (point-min))))

(cl-defstruct (youtube-video (:constructor youtube-video--create)
			  (:copier nil))
  "Information about a Youtube video."
  (title     "" :read-only t)
  (id        0  :read-only t)
  (author    "" :read-only t)
  (authorId  "" :read-only t)
  (length    0  :read-only t)
  (views     0  :read-only t)
  (published 0 :read-only t))

(cl-defun youtube--do-it (&key (data nil) &allow-other-keys)
  "Draw the buffer with youtube DATA results."
  (let*
      (
      (videos (-map (lambda (i)
		      (youtube-video--create
		       :title     (assoc-default 'title i)
		       :author    (assoc-default 'author i)
		       :authorId  (assoc-default 'authorId i)
		       :length    (assoc-default 'lengthSeconds i)
		       :id        (assoc-default 'videoId i)
		       :views     (assoc-default 'viewCount i)
		       :published (assoc-default 'published i)))
		    data))
      )
  (setq youtube-videos videos)
  (with-current-buffer (get-buffer-create "youtube")
    (pop-to-buffer (current-buffer))
    (youtube--draw-buffer videos)
    )))

(defun youtube-search ()
  "Search youtube."
  (interactive)
  (let* (
	 (search-term (read-string "Search Youtube: " 'nil 'nil 'nil))
	 (search-params '())
	 )
    (add-to-list 'search-params (cons 'q search-term))
    (setq youtube-search-term search-term)
    (request
      (concat youtube-api-url "/api/v1/search")
      :params search-params
      :parser 'json-read
      :success 'youtube--do-it)))

(defun youtube-get-current-video ()
 "Get the currently selected video."
 (nth (1- (line-number-at-pos)) youtube-videos))

(defun youtube-play-current-video ()
  "Play currently selected youtube video."
  (interactive)
  "Plays the currently selected video."
  (let*
      (
       (video (youtube-get-current-video))
       (url (concat "https://www.youtube.com/watch?v=" (youtube-video-id video)))
       )
    (youtube-play url)
    ))

(defun youtube-quit ()
  "Quit youtube buffer."
  (interactive)
  (quit-window))

(defvar youtube-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" #'youtube-quit)
    (define-key map "h" #'describe-mode)
    (define-key map "j" #'next-line)
    (define-key map "k" #'previous-line)
    (define-key map "s" #'youtube-search)
    (define-key map "p" #'youtube-play-current-video)
    ;; (define-key map ">" #'youtube-search-next-page)
    ;; (define-key map "<" #'youtube-search-previous-page)
    map)
  "Keymap for `youtube-mode'.")

(define-derived-mode youtube-mode text-mode
  "youtube-mode"
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (evil-set-initial-state 'youtube-mode 'emacs)
  (make-local-variable 'youtube-videos))

(defun youtube-buffer ()
  "Name for the main youtube buffer."
  (get-buffer-create "youtube"))

;;;###autoload
(defun youtube ()
  "Enter youtube."
  (interactive)
  (switch-to-buffer (youtube-buffer))
  (unless (eq major-mode 'youtube-mode)
    (youtube-mode))
  (when (seq-empty-p youtube-search-term)
    (call-interactively #'youtube-search)))

(provide 'youtube)
;;; youtube.el ends here
