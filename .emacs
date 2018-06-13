;;; package --- Summary

;;; Commentary:
;;; Emacs config file

;;; Code:

;; disable toolbar, scrollbars, & menubar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; If you don't have MELPA in your package archives:
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; install use-package
(package-install 'use-package)
(require 'use-package)
(setq use-package-always-ensure t)

;; add evil
(use-package evil
  :init
  (evil-mode 1))

(evil-set-initial-state 'dired-mode 'emacs)

;; remap ; to : in evil and unmap q (because it's a pain in the ass and i don't use macros)
(with-eval-after-load 'evil-maps
   (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
   (define-key evil-motion-state-map (kbd ";") 'evil-ex)
   (define-key evil-motion-state-map (kbd "q") nil))

;; install flycheck
(use-package flycheck
  :init (global-flycheck-mode))

;; Install Intero
(use-package intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; use f6 to move to the next window
(global-set-key (kbd "<f6>") 'other-window)

;; install maggit
(use-package magit)

;; install themes
(use-package sublime-themes)

(if (display-graphic-p)
    (load-theme 'odersky t) )

;; install markdown-mode and set it to use pandoc
;; make sure you have pandoc installed!
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "pandoc"))

;; install neotree
(use-package neotree
  :bind
  ("<f8>" . neotree-toggle)
  (:map neotree-mode-map
  	("j" . neotree-next-line)
  	("k" . neotree-previous-line))
  :init
  (evil-set-initial-state 'neotree-mode 'emacs))

;; install helm
(use-package helm
  :bind
  ("<f4>" . helm-occur)
  ("<f7>" . helm-find-files)
  :init
  (helm-mode 1))

;; install elm-mode
(use-package elm-mode
  :init
  (setq company-backends '(company-elm)))

;; install flycheck-elm
(use-package flycheck-elm
  :hook flycheck-elm-setup)

;; install nix-mode
(use-package nix-mode)

;; install yaml-mode
(use-package yaml-mode)

;; install rust-mode
(use-package rust-mode
  :hook cargo-minor-mode)
(use-package cargo)

;; install clojure-mode
(use-package clojure-mode
  :hook info-clojure-minor-mode)

(use-package inf-clojure)

;; install/setup elfeed
(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  (:map elfeed-search-mode-map
	("j" . next-line)
	("k" . previous-line)
	("c" . elfeed-browsecomments-search)
	("y" . elfeed-download-yt))
  (:map elfeed-show-mode-map
	("j" . next-line)
	("k" . previous-line)
	("c" . elfeed-browsecomments-show))
  :init
  (setq elfeed-feeds
   '(("https://news.ycombinator.com/rss" hn hacker-news)
     ("https://www.reddit.com/r/denvernuggets.rss" nba nuggets)
     ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLlVlyGVtvuVlBMorPS3sGR4CM6lQ2F5dq" nba starters youtube)
     ("https://www.reddit.com/r/nba.rss" nba)
     ("https://www.reddit.com/r/programming.rss" programming)
     ("https://www.reddit.com/r/haskell.rss" programming haskell)
     ("http://feeds.feedburner.com/freakonomicsradio" freakonomics podcast)
     ("http://feeds.megaphone.fm/PPY4159411087" nba nuggets podcast)))
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'emacs))

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

;; install/setup emms
(use-package emms
  :bind
  ("C-x e" . emms)
  ("C-x s" . emms-streams)
  (:map emms-playlist-mode-map
   ("j" . next-line)
   ("k" . previous-line))
  :init
  (evil-set-initial-state 'emms-stream-mode 'emacs)
  (emms-all)
  (emms-default-players)
  (setq emms-stream-default-action "play")
  )

(use-package ripgrep)

(use-package powershell)

(use-package erc)

(use-package md4rd
  :bind (
	 :map md4rd-mode-map
	  ("q" . kill-this-buffer)
	  ("j" . next-line)
	  ("k" . previous-line))
  :init
  (evil-set-initial-state 'md4rd-mode 'emacs)
  (setq md4rd-subs-active '(haskell emacs programming)))

(defun stream-launch ()
  "Launches a twitch stream via streamlink."
  (interactive)
  (let (
	(url "twitch.tv/")
	(user "")
	(buffer-name (get-buffer-create "streamlink"))
	)
    (setq user (read-string "Enter stream name: "))
    (setq url (concat url user))
    (switch-to-buffer buffer-name)
    (setq-local buffer-read-only t)
    (make-process
     :name "streamlink"
     :buffer buffer-name
     :command (list "streamlink" url "360p")
     :stderr buffer-name
     )))

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

(use-package spaceline
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; disable backup files
(setq make-backup-files nil)

;; disable startup screen
(setq inhibit-startup-screen t)

(provide '.emacs)
;;; .emacs ends here
