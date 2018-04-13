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
(use-package evil)
(require 'evil)
(evil-mode 1)

;; remap ; to : in evil
(with-eval-after-load 'evil-maps
   (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
   (define-key evil-motion-state-map (kbd ";") 'evil-ex))

;; unmap q in evil (because it is a pain in the ass and I never use macros in vim)
(eval-after-load "evil-maps"
  (define-key evil-motion-state-map "q" nil))

;; install flycheck
(use-package flycheck
   :ensure t
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
(use-package neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle) ;; use f8 to toggle neotree

;; add hooks for neotree in evil
(add-hook 'neotree-mode-hook
	(lambda ()
	    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
	    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; install helm
(use-package helm)

;; use f7 to run helm-find-files
(global-set-key (kbd "<f7>") 'helm-find-files)

;; set helm-mode on
(helm-mode 1)

;; install elm-mode
(use-package elm-mode)

;; install flycheck-elm
(use-package flycheck-elm)
(add-hook 'flycheck-mode-hook 'flycheck-elm-setup)

(add-hook 'elm-mode-hook
          (lambda ()
            (setq company-backends '(company-elm))))

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
(package-install 'elfeed)
(evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'emacs)

(setq elfeed-feeds
      '(("https://news.ycombinator.com/rss" hn hacker-news)
        ("https://www.reddit.com/r/denvernuggets.rss" nba nuggets)
        ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLlVlyGVtvuVlBMorPS3sGR4CM6lQ2F5dq" nba starters youtube)
	("https://www.reddit.com/r/nba.rss" nba)
	("https://www.reddit.com/r/programming.rss" programming)
	("https://www.reddit.com/r/haskell.rss" programming haskell)
	("https://www.reddit.com/r/nixos.rss" programming nixos)
	))

(setq-default elfeed-search-filter "@1-week-ago +unread ")
(global-set-key (kbd "C-x w") 'elfeed)

(add-hook 'elfeed-search-mode-hook
          (lambda () (local-set-key (kbd "j") #'next-line)))
(add-hook 'elfeed-search-mode-hook
          (lambda () (local-set-key (kbd "k") #'previous-line)))

(defun elfeed-browsecomments-search ()
  (interactive)
  (elfeed-browsecomments (elfeed-search-selected :single)))

(defun elfeed-browsecomments-show ()
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
 
(add-hook 'elfeed-search-mode-hook
          (lambda () (local-set-key (kbd "c") #'elfeed-browsecomments-search)))

(add-hook 'elfeed-show-mode-hook
          (lambda () (local-set-key (kbd "c") #'elfeed-browsecomments-show)))

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
(package-install 'emms)
(require 'emms-setup)
(emms-all)
(emms-default-players)
(require 'emms-streams)

(global-set-key (kbd "C-x e") 'emms)

(add-hook 'emms-playlist-mode-hook
          (lambda () (local-set-key (kbd "j") #'next-line)))
(add-hook 'emms-playlist-mode-hook
          (lambda () (local-set-key (kbd "k") #'previous-line)))

(evil-set-initial-state 'emms-stream-mode 'emacs)
(setq emms-stream-default-action "play")

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

;; disable backup files
(setq make-backup-files nil)

;; disable startup screen
(setq inhibit-startup-screen t)

(provide '.emacs)
;;; .emacs ends here
