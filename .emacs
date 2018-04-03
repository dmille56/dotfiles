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

;; add evil
(package-install 'evil)
(require 'evil)
(evil-mode 1)

;; remap ; to : in evil
(with-eval-after-load 'evil-maps
   (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
   (define-key evil-motion-state-map (kbd ";") 'evil-ex))

;; unmap q in evil (because it is a pain in the ass and I never use macros in vim)
(eval-after-load "evil-maps"
  (define-key evil-motion-state-map "q" nil))

;; install use-package
(package-install 'use-package)

;; install flycheck
(use-package flycheck
   :ensure t
     :init (global-flycheck-mode))

;; Install Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; use f6 to move to the next window
(global-set-key (kbd "<f6>") 'other-window)

;; install maggit
(package-install 'magit)

;; install themes
(package-install 'sublime-themes)

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
(package-install 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle) ;; use f8 to toggle neotree

;; add hooks for neotree in evil
(add-hook 'neotree-mode-hook
	(lambda ()
	    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
	    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; install helm
(package-install 'helm)

;; use f7 to run helm-find-files
(global-set-key (kbd "<f7>") 'helm-find-files)

;; set helm-mode on
(helm-mode 1)

;; install elm-mode
(package-install 'elm-mode)

;; install flycheck-elm
(package-install 'flycheck-elm)
(add-hook 'flycheck-mode-hook 'flycheck-elm-setup)

(add-hook 'elm-mode-hook
          (lambda ()
            (setq company-backends '(company-elm))))

;; install nix-mode
(package-install 'nix-mode)

;; install yaml-mode
(package-install 'yaml-mode)

;; install rust-mode
(package-install 'rust-mode)
(package-install 'cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; install clojure-mode
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

;; install inf-clojure (clojure repl)
(unless (package-installed-p 'inf-clojure)
  (package-refresh-contents)
  (package-install 'inf-clojure))

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; install/setup elfeed
(package-install 'elfeed)
(evil-set-initial-state 'elfeed-search-mode 'emacs)

(setq elfeed-feeds
      '(("https://news.ycombinator.com/rss" hn hacker-news)
        ("https://www.reddit.com/r/denvernuggets.rss" nba nuggets)
	("https://www.reddit.com/r/nba.rss" nba)))

(setq-default elfeed-search-filter "@1-week-ago +unread ")
(global-set-key (kbd "C-x w") 'elfeed)

(add-hook 'elfeed-search-mode-hook
          (lambda () (local-set-key (kbd "j") #'next-line)))
(add-hook 'elfeed-search-mode-hook
          (lambda () (local-set-key (kbd "k") #'previous-line)))

;; TODO: implement this
(defun elfeed-opencomments ()
   (interactive)
   (let ((entry (elfeed-search-selected :single)))
     (message (elfeed-entry-title entry))))
 
(add-hook 'elfeed-search-mode-hook
          (lambda () (local-set-key (kbd "x") #'elfeed-opencomments)))

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

;; install ripgrep
(package-install 'ripgrep)

;; install powershell
(package-install 'powershell)

;; disable backup files
(setq make-backup-files nil)

;; disable startup screen
(setq inhibit-startup-screen t)

(provide '.emacs)
;;; .emacs ends here
