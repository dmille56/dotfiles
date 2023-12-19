;;; package --- Summary

;;; Commentary:
;;; Emacs config file

;;; Code:

;; disable toolbar, scrollbars, & menubar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; insert space instead of tabs
(setq-default indent-tabs-mode nil)

;; enable reloading buffers on file changes
(global-auto-revert-mode t)

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

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package evil-leader
  :init
  (setq evil-leader/in-all-states t) ;; allows evil leader via "C-<leader>" in other states
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "p" 'helm-find-files
   "i" 'helm-occur
   "b" 'switch-to-buffer
   "k" 'kill-buffer
   "e" 'projectile-command-map
   "q" 'treemacs
   "y" 'magit
   "j" 'avy-goto-char
   "f" 'avy-goto-line
   "x" 'compile
   "c" 'flycheck-list-errors
   ; "l" 'run-lsp-command-map
   "g" 'keyboard-quit

   ;; window management
   "o" 'other-window
   "1" 'delete-other-windows
   "2" 'split-window-below
   "3" 'split-window-right

   ;; tab management
   "a" 'tab-bar-switch-to-prev-tab ;; maybe remove these keybindings (switch to prev/next)... faster to just use tab-bar-switch-to-tab or ctrl-tab to cycle
   "s" 'tab-bar-switch-to-next-tab
   "t" 'tab-new
   "w" 'tab-bar-switch-to-tab

   ;; harpoon
   "h" 'harpoon-toggle-quick-menu
   "H a" 'harpoon-quick-menu-hydra
   "H <return>" 'harpoon-add-file

   ;; u submenu
   "u d" 'ddg
   "u r" 'rg-menu
   "u g" 'chatgpt-shell
   )
;;  (evil-leader/set-key
;;    "L" '(lambda () (interactive) (which-key-show-keymap 'lsp-command-map))) ;; :TODO: remove this when get lsp-command-map working
  ;; (evil-define-minor-mode-key 'normal lsp-mode (kbd "SPC l") lsp-command-map)
  ;; (evil-leader/set-key-for-mode 'lsp-mode "L" 'lsp-command-map)
  (evil-leader/set-key-for-mode 'lsp-mode "l" 'lsp-command-map)
  (global-evil-leader-mode)
  )

(defun run-lsp-command-map ()
  (interactive)
  (which-key-show-keymap 'lsp-command-map))

;; add evil
;; note: "c-z" to toggle to/from emacs state
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; remap ; to : in evil (for efficiency) and unmap q (because it's a pain in the ass and i don't use macros)
(with-eval-after-load 'evil-maps
   (define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)
   (define-key evil-normal-state-map (kbd ";") 'evil-ex)
   (define-key evil-normal-state-map (kbd "q") 'quit-window))

;; install evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Set default font
(set-face-attribute 'default nil
                    :family "Deja Vu Sans Mono for Powerline"
                    :height 110
                    :weight 'normal
                    :width 'normal)
(semantic-mode 1) ;; use semantic

;; ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)

;; install flycheck
(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package treemacs
  :bind
  ("C-x y" . treemacs))

(use-package treemacs-projectile)
(use-package treemacs-evil)

;; Install Dante
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :bind
  (:map dante-mode-map
	("M->" . xref-find-definitions)
	("M-?" . xref-find-references))
  ;; :init
  ;; (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; (add-hook 'haskell-mode-hook 'company-mode)
  ;; (add-hook 'haskell-mode-hook 'dante-mode)
  )

;; Add Haskell linting on-the-fly to dante
(add-hook 'dante-mode-hook
   '(lambda () (flycheck-add-next-checker 'haskell-dante
					  '(warning . haskell-hlint))))

;; add attrap - for Dante mode
(use-package attrap
  :ensure t
  :bind (("C-x /" . attrap-attrap)))

(use-package reformatter) ;; needed for ormolu

;; Add auto formatting for Haskell code via Ormolu (requires ormolu exe to be installed)
(use-package ormolu
 :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
       ("C-c r" . ormolu-format-buffer)))

;; install lsp-mode
;; set prefix for lsp-command-keymap (few alternatives - "s-l", "C-l", "C-c l")
(setq lsp-keymap-prefix "C-l")

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (haskell-mode . lsp)
         (powershell-mode . lsp)
         (csharp-mode . lsp)
         (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp)

(setq lsp-disabled-clients '(omnisharp))

(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 (setq lsp-log-io t)
 (message "Loaded lsp-haskell")
)

;; lsp-mode optional add ons
(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  (:map lsp-mode-map
	("C-c ." . lsp-ui-doc-glance))
  :init
  (evil-define-key 'normal lsp-mode-map
    (kbd "C-c .") 'lsp-ui-doc-glance)
  :config
  (setq lsp-ui-doc-enable nil))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package yasnippet)

;; For C#
(use-package csproj-mode)
(add-to-list 'auto-mode-alist '("\\.*axaml\\'" . xml-mode))

(use-package which-key
  :config
  (which-key-mode))

;; use f5 to move to the next tab
(global-set-key (kbd "<f5>") 'tab-bar-switch-to-next-tab)
;; use f6 to move to the next window
(global-set-key (kbd "<f6>") 'other-window)

;; install maggit
(use-package magit)

;; install markdown-mode and set it to use pandoc
;; make sure you have pandoc installed!
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "pandoc"))

;; install helm
(use-package helm
  :bind
  ("<f3>" . helm-bookmarks)
  ("<f4>" . helm-occur)
  ("<f7>" . helm-find-files)
  ("C-x c i" . helm-semantic-or-imenu)
  :init
  (helm-mode 1)
  :config
  (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
  (define-key helm-map (kbd "<f8>") 'helm-keyboard-quit)
  )

;; install themes
(use-package sublime-themes)
(use-package spacemacs-theme)
(use-package dracula-theme)

(if (or (display-graphic-p) (daemonp))
    ;; (load-theme 'odersky t)
    ;;(load-theme 'spacemacs-dark t)
    (load-theme 'dracula t))

;; install nix-mode
(use-package nix-mode
  :mode "\\.nix\\'")

;; for nix formatting
(use-package format-all
  :init (add-hook 'nix-mode-hook 'format-all-mode))

;; install yaml-mode
(use-package yaml-mode)

;; install/setup emms
(use-package emms
  :bind
  ("C-x e" . emms)
  ("C-x s" . emms-streams)
  (:map emms-playlist-mode-map
   ("j" . next-line)
   ("k" . previous-line))
  :init
  (emms-all)
  (if (eq system-type 'gnu/linux)
      (setq emms-player-list '(emms-player-mpv))
    (emms-default-players))
  (setq emms-source-file-default-directory "~/Music/")
  (setq emms-stream-default-action "play")
  )

(use-package rg
  :init
  (rg-enable-default-bindings))
(use-package helm-rg)

(use-package powershell) ;; :TODO: fix multi-line comments issue in this package

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

(defun ddg ()
  "Search duck duck go."
  (interactive)
  (let (
	(terms (read-string "Enter duckduckgo search terms: ")))
    (browse-url (concat "https://duckduckgo.com/?q=" terms))))

(global-set-key (kbd "<f8>") 'keyboard-quit) ;; alias Ctl-g to f8 (to save your pinky)

(use-package spaceline
  :init
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (setq org-startup-folded "overview"))

(global-set-key "\C-xa" 'org-agenda)
(global-set-key "\M-x" 'evil-ex)

(use-package org-modern
  :config
  (with-eval-after-load 'org (global-org-modern-mode))
  )


(use-package all-the-icons) ;; remember need to run (all-the-icons-install-fonts) to install the fonts

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package ranger
  :bind
  ("C-x t" . ranger)
  ("<f9>" . ranger)
  (:map ranger-mode-map
	("C-<tab>" . ranger-next-tab))
  :config
  (setq ranger-cleanup-on-disable t)
  (setq ranger-cleanup-eagerly t))

(use-package nyan-mode
  :init
  (nyan-mode))

(use-package projectile
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :init
  (projectile-mode +1))

(use-package helm-projectile
  :init
  (helm-projectile-on))

(use-package cheat-sh
  :ensure t
  :bind (("C-c ?" . cheat-sh)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package restart-emacs)

(use-package avy
  :init
  (global-set-key (kbd "C-:") 'avy-goto-line)
  (global-set-key (kbd "C-;") 'avy-goto-char))

(use-package diff-hl
  :init
  (global-diff-hl-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package read-aloud
  :init
  (global-set-key (kbd "C-C r p") 'read-aloud-buf)
  (global-set-key (kbd "C-C r s") 'read-aloud-stop)
  (global-set-key (kbd "C-C r t") 'read-aloud-this))

(use-package page-break-lines) ;; needed for dashboard

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))
  )

(use-package dashboard-hackernews)

(use-package golden-ratio
  :init
  (golden-ratio-mode 1)
  )

(use-package solaire-mode
  :init
  (solaire-global-mode)
  )

(use-package spacious-padding
  :hook (emacs-startup . spacious-padding-mode) ;; prevents bug (where there's a white line between frames) by having it init at emacs-startup instead of after-init (which runs before emacs-startup)
  )

;; :TODO: add more svg-tag tags
;; :NOTE: svg-tag-mode example
(use-package svg-tag-mode
  :init
  (setq svg-tag-tags
        '(
          (":TODO:" . ((lambda (tag) (svg-tag-make "TODO"))))
          (":NOTE:" . ((lambda (tag) (svg-tag-make "NOTE"))))
          )
        )
  (add-hook 'emacs-lisp-mode-hook 'svg-tag-mode)
  )

(use-package csv-mode)

(use-package beacon
  :init
  (beacon-mode 1))

(use-package selectric-mode) ;; haha this is the funniest package

(use-package harpoon
  :config
  ;; You can use this hydra menu that have all the commands
  (global-set-key (kbd "C-c a") 'harpoon-quick-menu-hydra)
  (global-set-key (kbd "C-c h <return>") 'harpoon-add-file)

  ;; And the vanilla commands
  (global-set-key (kbd "C-c h f") 'harpoon-toggle-file)
  (global-set-key (kbd "C-c h h") 'harpoon-toggle-quick-menu)
  (global-set-key (kbd "C-c h c") 'harpoon-clear)
  (global-set-key (kbd "C-c h 1") 'harpoon-go-to-1)
  (global-set-key (kbd "C-c h 2") 'harpoon-go-to-2)
  (global-set-key (kbd "C-c h 3") 'harpoon-go-to-3)
  (global-set-key (kbd "C-c h 4") 'harpoon-go-to-4)
  (global-set-key (kbd "C-c h 5") 'harpoon-go-to-5)
  (global-set-key (kbd "C-c h 6") 'harpoon-go-to-6)
  (global-set-key (kbd "C-c h 7") 'harpoon-go-to-7)
  (global-set-key (kbd "C-c h 8") 'harpoon-go-to-8)
  (global-set-key (kbd "C-c h 9") 'harpoon-go-to-9)
  )

(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
        (auth-source-pass-get 'secret "OPENAI_API_KEY")))))

(use-package editorconfig) ;; dependency for copilot

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

;; :TODO: figure out how to configure these packages (dumb-jump, corfu, and orderless)
(use-package dumb-jump) ;; :TODO: add support for powershell to this package

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  ) ;; dumb code completion

(use-package orderless
  :custom
  (completion-styles '(basic partial-completion orderless))
 )

(define-key transient-map (kbd "<f8>") 'transient-quit-all)
(define-key transient-map (kbd "ESC") 'transient-quit-all)
 
;; enable tab-bar-mode
(tab-bar-mode)

(evil-define-key 'normal eshell-mode-map
  (kbd "C-r") 'helm-eshell-history)

(evil-define-key 'insert eshell-mode-map
  (kbd "C-r") 'helm-eshell-history)

;; Set up zone-matrix
;; (straight-use-package
;;  '(zone-matrix :type git :host github :repo "ober/zone-matrix"))
;; 
;; (require 'zone-matrix)
;; (require 'zone-matrix-settings)
;; (require 'zone-settings)
;; (setq zone-programs [zone-matrix])
;; (zone-when-idle 60)

;; lsp-mode performance settings
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-completion-provider :capf)

;; disable backup files
;; (setq make-backup-files nil)

;; set auto save files to the temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; disable startup screen
(setq inhibit-startup-screen t)

(setq-local main-dir "~/dotfiles/emacs-config/")

(load-file
 (concat main-dir "twitchy.el"))

(global-set-key (kbd "M-p") 'twitchy-play-stream)

(load-file
 (concat main-dir "youtube.el"))

(global-set-key (kbd "M-y") 'youtube)

(load-file
 (concat main-dir "elfeed-config.el"))

(if (eq system-type 'gnu/linux)
    (load-file
     (concat main-dir "linux-specific.el")))

(if (eq system-type 'windows-nt)
    (load-file
     (concat main-dir "windows-specific.el")))

(provide 'main)
;;; main.el ends here
