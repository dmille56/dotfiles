;;; package --- Summary

;;; Commentary:
;;; Emacs config file :smile:👍💪

;;; Code:

;; Some same defaults to start:

;; disable toolbar, scrollbars, & menubar
(if (require 'tool-bar nil 'noerror) (tool-bar-mode -1))
(if (require 'scroll-bar nil 'noerror) (scroll-bar-mode -1))
(menu-bar-mode -1)

(delete-selection-mode 1)
(electric-indent-mode -1)
;; (electric-pair-mode 1)
;; (global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(setq org-edit-src-content-indentation 0)

;; Workaround to make org-tempo work with electric-pair-mode
;; (add-hook 'org-mode-hook (lambda ()
;;                            (setq-local electric-pair-inhibit-predicate
;;                            `(lambda (c)
;;                               (if (char-equal c ?<) t(,electric-pair-inhibit-predicate c))))))

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

;; How to check for system type:
;; (if (eq system-type 'gnu/linux)
;; (if (eq system-type 'windows-nt)

(setq my/config-machine
      (cond
             ((string-equal (system-name) "van") 'pc)
             ((string-equal (system-name) "localhost") 'phone)
             ((string-equal (system-name) "LOCAL-D31D4TRU3") 'work)
             (t 'pc) ;; fall-back to pc
             ))

(use-package hydra)
(use-package posframe)

;; (use-package helm-posframe
;;   :config
;;   (helm-posframe-enable))

;; (use-package hydra-posframe
;; ;; :NOTE: not recommended way to install... but I didn't want to install el-get
;; ;; https://github.com/Ladicle/hydra-posframe
;;   :straight (:host github :repo "ladicle/hydra-posframe" :files ("*.el"))
;;   :hook (after-init . hydra-posframe-mode))

(setq evil-want-keybinding nil)

(defhydra hydra-tab-management (:exit t)
  "tabs"
  ("n" tab-new "new")
  ("w" tab-bar-switch-to-tab "switch")
  ("1" (lambda () (interactive) (tab-bar-select-tab 1)) "1")
  ("2" (lambda () (interactive) (tab-bar-select-tab 2)) "2")
  ("3" (lambda () (interactive) (tab-bar-select-tab 3)) "3")
  ("4" (lambda () (interactive) (tab-bar-select-tab 4)) "4")
  ("5" (lambda () (interactive) (tab-bar-select-tab 5)) "5")
  ("6" (lambda () (interactive) (tab-bar-select-tab 6)) "6")
  ("7" (lambda () (interactive) (tab-bar-select-tab 7)) "7")
  ("8" (lambda () (interactive) (tab-bar-select-tab 8)) "8")
  ("9" (lambda () (interactive) (tab-bar-select-tab 9)) "9")
  ("0" (lambda () (interactive) (tab-bar-select-tab 10)) "10")
  ("m1" (lambda () (interactive) (tab-bar-move-tab-to 1)) "move 1")
  ("m2" (lambda () (interactive) (tab-bar-move-tab-to 2)) "move 2")
  ("m3" (lambda () (interactive) (tab-bar-move-tab-to 3)) "move 3")
  ("m4" (lambda () (interactive) (tab-bar-move-tab-to 4)) "move 4")
  ("m5" (lambda () (interactive) (tab-bar-move-tab-to 5)) "move 5")
  ("m6" (lambda () (interactive) (tab-bar-move-tab-to 6)) "move 6")
  ("m7" (lambda () (interactive) (tab-bar-move-tab-to 7)) "move 7")
  ("m8" (lambda () (interactive) (tab-bar-move-tab-to 8)) "move 8")
  ("m9" (lambda () (interactive) (tab-bar-move-tab-to 9)) "move 9")
  ("m0" (lambda () (interactive) (tab-bar-move-tab-to 10)) "move 10")
  )

(defhydra hydra-leader-misc (:exit t)
  "misc"
  ("d" ddg "ddg search" )
  ("D" dired "dired" )
  ("s" rg-menu "rg search")
  ("g" chatgpt-shell "chatgpt")
  ("b" helm-bookmarks "bookmarks")
  ("B" bookmark-set "set bookmark")
  ("t" ranger "ranger")
  ("T" hydra-org-timer/body "timers")
  ("y" treemacs "tree")
  ("w" elfeed "elfeed")
  ("r" helm-recentf "recent")
  ("a" org-agenda "agenda")
  ("P" spacious-padding-mode "padding toggle")
  ("S" selectric-mode "typewriter toggle")
  ("G" hydra-game/body "game")
  ("c" calc "calculator" )
  ("h" help-for-help "emacs help")
  ("." clippy-describe-function "clippy func")
  (">" clippy-describe-variable "clippy var")
  ("e" revert-buffer "revert buffer")
  )

(defhydra hydra-game (:exit t)
  "game"
  ("2" 2048-game "2048")
  ("t" tetris "tetris")
  ("d" doctor "doctor")
  ("s" sudoku "sudoku")
  ("p" pacmacs-start "pacmacs")
  )

(defhydra hydra-elisp-mode (:exit t)
  "elisp"
  ("r" eval-region "eval region")
  ("B" eval-buffer "eval buffer")
  ("l" (lambda () (interactive) (load-file (buffer-file-name (window-buffer)))) "load current file")
  ("L" load-file "load file")
  ("d" eval-defun "eval defun")
  ("e" eval-last-sexp "eval last sexp")
  ("D" edebug-defun "edebug function")
  ("s" paredit-forward-slurp-sexp "slurp")
  ("b" paredit-forward-barf-sexp "barf")
  )

(defhydra hydra-doc-view-mode (:exit t)
  "doc-view"
  ("g" doc-view-goto-page "go to page")
  ("n" doc-view-next-page "next page")
  ("p" doc-view-previous-page "prev page")
  )

(defhydra hydra-org-mode (:exit t)
  "org"
  ("o" org-open-at-point "open")
  ("l" org-insert-link "insert link")
  ("j" avy-org-goto-heading-timer "go to heading")
  ("r" avy-org-refile-as-child "avy refile child")
  ("c" org-make-toc-insert "insert table of contents")
  ("C" org-make-toc-set "edit table of contents")
  ("s" org-schedule "schedule")
  ("t" org-todo "todo")
  ("T" org-set-tags-command "set tags")
  ("e" org-export-dispatch "org export")
  )

(defhydra hydra-python-mode (:exit t)
  "python"
  ("f" python-shell-send-file "run file")
  ("b" python-shell-send-buffer "run buffer")
  ("l" python-shell-send-statement "run statement")
  ("p" run-python "start python shell")
  ("r" python-shell-send-region "run region")
  ("z" python-shell-switch-to-shell "switch to shell")
  )

 (defhydra hydra-org-timer (:exit t)
  "timer"
  ("t" org-timer-set-timer "set timer")
  ("s" org-timer-stop "stop")
  ("r" org-timer-start "start")
  ("p" org-timer-pause-or-continue "pause/continue")
  )

 (defhydra hydra-shell-run (:exit t)
   "run shell commands"
  ("R" shell-command-on-region "run region")
  ("x" async-shell-command "async run command")
  ("X" shell-command "run command")
  ("l" eshell-extensions-send-cur-line-to-eshell "run cur line in eshell")
  ("r" eshell-extensions-send-cur-region-to-eshell "run region in eshell")
  ("e" eshell-extensions-send-string-to-eshell "run command in eshell")
  )

 (defhydra hydra-emms-mode (:exit t)
   "emms"
  ("e" emms "emms")
  ("s" emms-streams "emms streams")
  )

 (defhydra hydra-evil-macros (:exit t)
   "macros"
  ("r" evil-record-macro "record macro") ;; traditionally q in vim/evil
  ("x" evil-execute-macro "execute macro") ;; also @ followed by macro register in vim/evil
  ("l" evil-execute-last-recorded-macro "execute last macro") ;; also @@ in vim/evil
  )

 (defhydra hydra-hl-todo (:exit t)
   "hl-todo"
  ("p" hl-todo-previous "previous")
  ("n" hl-todo-next "next")
  ("o" hl-todo-occur "occur")
  ("i" hl-todo-insert "insert todo")
  ("r" hl-todo-rgrep "rgrep")
  )

 (defhydra hydra-org-roam (:exit t)
   "org-roam"
  ("l" org-roam-buffer-toggle "buf toggle")
  ("f" org-roam-node-find "find")
  ("g" org-roam-graph "graph")
  ("i" org-roam-node-insert "node insert")
  ("c" org-roam-capture "capture")
  ("b" my/org-roam-capture-inbox "capture inbox")
  ("1" my/org-roam-capture-1-on-1 "capture 1-on-1")
  ("m" my/org-roam-capture-meeting "capture meeting")
  ("h" my/org-roam-capture-hype-doc "capture hype doc")
  ("j" org-roam-dailies-capture-today "capture today")
  )

(use-package evil-leader
  :init
  (setq evil-leader/in-all-states t) ;; allows evil leader via "C-<leader>" in other states
  :config
  (require 'evil-leader)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "p" 'helm-find-files
   "P" 'check-parens
   "i" 'helm-occur
   "I" 'consult-line
   "b" 'switch-to-buffer
   "k" 'kill-buffer
   "e" 'projectile-command-map
   ;; "j" 'avy-goto-char
   "d" 'pop-global-mark ;; go back where you were before a jump with say... avy
   "j" 'avy-goto-word-1
   "f" 'avy-goto-line
   "x" 'compile
   "E" 'hydra-shell-run/body
   "X" 'recompile
   "c" 'flycheck-list-errors
   "C" 'hydra-hl-todo/body
   ;; "l" 'run-lsp-command-map
   "g" 'magit
   "R" 'query-replace-regexp
   "v" 'helm-semantic-or-imenu
   "a" 'link-hint-open-link
   "M" 'hydra-emms-mode/body
   "m" 'hydra-evil-macros/body
   "O" 'hydra-org-roam/body

   ;; window management
   "o" 'other-window
   "1" 'delete-other-windows
   "2" 'split-window-below
   "3" 'split-window-right

   ;; tab management
   "t" 'hydra-tab-management/body

   ;; harpoon
   "h" 'harpoon-toggle-quick-menu
   "H a" 'harpoon-quick-menu-hydra
   "H <return>" 'harpoon-add-file

   ;; u submenu
   "u" 'hydra-leader-misc/body

   ;; leave r for mode specific keymaps
   )
;;  (evil-leader/set-key
;;    "L" '(lambda () (interactive) (which-key-show-keymap 'lsp-command-map))) ;; :TODO: remove this when get lsp-command-map working
  ;; (evil-define-minor-mode-key 'normal lsp-mode (kbd "SPC l") lsp-command-map)
  ;; (evil-leader/set-key-for-mode 'lsp-mode "L" 'lsp-command-map)
  (evil-leader/set-key-for-mode 'lsp-mode "l" 'lsp-command-map)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "r" 'hydra-elisp-mode/body)
  (evil-leader/set-key-for-mode 'lisp-interaction-mode "r" 'hydra-elisp-mode/body)
  (evil-leader/set-key-for-mode 'doc-view-mode "r" 'hydra-doc-view-mode/body)
  (evil-leader/set-key-for-mode 'org-mode "r" 'hydra-org-mode/body)
  (evil-leader/set-key-for-mode 'python-mode "r" 'hydra-python-mode/body)
  (evil-leader/set-key-for-mode 'python-ts-mode "r" 'hydra-python-mode/body)
  (global-evil-leader-mode)
  )

(defun run-lsp-command-map ()
  "Show lsp-command-map."
  (interactive)
  (which-key-show-keymap 'lsp-command-map))

;; add evil
;; note: "c-z" to toggle to/from emacs state
(use-package evil
  :init
  ;; (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  ;; (setq evil-want-keybinding nil)
  ;; (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

;; remap ; to : in evil (for efficiency) and unmap q (because it's a pain in the ass and i don't use macros)
(with-eval-after-load 'evil-maps
   ;; (define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char) ;; uncomment if you still want to be able to use the original ';' key
   (define-key evil-normal-state-map (kbd ";") 'evil-ex)
   (define-key evil-normal-state-map (kbd "M-x") 'helm-M-x)
   (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
   (define-key evil-normal-state-map (kbd "gZ") 'zoxide-find-file)
   (define-key evil-normal-state-map (kbd "gb") 'evil-jump-backward)
   ;; (define-key evil-visual-state-map (kbd ":") 'evil-repeat-find-char)
   (define-key evil-visual-state-map (kbd ";") 'evil-ex)
   (define-key evil-visual-state-map (kbd "M-x") 'helm-M-x)
   (define-key evil-normal-state-map (kbd "q") 'quit-window))

;; install evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-tutor)

;; :TODO: figure out how to use this package effectively
(use-package evil-textobj-tree-sitter)

;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

(define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
(define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))

;; You can also bind multiple items and we will match the first one we can find
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
(define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))

;; Set default font
(set-face-attribute 'default nil
                    :family "Deja Vu Sans Mono for Powerline"
                    :height 110
                    :weight 'normal
                    :width 'normal)
;; (semantic-mode 1) ;; use semantic :TODO: re-enable when figure out why it kept throwing error

;; ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)
(evil-define-key 'normal dired-mode-map ";" 'evil-ex)

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
  :hook (;; replace X-mode with concrete major-mode(e. g. python-mode)
         ;; (haskell-mode . lsp)
         ;; (powershell-mode . lsp)
         ;; (csharp-mode . lsp)
         ;; (python-mode . lsp)
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
  ("<f4>" . helm-occur)
  ("<f7>" . helm-find-files)
  ("C-x c i" . helm-semantic-or-imenu)
  :init
  (global-set-key (kbd "M-x") #'helm-M-x)
  (helm-mode 1)
  :config
  (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
  (define-key helm-map (kbd "<f8>") 'helm-keyboard-quit)
  )

;; install themes
;; :TODO: look into installing and using circadian.el (to switch between light/dark at sunset)
(use-package sublime-themes)
(use-package spacemacs-theme)
(use-package dracula-theme)
(straight-use-package '(nano-theme :type git :host github
                                   :repo "rougier/nano-theme"))

(load-theme 'dracula t)
;; (if (or (display-graphic-p) (daemonp))
;;     (load-theme 'dracula t))

;; install nix-mode
(use-package nix-mode
  :mode "\\.nix\\'"
  :init
  (add-hook 'nix-mode-hook (lambda () (setq-local compile-command "home-manager switch")))
  )

;; for nix formatting
(use-package format-all
  :init
  (add-hook 'nix-mode-hook 'format-all-mode)
  )

;; install yaml-mode
;; (use-package yaml-mode)

(use-package yaml)

(use-package yaml-pro)

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
  (evil-set-initial-state 'emms-playlist-mode 'emacs)
  )

(use-package rg
  :init
  (rg-enable-default-bindings))

(use-package helm-rg)

(use-package powershell)
(use-package koopa-mode)

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
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-minor-modes-off) ;; helps reduce modeline clutter
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  )

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

(setq org-agenda-span 14
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")

(use-package org-modern
  ;; :config
  ;; (with-eval-after-load 'org (global-org-modern-mode))
  )

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  )
(setq org-return-follows-link t)

;; :TODO: add more org-roam-capture-templates and figure out how they work correctly
(setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
            :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n")
            :unnarrowed t)
        )
)

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "inbox.org" "#+title: inbox\n#+filetags: :inbox:")))))

(defun my/org-roam-capture-meeting ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("m" "meeting" plain "* %<%Y-%m-%d> %?"
                                  :if-new (file+head "meetings.org" "#+title: meetings\n#+filetags: :meetings:")))))

(defun my/org-roam-capture-1-on-1 ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("o" "1-on-1" plain "* %<%Y-%m-%d> %?"
                                  :if-new (file+head "1-on-1.org" "#+title: 1-on-1\n#+filetags: :1-on-1:meetings:career:")))))

(defun my/org-roam-capture-hype-doc ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("h" "hype" plain "* %<%Y-%m-%d> %?"
                                  :if-new (file+head "hype-doc.org" "#+title: hype doc\n#+filetags: :hype:career:")))))

(setq my-org-roam-directory
      (cond
       ((eq my/config-machine 'work) "~\\OneDrive - Microsoft\\Desktop\\roam-notes")
       (t "~/roam-notes")
       ))

;; :TODO: learn how to use org-roam package for notes
;; :TODO: also look up org-drill
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename my-org-roam-directory))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n b" . my/org-roam-capture-inbox)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-make-toc
  :config
  (add-hook 'org-mode-hook #'org-make-toc-mode)
  (setq org-make-toc-insert-custom-ids t))

(use-package all-the-icons) ;; remember need to run (all-the-icons-install-fonts) to install the fonts

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package all-the-icons-completion
;;   :init
;;   (all-the-icons-completion-mode))

(use-package ranger
  :bind
  ("C-x t" . ranger)
  ;; ("<f9>" . ranger)
  (:map ranger-mode-map
	("C-<tab>" . ranger-next-tab))
  :config
  (setq ranger-cleanup-on-disable t)
  (setq ranger-cleanup-eagerly t))

(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (nyan-mode))

(use-package zone-nyan)

(use-package projectile
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-sort-order 'recently-active)
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

(defun my/goto-first-non-whitespace ()
  "Move the cursor to the first non-whitespace character on the current line."
  (interactive)
  (beginning-of-line)  ; Move to the beginning of the current line
  (skip-chars-forward " \t")  ; Skip forward over spaces and tabs
  )

(advice-add 'avy-goto-line :after #'my/goto-first-non-whitespace)

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package read-aloud
  :init
  (global-set-key (kbd "C-C r p") 'read-aloud-buf)
  (global-set-key (kbd "C-C r s") 'read-aloud-stop)
  (global-set-key (kbd "C-C r t") 'read-aloud-this))

(use-package company-box
  :hook (company-mode . company-box-mode))
(add-hook 'after-init-hook 'global-company-mode)
;; use company-search with C-s

(use-package page-break-lines) ;; needed for dashboard

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-center-content t)
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
  ;; reenable hook when the bug gets fixed...
  ;; :hook (emacs-startup . spacious-padding-mode) ;; prevents bug (where there's a white line between frames) by having it init at emacs-startup instead of after-init (which runs before emacs-startup)
  )

(defface svg-tag-todo-face
  '((t (:foreground "#282A36" :background "#FFB86C" :weight bold)))
  "A custom face for todo tags using svg-tag-mode."
  :group 'emacs)

(defface svg-tag-note-face
  '((t (:foreground "#282A36" :background "#8BE9FD" :weight bold)))
  "A custom face for note tags using svg-tag-mode."
  :group 'emacs)

;; :TODO: add more svg-tag tags
;; :NOTE: svg-tag-mode example
(use-package svg-tag-mode
  :init
  (setq svg-tag-tags
        '(
          (":TODO:" . ((lambda (tag) (svg-tag-make "TODO" :face 'svg-tag-todo-face))))
          (":NOTE:" . ((lambda (tag) (svg-tag-make "NOTE" :face 'svg-tag-note-face))))
          )
        )
  (add-hook 'prog-mode-hook 'svg-tag-mode)
  )

(use-package hl-todo)

(use-package flycheck-hl-todo
  :ensure t
  :defer 5 ; Need to be initialized after the rest of checkers
  :config
  (flycheck-hl-todo-setup))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package csv-mode)

(use-package beacon
  :init
  (beacon-mode 1))

(use-package selectric-mode) ;; haha this is the funniest package

(use-package harpoon)

(use-package chatgpt-shell
  :ensure t
  :config
  (setq chatgpt-shell-model-version 3)
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
        (auth-source-pass-get 'secret "OPENAI_API_KEY")))))

(use-package editorconfig) ;; dependency for copilot

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :bind (("C-M-\\" . copilot-accept-completion)))

;; :TODO: figure out how to configure these packages (dumb-jump, corfu, and orderless)
(use-package dumb-jump
  :straight (:host github :repo "dmille56/dumb-jump" :files ("dumb-jump.el"))
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package helm-xref)
(use-package helm-swoop)

(use-package corfu
  ;; :init
  ;; (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s) ;; Orderless field separator
  ) ;; dumb code completion

;; (use-package orderless
;;   :custom
;;   (completion-styles '(basic partial-completion orderless))
;;  )

(use-package consult)

(define-key transient-map (kbd "<f8>") 'transient-quit-all)
(define-key transient-map (kbd "<escape>") 'transient-quit-all)
 
;; enable tab-bar-mode
(tab-bar-mode)
(setq tab-bar-tab-hints t)

(evil-define-key 'normal eshell-mode-map
  (kbd "C-r") 'helm-eshell-history)

(evil-define-key 'insert eshell-mode-map
  (kbd "C-r") 'helm-eshell-history)

(defun eshell-my-cd (dir)
  "Change DIR in eshell."
  (eshell/cd dir)
  (eshell-send-input))

(defun eshell-go-up-one-dir ()
  "Go up one directory in eshell."
  (interactive)
  (eshell-my-cd "..")
)

(evil-define-key 'normal eshell-mode-map
  (kbd "C-l") 'eshell-go-up-one-dir)

(evil-define-key 'insert eshell-mode-map
  (kbd "C-l") 'eshell-go-up-one-dir)

(use-package eshell-syntax-highlighting
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-toggle
  :init
  (setq eshell-toggle-window-side 'right)
  (setq eshell-toggle-use-projectile-root t)
  ;; (setq eshell-toggle-run-command nil)
  :bind
  ("C-x C-z" . eshell-toggle)
  ("<f3>" . eshell-toggle)
  )

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "ff" "find-file-other-window $1")
            (eshell/alias "ffw" "find-file $1")
            (eshell/alias "fft" "find-file-other-tab $1")
            (eshell/alias "ffo" "find-file-other-frame $1")
            (eshell/alias "cls" "clear-scrollback")
            (eshell/alias "zi" "eshell-cd-with-zoxide $1")
            ))

;; Games

(use-package 2048-game)

(use-package sudoku
  :init
  (evil-set-initial-state 'sudoku-mode 'emacs)
  :config
  (define-key sudoku-mode-map (kbd "h") 'sudoku-move-point-left)
  (define-key sudoku-mode-map (kbd "H") 'sudoku-hint)
  )

(use-package pacmacs
  :init
  (evil-set-initial-state 'pacmacs-mode 'emacs)
  )

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package link-hint)

(use-package treesit-auto
  :custom
  (setq treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode)
  )

(require 'eglot)
;; Add-hooks for eglot
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'yaml-mode-hook 'eglot-ensure)
(add-hook 'nix-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(setq compilation-scroll-output 'first-error)

(use-package fireplace)

(use-package autotetris-mode)

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(evil-set-initial-state 'eat-mode 'emacs)

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package git-timemachine)
(use-package clippy)

(use-package command-log-mode)

(use-package smartparens-mode
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;; :TODO: figure out how to either integrate zoxide or eshell-z
(use-package zoxide)

(defun eshell-cd-with-zoxide (&optional query)
   (interactive)
   (zoxide-open-with query 'eshell-my-cd))

(use-package eshell-z)

(use-package ts-query-highlight
  :straight (:type git :host sourcehut :repo "meow_king/ts-query-highlight"))

(use-package treesit-jump
  :straight (:host github :repo "dmille56/treesit-jump" :files ("*.el" "treesit-queries"))
  ;; :load-path "~/Desktop/prog/treesit-jump/"
  :config
  (global-set-key (kbd "<f9>") 'treesit-jump-jump)
  (setq treesit-jump-queries-filter-list '("inner" "test" "param"))
)

;; (add-to-list 'treesit-jump-queries-extra-alist (cons 'powershell-ts-mode '("(flow_control_statement (_)) @flow")))
;; (add-to-list 'treesit-jump-queries-extra-alist (cons 'python-ts-mode '("(return_statement (_)) @return")))

(use-package powershell-ts-mode
  :straight (:host github :repo "dmille56/powershell-ts-mode")
  ;; :load-path "~/Desktop/prog/powershell-ts-mode/"
  :config
  ;; Associate .ps1 files with powershell-ts-mode
  (add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.psd1\\'" . powershell-ts-mode))
  (setq powershell-ts-enable-imenu-top-level-vars nil)
)

(use-package paredit)

(use-package ebuku)

(use-package ellama
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e"))

;; Set up zone-matrix
(straight-use-package
 '(zone-matrix :type git :host github :repo "dmille56/zone-matrix"))
 
(require 'zone-matrix)
(require 'zone-matrix-settings)
(require 'zone-settings)

(setq zone-programs [zone-nyan zone-matrix])

;; lsp-mode performance settings
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-completion-provider :capf)

;; disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq kill-buffer-delete-auto-save-files t) ;; prompt to delete auto save files when the buffer is killed

;; set auto save files to the temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; disable startup screen
(setq inhibit-startup-screen t)

(global-set-key [escape] 'keyboard-escape-quit)

(setq-local my/main-dir
      (cond
       ((eq my/config-machine 'pc) "~/dotfiles/emacs-config/")
       ((eq my/config-machine 'work) "~\\scratch\\dotfiles\\emacs-config\\")
       (t "~/dotfiles/emacs-config/")
       ))

(setq org-clock-sound (concat my/main-dir "timer.wav"))

(load-file
 (concat my/main-dir "twitchy.el"))

(global-set-key (kbd "M-p") 'twitchy-play-stream)

(load-file
 (concat my/main-dir "youtube.el"))

(global-set-key (kbd "M-y") 'youtube)

(load-file
 (concat my/main-dir "elfeed-config.el"))

(load-file
 (concat my/main-dir "eshell-extensions.el"))

(if (eq system-type 'gnu/linux)
    (load-file
     (concat my/main-dir "linux-specific.el")))

(if (eq system-type 'windows-nt)
    (load-file
     (concat my/main-dir "windows-specific.el")))

(provide 'main)
;;; main.el ends here
