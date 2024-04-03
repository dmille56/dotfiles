;;; package --- Summary

;;; Commentary:
;;; Emacs config file :smile:👍💪

;;; Code:

(setq gc-cons-threshold 100000000)

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
(setq-default org-edit-src-content-indentation 0)

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
(unless package-archive-contents (package-refresh-contents))

;; install use-package
(package-install 'use-package)
(require 'use-package)
(setq use-package-always-ensure t)

;; run esup to benchmark startup code
(use-package esup :defer)

(use-package auto-package-update
  :defines (auto-package-update-delete-old-versions auto-package-update-hide-results)
  :functions (auto-package-update-maybe)
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

(defvar my/config-machine
      (cond
             ((string-equal (system-name) "van") 'pc)
             ((string-equal (system-name) "localhost") 'phone)
             ((string-equal (system-name) "LOCAL-D31D4TRU3") 'work)
             (t 'pc) ;; fall-back to pc
             ))

(use-package hydra :defer)
(use-package posframe :defer)

;; (use-package transient-posframe
;;   :config
;;   (transient-posframe-mode))

;; (use-package helm-posframe
;;   :config
;;   (helm-posframe-enable))

;; (use-package hydra-posframe
;; ;; https://github.com/Ladicle/hydra-posframe
;;   :straight (:host github :repo "ladicle/hydra-posframe" :files ("*.el"))
;;   :hook (after-init . hydra-posframe-mode))

(setq-default evil-want-keybinding nil)

(defmacro define-tab-management-transient ()
  "Define a macro for tab-management transient."
  `(transient-define-prefix my/transient-tab-management ()
     "Transient for tab management."
     [["Tabs"
       ("n" "new" tab-new "new")
       ("w" "switch" tab-bar-switch-to-tab "switch")]
      ["Switch"
       ,@(cl-loop for i from 1 to 9
                  collect (let ((key (number-to-string i))
                                (name (number-to-string i))
                                (doc (format "Select tab %d" i)))
                            `(,key ,name (lambda () (interactive) (tab-bar-select-tab ,i)) ,doc)))
       ("0" "0" (lambda () (interactive) (tab-bar-select-tab 10)) "Select tab 10")]
      ["Move"
       ,@(cl-loop for i from 1 to 9
                  collect (let ((key (format "m%d" i))
                                (name (format "move %d" i))
                                (doc (format "Move tab to %d" i)))
                            `(,key ,name (lambda () (interactive) (tab-bar-move-tab-to ,i)) ,doc)))
       ("m0" "move 10" (lambda () (interactive) (tab-bar-move-tab-to 10)) "Move tab to 10")]]))

(define-tab-management-transient)

(transient-define-prefix my/transient-leader-misc ()
  "Transient for leader misc."
  [["Misc"
    ("s" "rg search" rg-menu)
    ("b" "bookmarks" helm-bookmarks)
    ("B" "set bookmark" bookmark-set)
    ("T" "timers" my/transient-org-timer)
    ("w" "elfeed" elfeed)
    ("r" "recent" helm-recentf)
    ("R" "query replace regex" query-replace-regexp)
    ("a" "agenda" org-agenda)
    ("j" "treesit-jump" treesit-jump-transient)
    ("l" "git-link" my/transient-git-link)
    ("Ll" "leetcode" leetcode)
    ("Ld" "leetcode daily" leetcode-daily)
    ("e" "revert buffer" revert-buffer)]
   ["Ai"
    ("g" "chatgpt" chatgpt-shell)
    ("Gr" "chatgpt send and review" chatgpt-shell-send-and-review-region)
    ("GR" "chatgpt refactor code" chatgpt-shell-refactor-code)
    ("Gd" "chatgpt describe code" chatgpt-shell-describe-code)
    ("Gc" "chatgpt write git commit" chatgpt-shell-write-git-commit)
    ]
   ["Help"
    ("h" "emacs help" help-for-help)
    ("." "clippy func" clippy-describe-function)
    (">" "clippy var" clippy-describe-variable)
    ]
   ["Random"
    ("p" "game" my/transient-game)
    ("P" "padding toggle" spacious-padding-mode)
    ("S" "typewriter toggle" selectric-mode)
    ("c" "calculator" calc)
    ("t" "ranger" ranger)
    ("d" "ddg search" ddg)
    ("D" "dired" dired)
    ("y" "tree" treemacs)
    ]
   ])

(transient-define-prefix my/transient-git-link ()
  "Transient for git-link."
  ["Git-link"
   ("g" "link" git-link)
   ("c" "commit" git-link-commit)
   ("h" "homepage" git-link-homepage)])

(transient-define-prefix my/transient-game ()
  "Transient for selecting a game."
  ["Game"
   ("2" "2048" 2048-game)
   ("t" "tetris" tetris)
   ("d" "doctor" doctor)
   ("s" "sudoku" sudoku)
   ("p" "pacmacs" pacmacs-start)])

(transient-define-prefix my/transient-elisp-mode ()
  "Transient for elisp mode."
  [["Eval"
    ("r" "eval region" eval-region)
    ("B" "eval buffer" eval-buffer)
    ("d" "eval defun "eval-defun)
    ("e" "eval last sexp" eval-last-sexp)]
   ["Load"
    ("l" "load cur file" (lambda () (interactive) (load-file (buffer-file-name (window-buffer)))))
    ("L" "load file" load-file)]
   ["Edit"
    ("D" "edebug func" edebug-defun)
    ("s" "slurp" paredit-forward-slurp-sexp)
    ("b" "barf" paredit-forward-barf-sexp)]])

(transient-define-prefix my/transient-doc-view-mode ()
  "Transient for doc-view mode."
  ["Doc View"
   ("g" "go to page" doc-view-goto-page)
   ("n" "next page" doc-view-next-page)
   ("p" "prev page" doc-view-previous-page)])

(transient-define-prefix my/transient-org-mode ()
  "Transient for org mode."
  ["Org"
   ("o" "open" org-open-at-point)
   ("l" "insert link" org-insert-link)
   ("j" "goto heading" avy-org-goto-heading-timer)
   ("r" "avy refile child" avy-org-refile-as-child)
   ("c" "insert toc" org-make-toc-insert)
   ("C" "edit toc" org-make-toc-set)
   ("s" "schedule" org-schedule)
   ("S" "structure template" org-insert-structure-template)
   ("t" "todo" org-todo)
   ("T" "set tags" org-set-tags-command)
   ("e" "org export" org-export-dispatch)
   ("b" "checkbox" org-toggle-checkbox)])

(transient-define-prefix my/transient-python-mode ()
  "Transient for python mode."
  ["Python"
   ("f" "run file" python-shell-send-file)
   ("b" "run buffer" python-shell-send-buffer)
   ("l" "run statement" python-shell-send-statement)
   ("p" "start python shell" run-python)
   ("r" "run region" python-shell-send-region)
   ("z" "switch to shell" python-shell-switch-to-shell)])

(transient-define-prefix my/transient-org-timer ()
  "Transient for org-timer."
  ["Timers"
   ("t" "set timer" org-timer-set-timer)
   ("s" "stop" org-timer-stop)
   ("r" "start" org-timer-start)
   ("p" "pause/continue" org-timer-pause-or-continue)])

(transient-define-prefix my/transient-shell-run ()
  "Transient for running shell commands."
  ["Shell Run"
   ("R" "run region" shell-command-on-region)
   ("x" "async run command" async-shell-command)
   ("X" "run command" shell-command)
   ("l" "run cur line in eshell" eshell-extensions-send-cur-line-to-eshell)
   ("r" "run region in eshell" eshell-extensions-send-cur-region-to-eshell)
   ("e" "run command in eshell" eshell-extensions-send-string-to-eshell)])

(transient-define-prefix my/transient-emms ()
  "Transient for emms."
  ["Emms"
   ("e" "emms" emms)
   ("s" "emms streams" emms-streams)])

(transient-define-prefix my/transient-evil-macros ()
  "Transient for evil macros."
  ["Evil macros"
   ("r" "record macro" evil-record-macro) ;; traditionally q in vim/evil
   ("x" "execute macro" evil-execute-macro) ;; also @ followed by macro register in vim/evil
   ("l" "execute last macro" evil-execute-last-recorded-macro)]) ;; also @@ in vim/evil

(transient-define-prefix my/transient-hl-todo ()
  "Transient for hl-todo."
  ["Hl-Todo"
   ("p" "previous" hl-todo-previous)
   ("n" "next" hl-todo-next)
   ("o" "occur" hl-todo-occur)
   ("i" "insert todo" hl-todo-insert)
   ("r" "rgrep" hl-todo-rgrep)])

(transient-define-prefix my/transient-org-roam ()
  "Transient for org-roam."
  [["Org-Roam"
    ("l" "buf toggle" org-roam-buffer-toggle)
    ("f" "find" org-roam-node-find)
    ("g" "graph" org-roam-graph)
    ("i" "node insert" org-roam-node-insert)]
   ["Capture"
    ("c" "capture" org-roam-capture)
    ("b" "capture inbox" my/org-roam-capture-inbox)
    ("1" "capture 1-on-1" my/org-roam-capture-1-on-1)
    ("m" "capture meeting" my/org-roam-capture-meeting)
    ("h" "capture hype doc" my/org-roam-capture-hype-doc)
    ("j" "capture today" org-roam-dailies-capture-today)]])

(transient-define-prefix my/transient-forge ()
  "Transient for magit forge."
  ["Forge"
   ("y" "pull" forge-pull)
   ("f" "fork" forge-fork)
   ("n" "pull notifications" forge-pull-notifications)
   ("N" "list notifications" forge-list-notifications)
   ("cp" "create pull request" forge-create-pullreq)
   ("cP" "create post" forge-create-post)
   ("ci" "create issue" forge-create-issue)
   ("cI" "create pull request from issue" forge-create-pullreq-from-issue)
   ("b" "browse" forge-browse)])

(use-package evil-leader
  :defines (evil-leader/in-all-states)
  :functions (evil-leader/set-leader evil-leader/set-key evil-leader/set-key-for-mode global-evil-leader-mode)
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
   "E" 'my/transient-shell-run
   "c" 'compile
   "C" 'recompile
   "x" 'flycheck-list-errors
   "X" 'my/transient-hl-todo
   ;; "l" 'run-lsp-command-map
   "g" 'magit
   "F" 'my/transient-forge
   "v" 'helm-semantic-or-imenu
   "a" 'link-hint-open-link
   "M" 'my/transient-emms
   "m" 'my/transient-evil-macros
   "O" 'my/transient-org-roam

   ;; window management
   "o" 'other-window
   "1" 'delete-other-windows
   "2" 'split-window-below
   "3" 'split-window-right

   ;; tab management
   "t" 'my/transient-tab-management
   "T" 'eshell-toggle

   ;; harpoon
   "h" 'harpoon-toggle-quick-menu
   "H a" 'harpoon-quick-menu-hydra
   "H <return>" 'harpoon-add-file

   ;; u submenu
   "u" 'my/transient-leader-misc

   ;; leave r for mode specific keymaps
   )
;;  (evil-leader/set-key
;;    "L" '(lambda () (interactive) (which-key-show-keymap 'lsp-command-map))) ;; :TODO: remove this when get lsp-command-map working
  ;; (evil-define-minor-mode-key 'normal lsp-mode (kbd "SPC l") lsp-command-map)
  ;; (evil-leader/set-key-for-mode 'lsp-mode "L" 'lsp-command-map)
  (evil-leader/set-key-for-mode 'lsp-mode "l" 'lsp-command-map)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "r" 'my/transient-elisp-mode)
  (evil-leader/set-key-for-mode 'lisp-interaction-mode "r" 'my/transient-elisp-mode)
  (evil-leader/set-key-for-mode 'doc-view-mode "r" 'my/transient-doc-view-mode)
  (evil-leader/set-key-for-mode 'org-mode "r" 'my/transient-org-mode)
  (evil-leader/set-key-for-mode 'python-mode "r" 'my/transient-python-mode)
  (evil-leader/set-key-for-mode 'python-ts-mode "r" 'my/transient-python-mode)
  (global-evil-leader-mode))

(defun run-lsp-command-map ()
  "Show lsp-command-map."
  (interactive)
  (which-key-show-keymap 'lsp-command-map))

;; note: "c-z" to toggle to/from emacs state
(use-package evil
  :functions (evil-define-key evil-mode evil-set-initial-state)
  :defines (evil-normal-state-map evil-visual-state-map evil-motion-state-map)
  ;; :init
  ;; (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  ;; (setq evil-want-keybinding nil)
  ;; (setq evil-search-module 'evil-search)
  :init
  (evil-mode 1))

(use-package evil-snipe
  :functions (evil-snipe-mode evil-snipe-override-mode)
  :after evil
  :init
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-collection
  :functions (evil-collection-init)
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
  :functions global-evil-surround-mode
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-tutor :defer)

;; :TODO: figure out how to use this package effectively
(use-package evil-textobj-tree-sitter
  :functions evil-textobj-tree-sitter-get-textobj
  :defines evil-outer-text-objects-map evil-inner-text-objects-map
  :after treesit)

;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
(with-eval-after-load 'treesit
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner")))

  ;; You can also bind multiple items and we will match the first one we can find
;;  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
;;  (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner"))))

;; Set default font
(unless (eq my/config-machine 'phone)
  (set-frame-font "DejaVu Sans Mono for Powerline-13" nil t))
;; (semantic-mode 1) ;; use semantic :TODO: re-enable when figure out why it kept throwing error

;; ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(with-eval-after-load 'dired
  (defvar dired-mode-map)
  (define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)
  (evil-define-key 'normal dired-mode-map ";" 'evil-ex))

;; install flycheck
(use-package flycheck
  :functions global-flycheck-mode flycheck-add-next-checker
  :defines (flycheck-add-next-checker)
  :init
  (global-flycheck-mode))

(use-package treemacs
  :defer
  :bind
  ("C-x y" . treemacs))

(use-package treemacs-projectile :defer)
(use-package treemacs-evil :defer)

(use-package haskell-mode
  :defer
  :defines (haskell-mode-map))

;; Install Dante
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :defines (dante-mode-map)
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
   #'(lambda () (flycheck-add-next-checker 'haskell-dante
					  '(warning . haskell-hlint))))

;; add attrap - for Dante mode
(use-package attrap
  :after dante
  :ensure t
  :bind (("C-x /" . attrap-attrap)))

(use-package reformatter :after ormolu) ;; needed for ormolu

;; Add auto formatting for Haskell code via Ormolu (requires ormolu exe to be installed)
(use-package ormolu
 :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
       ("C-c r" . ormolu-format-buffer)))

;; install lsp-mode
;; set prefix for lsp-command-keymap (few alternatives - "s-l", "C-l", "C-c l")

(use-package lsp-mode
  :defer
  :defines lsp-keymap-prefix lsp-completion-provider lsp-disabled-clients lsp-log-io lsp-mode-map
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-completion-provider :capf)
  (setq lsp-disabled-clients '(omnisharp))
  ;; lsp-mode performance settings
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :hook (;; replace X-mode with concrete major-mode(e. g. python-mode)
         ;; (haskell-mode . lsp)
         ;; (powershell-mode . lsp)
         ;; (csharp-mode . lsp)
         ;; (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp)

(use-package lsp-haskell
 :defines lsp-haskell-process-path-hie
 :defer
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 (setq lsp-log-io t)
 (message "Loaded lsp-haskell")
)

;; lsp-mode optional add ons
(use-package lsp-ui
  :defines lsp-ui-doc-enable
  :commands lsp-ui-mode
  :bind
  (:map lsp-mode-map
	("C-c ." . lsp-ui-doc-glance))
  :init
  (evil-define-key 'normal lsp-mode-map
    (kbd "C-c .") 'lsp-ui-doc-glance)
  :config
  (setq lsp-ui-doc-enable nil))

(use-package helm-lsp :commands helm-lsp-workspace-symbol :defer)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list :defer)
(use-package yasnippet :defer)

;; For C#
(use-package csproj-mode :defer)
(add-to-list 'auto-mode-alist '("\\.*axaml\\'" . xml-mode))

(use-package which-key
  :functions (which-key-show-keymap which-key-mode)
  :defer
  :init
  (which-key-mode))

;; use f5 to move to the next tab
(global-set-key (kbd "<f5>") 'tab-bar-switch-to-next-tab)
;; use f6 to move to the next window
(global-set-key (kbd "<f6>") 'other-window)

;; install maggit
(use-package magit
  :defines forge-add-default-bindings magit-pull-or-fetch
  :defer
  :init
  (setq magit-pull-or-fetch t)
  (setq forge-add-default-bindings t))

(use-package forge :after magit)

(use-package git-link
  :defines git-link-open-in-browser
  :defer
  :init
  (setq git-link-open-in-browser t))

;; Set the files that are searched for writing tokens
;; by default ~/.authinfo will be used
(setq auth-sources '("~/.authinfo.gpg"))

;; install markdown-mode and set it to use pandoc
;; make sure you have pandoc installed!
(use-package markdown-mode
  :defines markdown-command
  :defer
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "pandoc"))

;; install helm
(use-package helm
  :defines helm-map
  :functions (helm-M-x helm-mode)
  :defer
  :bind
  ("<f4>" . helm-occur)
  ("<f7>" . helm-find-files)
  ("C-x c i" . helm-semantic-or-imenu)
  :init
  (global-set-key (kbd "M-x") #'helm-M-x)
  :config
  (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
  (define-key helm-map (kbd "<f8>") 'helm-keyboard-quit)
  (helm-mode 1)
  )

;; install themes
;; :TODO: look into installing and using circadian.el (to switch between light/dark at sunset)
;; (use-package sublime-themes)
;; (use-package spacemacs-theme)
(use-package dracula-theme)
;; (straight-use-package '(nano-theme :type git :host github :repo "rougier/nano-theme"))

(load-theme 'dracula t)
;; (if (or (display-graphic-p) (daemonp))
;;     (load-theme 'dracula t))

;; install nix-mode
(use-package nix-mode :mode "\\.nix\\'" :defer)

;; for nix formatting
(use-package format-all
  :defer
  :init
  (add-hook 'nix-mode-hook 'format-all-mode)
  )

;; install yaml-mode
;; (use-package yaml-mode)

(use-package yaml :defer)

(use-package yaml-pro :defer)

;; install/setup emms
(use-package emms
  :defines (emms-playlist-mode-map emms-player-list emms-source-file-default-directory emms-stream-default-action)
  :functions (emms-all emms-default-players)
  :defer
  :bind
  ("C-x e" . emms)
  ("C-x s" . emms-streams)
  (:map emms-playlist-mode-map
   ("j" . next-line)
   ("k" . previous-line))
  :config
  (emms-all)
  (if (eq system-type 'gnu/linux)
      (setq emms-player-list '(emms-player-mpv))
    (emms-default-players))
  (setq emms-source-file-default-directory "~/Music/")
  (setq emms-stream-default-action "play")
  (evil-set-initial-state 'emms-playlist-mode 'emacs))

(use-package rg
  :functions rg-enable-default-bindings
  :config
  (rg-enable-default-bindings))

(use-package helm-rg :defer)

(use-package powershell :defer)
(use-package koopa-mode :defer)

(use-package erc :defer)

(use-package md4rd
  :defines (md4rd-mode-map md4rd-subs-active)
  :defer
  :bind (
	 :map md4rd-mode-map
	  ("q" . kill-this-buffer)
	  ("j" . next-line)
	  ("k" . previous-line))
  :config
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
  :defines spaceline-highlight-face-func
  :functions (spaceline-emacs-theme spaceline-helm-mode spaceline-toggle-minor-modes-off)
  :defer
  :init
  (spaceline-emacs-theme)
  :config
  (spaceline-helm-mode)
  (spaceline-toggle-minor-modes-off) ;; helps reduce modeline clutter
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

(use-package evil-org
  :functions (evil-org-agenda-set-keys evil-org-set-key-theme)
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (setq-default org-startup-folded "overview"))

(global-set-key "\C-xa" 'org-agenda)
(global-set-key "\M-x" 'evil-ex)

(setq-default org-agenda-span 14
              org-agenda-start-on-weekday nil
              org-agenda-start-day "-3d")

;; (use-package org-modern
;;   ;; :config
;;   ;; (with-eval-after-load 'org (global-org-modern-mode))
;;   )

(use-package org-present)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  )
(setq-default org-return-follows-link t)

(setq-default org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
            :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n")
            :unnarrowed t)
        )
)

(defun my/org-roam-capture-inbox ()
  "Org roam capture inbox."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "inbox.org" "#+title: inbox\n#+filetags: :inbox:")))))

(defun my/org-roam-capture-meeting ()
  "Org roam capture meeting."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("m" "meeting" plain "* %<%Y-%m-%d> %?"
                                  :if-new (file+head "meetings.org" "#+title: meetings\n#+filetags: :meetings:")))))

(defun my/org-roam-capture-1-on-1 ()
  "Org roam capture 1-on-1 meeting."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("o" "1-on-1" plain "* %<%Y-%m-%d> %?"
                                  :if-new (file+head "1-on-1.org" "#+title: 1-on-1\n#+filetags: :1-on-1:meetings:career:")))))

(defun my/org-roam-capture-hype-doc ()
  "Org roam capture for hype doc."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("h" "hype" plain "* %<%Y-%m-%d> %?"
                                  :if-new (file+head "hype-doc.org" "#+title: hype doc\n#+filetags: :hype:career:")))))

(defvar my-org-roam-directory
      (cond
       ((eq my/config-machine 'work) "~\\OneDrive - Microsoft\\Desktop\\roam-notes")
       ((eq my/config-machine 'phone) "/data/data/com.termux/files/home/storage/shared/roam-notes")
       (t "~/roam-notes")
       ))

(use-package org-roam
  :defines org-roam-capture-templates org-roam-node-display-template
  :functions (org-roam-node-create org-roam-capture- org-roam-db-autosync-mode)
  :defer
  :custom
  (org-roam-directory my-org-roam-directory)
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
  ;; :TODO: look into org-roam-protocol at some point
  ;; (require 'org-roam-protocol)
  (org-roam-db-autosync-mode))

;; :TODO: learn how to use this package (org-drill)
(use-package org-drill :defer)

(use-package org-make-toc
  :defines org-make-toc-insert-custom-ids
  :functions org-make-toc-mode
  :after org
  :config
  (add-hook 'org-mode-hook #'org-make-toc-mode)
  (setq org-make-toc-insert-custom-ids t))

(use-package all-the-icons) ;; remember need to run (all-the-icons-install-fonts) to install the fonts

(use-package all-the-icons-dired
  :defer
  :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package all-the-icons-completion
;;   :init
;;   (all-the-icons-completion-mode))

(use-package ranger
  :defines ranger-mode-map ranger-cleanup-on-disable ranger-cleanup-eagerly
  :defer
  :bind
  ("C-x t" . ranger)
  ;; ("<f9>" . ranger)
  (:map ranger-mode-map
	("C-<tab>" . ranger-next-tab))
  :config
  (setq ranger-cleanup-on-disable t)
  (setq ranger-cleanup-eagerly t))

(use-package nyan-mode
  :defines nyan-animate-nyancat nyan-wavy-trail
  :functions nyan-mode
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (nyan-mode))

(use-package zone-nyan :defer)

(use-package projectile
  :defines projectile-mode-map projectile-switch-project-action projectile-sort-order
  :functions projectile-mode
  :defer
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-sort-order 'recently-active)
  (projectile-mode +1))

(use-package helm-projectile
  :functions helm-projectile-on
  :after projectile
  :config
  (helm-projectile-on))

(use-package cheat-sh
  :ensure t
  :bind (("C-c ?" . cheat-sh)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package restart-emacs :defer)

(use-package avy
  :defines avy-background
  :ensure t
  :init
  (setq avy-background t) ;; set background to black and grey when using avy
  (global-set-key (kbd "C-:") 'avy-goto-line)
  (global-set-key (kbd "C-;") 'avy-goto-char))

(defun my/goto-first-non-whitespace ()
  "Move the cursor to the first non-whitespace character on the current line."
  (interactive)
  (beginning-of-line)  ; Move to the beginning of the current line
  (skip-chars-forward " \t"))  ; Skip forward over spaces and tabs

(advice-add 'avy-goto-line :after #'my/goto-first-non-whitespace)

(use-package diff-hl
  :functions global-diff-hl-mode
  :init
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package read-aloud
  :defer
  :init
  (global-set-key (kbd "C-C r p") 'read-aloud-buf)
  (global-set-key (kbd "C-C r s") 'read-aloud-stop)
  (global-set-key (kbd "C-C r t") 'read-aloud-this))

(use-package company)

(use-package company-box
  :hook (company-mode . company-box-mode))
(add-hook 'after-init-hook 'global-company-mode)
;; use company-search with C-s

(use-package page-break-lines) ;; needed for dashboard

(use-package dashboard
  :defines dashboard-projects-backend dashboard-center-content dashboard-items
  :functions dashboard-setup-startup-hook
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5))))

;; (use-package dashboard-hackernews)

(use-package golden-ratio
  :functions golden-ratio-mode
  :init
  (golden-ratio-mode 1))

;; (use-package solaire-mode
;;   :init
;;   (solaire-global-mode))

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

(use-package svg-tag-mode
  :defines svg-tag-tags
  :init
  (setq svg-tag-tags
        '(
          (":TODO:" . ((lambda (tag) (svg-tag-make "TODO" :face 'svg-tag-todo-face))))
          (":NOTE:" . ((lambda (tag) (svg-tag-make "NOTE" :face 'svg-tag-note-face))))
          )
        )
  (add-hook 'prog-mode-hook 'svg-tag-mode))

(use-package hl-todo)

(use-package flycheck-hl-todo
  :functions flycheck-hl-todo-setup
  :ensure t
  :defer 5 ; Need to be initialized after the rest of checkers
  :config
  (flycheck-hl-todo-setup))

;; (use-package magit-todos
;;   :after magit
;;   :config (magit-todos-mode 1))

(use-package csv-mode :defer)

(unless (eq my/config-machine 'work)
  (use-package beacon
    :functions beacon-mode
    :init
    (beacon-mode 1)))

(if (eq my/config-machine 'work)
    (global-hl-line-mode))

(use-package selectric-mode) ;; haha this is the funniest package

(use-package harpoon)

(use-package pcsv :defer)

(use-package chatgpt-shell
  :defines chatgpt-shell-model-version
  :defer
  :ensure t
  :config
  (setq chatgpt-shell-model-version "gpt-3.5-turbo-16k-0613")
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
        (auth-source-pass-get 'secret "OPENAI_API_KEY")))))

;; :TODO: fix this function
(defun my/ollama-write-git-commit ()
  "Generate a git commit using ollama.
Make sure to run \='ollama serve\=' and have zephyr model."
  (interactive)
  (let* ((diff (shell-command-to-string "git diff --cached --color=never"))
         (prompt-header "Please help me write a git commit message (limit to 50 characters) for the following commit:")
         (str-to-send (concat prompt-header "\n\n" diff))
         (ollama-cmd (concat "ollama run zephyr " (shell-quote-argument str-to-send))))
    (message ollama-cmd)
    (if diff (insert (shell-command-to-string ollama-cmd)))))

(use-package editorconfig) ;; dependency for copilot

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :bind (("C-M-\\" . copilot-accept-completion)))

(use-package dumb-jump
  :straight (:host github :repo "dmille56/dumb-jump" :files ("dumb-jump.el"))
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package helm-xref :defer)
(use-package helm-swoop :defer)

;; :TODO: figure out how to use corfu and orderless
(use-package corfu
  ;; :init
  ;; (global-corfu-mode)
  :defer
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s) ;; Orderless field separator
  ) ;; dumb code completion

;; (use-package orderless
;;   :custom
;;   (completion-styles '(basic partial-completion orderless))
;;  )

(use-package consult :defer)

(with-eval-after-load 'transient
  (defvar transient-map)
  (define-key transient-map (kbd "<f8>") 'transient-quit-all)
  (define-key transient-map (kbd "<escape>") 'transient-quit-all))
 
;; enable tab-bar-mode
(tab-bar-mode)
(setq tab-bar-tab-hints t)

;; To repress eshell functions/varabiles not being found errors
(defvar eshell-mode-map)
(declare-function eshell/cd "esh-cmd")
(declare-function eshell-send-input "esh-cmd")
(declare-function eshell/alias "esh-cmd")

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
  :functions eshell-syntax-highlighting-global-mode
  :after eshell
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-toggle
  :defines eshell-toggle-window-side eshell-toggle-use-projectile-root
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

(use-package 2048-game :defer)

(use-package sudoku
  :defines sudoku-mode-map
  :defer
  :init
  (evil-set-initial-state 'sudoku-mode 'emacs)
  :config
  (define-key sudoku-mode-map (kbd "h") 'sudoku-move-point-left)
  (define-key sudoku-mode-map (kbd "H") 'sudoku-hint)
  )

(use-package pacmacs
  :defer
  :init
  (evil-set-initial-state 'pacmacs-mode 'emacs)
  )

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package link-hint :defer)

(use-package treesit-auto
  :functions global-treesit-auto-mode
  :custom
  (setq treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode)
  )

;; Add-hooks for eglot
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'yaml-mode-hook 'eglot-ensure)
(add-hook 'nix-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (defvar eglot-server-programs)
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp"))))

(use-package flycheck-eglot
  :functions global-flycheck-eglot-mode
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(setq-default compilation-scroll-output 'first-error)

(use-package fireplace :defer)

(use-package autotetris-mode :defer)

(use-package eat
  :defer
  :straight (:type git :host codeberg :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

(evil-set-initial-state 'eat-mode 'emacs)

(use-package perspective
  :functions persp-mode
  :defer
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package clippy :defer)

(use-package command-log-mode :defer)

(use-package smartparens-mode
  :defer
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;; :TODO: figure out how to get zoxide to work better with eshell
(use-package zoxide
  :defer
  :functions zoxide-open-with)

(defun eshell-cd-with-zoxide (&optional query)
  "Eshell cd using zoxide and a QUERY."
  (interactive)
  (zoxide-open-with query 'eshell-my-cd))

(use-package eshell-z :after eshell)

(use-package ts-query-highlight
  :defer
  :straight (:type git :host sourcehut :repo "meow_king/ts-query-highlight"))

;; My packages... Define them twice to allow loading local version if available first...
(defvar treesit-jump-path "~/Desktop/prog/treesit-jump/")
(defvar powershell-ts-mode-path "~/Desktop/prog/powershell-ts-mode/")

(use-package treesit-jump
  :after treesit
  :load-path treesit-jump-path
  :if (file-directory-p treesit-jump-path))

(use-package treesit-jump
  :after treesit
  :straight (:host github :repo "dmille56/treesit-jump" :files ("*.el" "treesit-queries"))
  :if (not (file-directory-p treesit-jump-path)))

;;(global-set-key (kbd "<f9>") 'treesit-jump-jump)
(global-set-key (kbd "<f9>") 'treesit-jump-transient)
(setq-default treesit-jump-queries-filter-list '("inner" "test" "param"))
;; (add-to-list 'treesit-jump-queries-extra-alist (cons 'powershell-ts-mode '("(flow_control_statement (_)) @flow")))
;; (add-to-list 'treesit-jump-queries-extra-alist (cons 'python-ts-mode '("(return_statement (_)) @return")))

(use-package powershell-ts-mode
  :load-path powershell-ts-mode-path
  :if (file-directory-p powershell-ts-mode-path))

(use-package powershell-ts-mode
  :straight (:host github :repo "dmille56/powershell-ts-mode")
  :if (not (file-directory-p powershell-ts-mode-path)))

(setq-default powershell-ts-enable-imenu-top-level-vars nil)

(use-package imenu-list
  :functions imenu-list-smart-toggle
  :defer
  :config
  (global-set-key (kbd "C-'") #'imenu-list-smart-toggle))

(use-package paredit :after elisp-mode)

(use-package ebuku :defer)

(use-package ellama
  :defer
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e"))

(use-package package-lint)

(use-package atomic-chrome
  :demand t
  :straight (atomic-chrome
             :repo "KarimAziev/atomic-chrome"
             :type git
             :flavor nil
             :host github)
  :commands (atomic-chrome-start-server)
  :config (atomic-chrome-start-server))

(use-package leetcode
  :defines leetcode-prefer-language
  :defer
  :config
  (setq leetcode-prefer-language "python3"))

(use-package zone-matrix
  :defer
  :straight (:host github :repo "dmille56/zone-matrix"))

(setq-default zone-programs [zone-nyan zone-matrix])

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

(defvar my/main-dir
      (cond
       ((eq my/config-machine 'pc) "~/dotfiles/emacs-config/")
       ((eq my/config-machine 'work) "~\\scratch\\dotfiles\\emacs-config\\")
       (t "~/dotfiles/emacs-config/")
       ))

(setq-default org-clock-sound (concat my/main-dir "timer.wav"))

;; (load-file
;;  (concat my/main-dir "twitchy.el"))
;; 
;; (global-set-key (kbd "M-p") 'twitchy-play-stream)

;; (load-file
;;  (concat my/main-dir "youtube.el"))

;; (global-set-key (kbd "M-y") 'youtube)

(defvar elfeed-config-path (concat my/main-dir "elfeed-config.el"))
(defvar eshell-extensions-path (concat my/main-dir "eshell-extensions.el"))

(global-set-key (kbd "C-x w") 'elfeed)
(autoload 'elfeed elfeed-config-path nil t)

(autoload 'eshell-extensions-send-string-to-eshell eshell-extensions-path nil t)
(autoload 'eshell-extensions-send-cur-line-to-eshell eshell-extensions-path nil t)
(autoload 'eshell-extensions-send-cur-region-to-eshell eshell-extensions-path nil t)

(if (eq system-type 'gnu/linux)
    (load-file
     (concat my/main-dir "linux-specific.el")))

(if (eq system-type 'windows-nt)
    (load-file
     (concat my/main-dir "windows-specific.el")))

(provide 'main)
;;; main.el ends here
