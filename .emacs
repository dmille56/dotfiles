;; disable toolbar
(tool-bar-mode -1)

;; disable scrollbars
(scroll-bar-mode -1)

;; disable menubar
(menu-bar-mode -1)

; add evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; If you don't have MELPA in your package archives:
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

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

;; set keybindings for elscreen
(define-key evil-normal-state-map (kbd "s") 'elscreen-next)
(define-key evil-normal-state-map (kbd "a") 'elscreen-previous)

;; remap ; to : in evil
(with-eval-after-load 'evil-maps
   (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
     (define-key evil-motion-state-map (kbd ";") 'evil-ex))

;; disable backup files
(setq make-backup-files nil)

;; disable startup screen
(setq inhibit-startup-screen t)
