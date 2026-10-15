;;; early-init.el --- Package activation policy -*- lexical-binding: t; -*-

;; Install as ~/.emacs.d/early-init.el, before Emacs loads ~/.emacs.
;; Existing package.el installations stay on disk, but are not activated.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
