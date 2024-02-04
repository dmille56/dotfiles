;;; powershell-ts-shell-mode --- Summary

;; Mode to run powershell via comint

;;; Commentary:

;; Useful references:
;; - https://www.masteringemacs.org/article/comint-writing-command-interpreter
;; - http://web.archive.org/web/20100123183703/http://Blogs.MSDN.com/dotnetinterop/archive/2008/04/10/run-powershell-as-a-shell-within-emacs.aspx
;; - https://www.emacswiki.org/emacs/PowerShell

;; Notes:
;; :TODO: add powershell shell support
;; :TODO: figure out why comint goes haywire when running powershell process

;;; Code:

(require 'comint)

(defvar powershell-ts-shell-file-path "pwsh"
  "Path to the program used by `run-powershell'.")

;; "-NoProfile"
;; "-Command" "-"
(defvar powershell-ts-shell-arguments '("-Command" "-")
  "Commandline arguments to pass to `powershell' process.")

(defvar powershell-ts-shell-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-powershell'.")

(defvar powershell-ts-shell-prompt-regex  "PS [^#$%>]+> "
  "Prompt for `run-powershell'.")

(defvar powershell-ts-shell-buffer-name "*powershell-ts-shell*"
  "Name of the buffer to use for the `run-powershell' comint instance.")

;;;###autoload
(defun powershell-ts-shell ()
  "Run an inferior instance of `powershell' inside Emacs."
  (interactive)
  (let* ((powershell-program powershell-ts-shell-file-path)
         (buffer (get-buffer-create powershell-ts-shell-buffer-name))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    ;; if the process is dead then re-create the process and reset the
    ;; mode.
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "powershell-ts-shell" buffer
               powershell-program nil powershell-ts-shell-arguments)
        (powershell-ts-shell-mode)))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (when buffer
      (pop-to-buffer buffer))))

(defun powershell-ts-shell--initialize ()
  "Helper function to initialize powershell."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode powershell-ts-shell-mode comint-mode "powershell-ts-shell"
  "Major mode for `run-powershell'.

\\<powershell-ts-shell-mode-map>"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp powershell-ts-shell-prompt-regex)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  ;; (set (make-local-variable 'font-lock-defaults) '(powershell-ts-shell-font-lock-keywords t)) ;; :TODO: look into adding font lock keywords
  (set (make-local-variable 'paragraph-start) powershell-ts-shell-prompt-regex))

(add-hook 'powershell-ts-shell-mode-hook 'powershell-ts-shell--initialize)

(provide 'powershell-ts-shell-mode)
;;; powershell-ts-shell-mode.el ends here
