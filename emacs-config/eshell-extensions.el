;;; eshell-extensions.el --- Summary

;;; Commentary:

;;; Code:

(require 'eshell-toggle)

(defun eshell-extensions-get-toggle-window (buf)
  "Open BUF in another window if not already visible, otherwise focus on it."
    (if (not buf)
        (message "No buffer %s" buf)
      (let ((win (get-buffer-window buf)))
        (if win
            (select-window win)
          (eshell-toggle)))))

(defun eshell-extensions-get-toggle-buffer ()
  "Get the eshell toggle buffer."
  (let ((buf-name (eshell-toggle--make-buffer-name)))
    (unless (get-buffer buf-name)
      (progn
        (eshell-toggle)
        (get-buffer buf-name)
        ))
    (get-buffer buf-name)))

(defun eshell-extensions-send-string-to-eshell (command)
  "Send a COMMAND string to an Eshell buffer and execute it."
  (interactive)
  (let ((eshell-buffer (eshell-extensions-get-toggle-buffer)))
    (eshell-extensions-get-toggle-window eshell-buffer)
    ;; Send the command to Eshell
    (with-current-buffer eshell-buffer
      (goto-char (point-max))              ; Move to the end of the Eshell buffer
      (insert command)                     ; Insert the command
      (eshell-send-input))))               ; Simulate pressing "return"

(defun eshell-extensions-get-cur-line ()
  "Return the content of the current line as a string."
  (interactive)
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun eshell-extensions-get-cur-region ()
  "Return the currently selected region as a string."
  (interactive)
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (error "No region selected")))

;; Usage example
;; (eshell-extensions-send-string-to-eshell "echo Hello, Eshell!")

;; (eshell-extensions-get-toggle-window (eshell-extensions-get-toggle-buffer))

;; (setq my-multi-line-string "
;; echo hello1
;; sleep 5
;; echo hello2
;; sleep 5
;; echo hello3")

;; (eshell-extensions-send-string-to-eshell my-multi-line-string)

(provide 'eshell-extensions)
;;; eshell-extensions.el ends here
