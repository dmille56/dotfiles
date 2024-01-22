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
  (let ((eshell-buffer (eshell-extensions-get-toggle-buffer)))
    (eshell-extensions-get-toggle-window eshell-buffer)
    ;; Send the command to Eshell
    (with-current-buffer eshell-buffer
      (goto-char (point-max))              ; Move to the end of the Eshell buffer
      (insert command)                     ; Insert the command
      (eshell-send-input))))               ; Simulate pressing "return"

;; Usage example
;; (eshell-extensions-send-string-to-eshell "echo Hello, Eshell!")

;; (eshell-extensions-get-toggle-window (eshell-extensions-get-toggle-buffer-2))


(provide 'eshell-extensions)
;;; eshell-extensions.el ends here
