;;; eshell-extensions.el --- Summary

;;; Commentary:

;;; Code:

(require 'eshell-toggle)

(defvar eshell-extensions-toggle-buffer-regex "\*et:.*")

(defun find-buffer-regex (regex)
  "Find the first buffer whose name matches the given REGEX."
  (interactive "sBuffer name regex: ")
  (catch 'found
    (dolist (buf (buffer-list))
      (when (string-match regex (buffer-name buf))
        (throw 'found buf)))))

(defun eshell-extensions-get-toggle-buffer ()
  "Get the eshell toggle buffer."
  (find-buffer-regex eshell-extensions-toggle-buffer-regex))

(defun eshell-extensions-get-toggle-buffer-2 ()
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
    ;; Send the command to Eshell
    (with-current-buffer eshell-buffer
      (goto-char (point-max))              ; Move to the end of the Eshell buffer
      (insert command)                     ; Insert the command
      (eshell-send-input))))               ; Simulate pressing "return"

;; Usage example
;; (eshell-extensions-send-string-to-eshell "echo Hello, Eshell!")

;; (print (eshell-extensions-get-toggle-buffer-2))

(provide 'eshell-extensions)
;;; eshell-extensions.el ends here
