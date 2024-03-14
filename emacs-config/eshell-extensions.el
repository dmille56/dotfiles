;;; eshell-extensions.el --- Summary

;;; Commentary:
;;; Version: 0.10.0
;;; Package-Requires: ((eshell-toggle "0.10.0"))

;;; Code:

(require 'eshell-toggle)

(defgroup eshell-extensions nil
  "Customize group for eshell-extensions.el"
  :group 'emacs)

(defcustom eshell-extensions-string-process-function
  'eshell-extensions-strip-comments-from-str
  "Function to process strings with before sending to eshell."
  :type 'function
  :group 'eshell-extensions)

(defcustom eshell-extensions-comment-strs
  '("#" "//" "--" ";;")
  "List of comment starter strings."
  :type '(repeat string)
  :group 'eshell-extensions)

(defun eshell-extensions-strip-comments-from-str (str)
  "Strip comment delimiters and leading whitespace from each line in STR.
Can be in a single or multi-line string.  Retains the content after the comment characters."
  (let* ((comment-regex (concat "^[[:space:]]*\\("
                                (mapconcat (lambda (delim)
                                             (regexp-quote delim))
                                           eshell-extensions-comment-strs
                                           "\\|")
                                "\\)[[:space:]]*"))
         (lines (split-string str "\n")))
    (mapconcat (lambda (line)
                 (if (string-match comment-regex line)
                     (substring line (match-end 0))
                   line))
               lines "\n")))

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

;;;###autoload
(defun eshell-extensions-send-string-to-eshell (command)
  "Send a COMMAND string to an Eshell buffer and execute it."
  (interactive "sEnter a command to run in eshell: ")
  (let (
        (eshell-buffer (eshell-extensions-get-toggle-buffer))
        (processed-command (funcall eshell-extensions-string-process-function command))
         )
    (eshell-extensions-get-toggle-window eshell-buffer)
    ;; Send the command to Eshell
    (with-current-buffer eshell-buffer
      (goto-char (point-max))              ; Move to the end of the Eshell buffer
      (insert processed-command)                     ; Insert the command
      (eshell-send-input))))               ; Simulate pressing "return"

(defun eshell-extensions-get-cur-line ()
  "Return the content of the current line as a string."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun eshell-extensions-get-cur-region ()
  "Return the currently selected region as a string."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (error "No region selected")))

;;;###autoload
(defun eshell-extensions-send-cur-line-to-eshell ()
  "Send the current line to an eshell buffer and execute it."
  (interactive)
  (eshell-extensions-send-string-to-eshell (eshell-extensions-get-cur-line))
  )

;;;###autoload
(defun eshell-extensions-send-cur-region-to-eshell ()
  "Send the current line to an eshell buffer and execute it."
  (interactive)
  (eshell-extensions-send-string-to-eshell (eshell-extensions-get-cur-region))
  )

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
