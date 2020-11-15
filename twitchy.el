;;; Package --- sumary
;;; Commentary:

(require 'dash)
(require 'helm)
(require 's)

;;; Code:

(setq twitchy-proc-list 'nil)

(defvar twitchy-mode-hook nil)

(defvar twitchy-mode-map
  (let ((map (make-keymap)))
    (define-key map "p" 'twitchy-play-stream)
    (define-key map "d" 'twitchy-list)
    (define-key map "s" 'twitchy-stop)
    (define-key map "o" 'twitchy-select-stream)
    map)
  "Keymap for twitchy major mode.")

(evil-define-key 'normal twitchy-mode-map
  "s" 'twitchy-stop
  "d" 'twitchy-list
  "p" 'twitchy-play-stream
  "o" 'twitchy-select-stream)

(define-derived-mode twitchy-mode
   special-mode "twitch"
   "Major mode for watching twitch."
   (setq case-fold-search nil))

(defun twitchy-list ()
  "List active stream."
  (interactive)
  (print
   (map-keys twitchy-proc-list)))

(defun twitchy-stop ()
  "Stop active stream."
  (interactive)
  (let* (
	  (name (helm :sources (helm-build-sync-source "Select Twitch Stream To Stop:"
						       :candidates (map-keys twitchy-proc-list)
						       :fuzzy-match t)
					    :buffer "*helm twitch stream stop*"))
	  (proc (cdr (assoc name twitchy-proc-list)))
	  )
    (if name
	(progn
	  (setq twitchy-proc-list (assoc-delete-all name twitchy-proc-list))
	  (interrupt-process proc)
	  (stop-process proc)
	  )
      )
    )
  )

(defun twitchy-call-helm (streams)
  "Call helm to select stream.  STREAMS = a list of streams to choose from."
  (helm :sources (helm-build-sync-source "Select Twitch Stream:"
		   :candidates streams
		   :fuzzy-match t)
	:buffer "*helm twitch stream select*"))

(defun twitchy-get-output (proc str)
  "Process filter to 'make-process' in twitchy-select-stream.  PROC = process to filter, STR = output."
  (setq twitchy-output (cons str twitchy-output)))

(defun twitchy-play-sentinel (process event)
  "Process sentinel to 'make-process' in twitchy-play.  PROCESS = process, EVENT = event.  This deletes the process from the process list on quit."
  (progn
   ;; (princ
   ;;  (format "Process sentinel: %s had the event '%s'" process event))
   (if
       (or
	(string-equal "finished\n" event)
	(string-equal "deleted\n" event)
	(string-prefix-p "exited abnormally with code" event)
	)
       (setq twitchy-proc-list (rassq-delete-all process twitchy-proc-list)))
   )
  )

(defun twitchy-play (stream)
  "Play STREAM."
  (let* (
	 (twitch-prefix "https://www.twitch.tv/")
	 (stream-name (s-chop-prefix twitch-prefix stream))
	 (url (concat twitch-prefix stream-name))
	 (buffer-name (get-buffer-create "*twitch streams*"))
	 )
    (switch-to-buffer buffer-name)
    (twitchy-mode)
    (setq-local buffer-read-only t)
    (setq twitchy-proc-list
	  (cons
	  (cons stream-name (make-process
	   :name "streamlink"
	   :buffer buffer-name
	   :command (list "streamlink" "--player=mpv --fs --profile=gpu-hq --scale=ewa_lanczossharp" url "360p")
	   :stderr buffer-name
	   :sentinel #'twitchy-play-sentinel
	   ))
	  twitchy-proc-list)
	  )
    )
  )

(defun twitchy-play-stream ()
  "Input a twitch stream to play."
  (interactive)
  (let* (
	 (default (or
		   (ignore-errors
		     (current-kill 0 t))
		   (thing-at-point 'symbol t)))
	 (defaultTrim (if default
			  (s-trim default)
			'nil))
	 (prompt (concat "Enter twitch stream name to play (default: '" defaultTrim "'): "))
	 (name (read-string prompt 'nil 'nil defaultTrim))
	 )
    (if name (twitchy-play name))))

(defun twitchy-select-stream ()
  "Select a twitch stream to play via Helm interface."
  (interactive)
  (setq twitchy-output nil)
  (let (
      (proc
       (make-process :command '("twitchy" "--non-interactive")
		     :name "twitchy"
                     :filter #'twitchy-get-output)))
    (while (eq (process-status proc) 'run)
	 (accept-process-output proc))
       (let (
	     (selected (twitchy-call-helm (twitchy-wrangle twitchy-output))))
	 (twitchy-play
	  (s-trim
	   (car
	    (split-string selected "\t")))))))

(defun twitchy-wrangle (streams)
  "Wrangle twitchy STREAMS output to pretty print by padding it with spaces."
  (let* (
	 (orig (-map (lambda (n) (split-string n ",")) (split-string (s-trim (car streams)) "\n")))
	 (nRows (length orig))
	 (nCols (length (nth 0 orig)))
	 (rowSeq (number-sequence 0 (- nRows 1)))
	 (colSeq (number-sequence 0 (- nCols 1)))
	 (maxLens (-map (lambda (i) (-reduce 'max (-map 'length (-select-column i orig)))) colSeq))
	 (paddedSwap (-map (lambda (i) (-map (lambda (j) (s-pad-right (nth i maxLens) " " j)) (-select-column i orig))) colSeq))
	 (backToNorm (-map (lambda (j) (-select-column j paddedSwap)) rowSeq))
	 (result (-map (lambda (i) (s-join "\t" i)) backToNorm))
	 )
    result
    )
  )

(provide 'twitchy-mode)
(provide 'twitchy)
;;; twitchy.el ends here
