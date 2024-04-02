;;; region-select.el  --- Summary

;;; Commentary:
;;; Version: 0.10.0

;;; Code:

(require 'cl-lib)

(defgroup region-select nil
  "Customize group for region-select.el."
  :group 'emacs)

(defcustom region-select-faces
  '(
    (:background "red" :foreground "white")
    (:background "blue" :foreground "white")
    (:background "green" :foreground "white")
    (:background "yellow" :foreground "white")
    (:background "purple" :foreground "white")
    (:background "orange" :foreground "white")
    (:background "brown" :foreground "white")
    (:background "pink" :foreground "white")
    )
  "List of faces to use when selecting regions."
  :type '(repeat face)
  :group 'region-select)

(setq-default region-select-buffer-blackout-face '(:background "black" :foreground "grey"))

(setq-default region-select-keyboard-characters-list '("a" "s" "d" "f" "g" "h" "j" "k" "l" "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" ";" "'" "z" "x" "c" "v" "b" "n" "m" "," "." "[" "]"))

(defun region-select-expand-keyboard-characters (characters list-length)
  "Expand CHARACTERS into a list of strings equal to LIST-LENGTH."
  (let ((result '())
        (current-string ""))
    (if (< list-length (length characters))
        (setq result (cl-subseq characters 0 list-length))
      (progn
        (setq result (reverse (cl-subseq characters 0 (length characters))))
        (catch 'done
          (dolist (char1 characters)
            (dolist (char2 characters)
              (setq current-string (concat char1 char2))
              (push current-string result)
              (if (>= (length result) list-length) (throw 'done t)))))
        (setq result (reverse result))))
    result))

(defun select-region-adjust-list-length (input-list target-length)
  "Adjust the length of INPUT-LIST to TARGET-LENGTH.
  By repeating or truncating elements."
  (let ((result ())  ;; Initialize an empty list to store the result.
        (current-list input-list))  ;; Start with the initial list.
    ;; Loop and construct the list with the required elements.
    (cl-loop for i from 0 below target-length
             do (push (nth (mod i (length input-list)) input-list) result))
    ;; Reverse the list to maintain the original order and return.
    (nreverse result)))

(defun select-region-dynamic-overlay-session (strings positions)
  "Start a session to dynamically overlay strings and jump to the match."
  (interactive)
  ;; Example data: List of strings and their start positions in the buffer
  (let* ((input "")
         (res 'nil)
         (overlays (mapcar (lambda (pos) (make-overlay pos (+ pos (length (car strings))))) positions)))
    ;; Initial overlay setup
    (cl-loop for ov in overlays
             for str in strings
             do (overlay-put ov 'display str)
                (overlay-put ov 'face '(:background "red" :foreground "white")))
    ;; User input loop
    (setq res
          (catch 'exit
            (while t
              (let ((char (read-char-exclusive "Type next character (RET to finish): ")))
                ;; Exit on RET
                (if (or (eq char 'RET) (eq char 13))
                    (throw 'exit 'abort)
                  (setq input (concat input (char-to-string char))))
                ;; Update or clear overlays based on input
                (cl-loop for ov in overlays
                         for str in strings
                         if (string-prefix-p input str)
                         do (overlay-put ov 'display (substring str (length input)))
                         else do (delete-overlay ov))
                ;; Check for completion
                (let ((remaining (cl-remove-if-not (lambda (s) (string-prefix-p input s)) strings)))
                  (when (= (length remaining) 1)
                    (let ((final-pos (nth (cl-position (car remaining) strings :test 'equal) positions)))
                      (message "Jumping to: %s" (car remaining))
                      (goto-char final-pos)
                      (mapc 'delete-overlay overlays)
                      (throw 'exit 'nil))))))))
    (if (eq res 'abort)
        (message "Aborted!")
        (cl-loop for ov in overlays
                 do (delete-overlay ov)))))

(defun generate-random-visible-buffer-positions ()
  "Generate 5 random positions within the visible part of the current buffer."
  (interactive)
  (let ((positions '())
        (start (window-start))
        (end (window-end)))
    (dotimes (i 5 positions)
      (push (+ start (random (1+ (- end start)))) positions))
    positions))

(defun test-overlay ()
  (interactive)
  (select-region-dynamic-overlay-session '("ab" "ac" "ad" "gh" "jk") (sort (generate-random-visible-buffer-positions) #'<)))

(global-set-key (kbd "<f8>") 'test-overlay)

;; :TODO: figure out performance issue with this function (it causes emacs to lag super slow when evaluated)
;; (defun overlay-buffer (face)
;;   "Overlay the entire buffer with a face that has a black background and grey foreground."
;;   (interactive)
;;   ;; Create an overlay that spans the entire buffer
;;   (let ((buffer-overlay (make-overlay (window-start) (window-end))))
;;     ;; Set the overlay's face property
;;     (overlay-put buffer-overlay 'face face)
;;     ;; Optionally, store the overlay in a buffer-local variable if you need to remove it later
;;     (setq-local buffer-local-overlay buffer-overlay)))
;; 
;; (defun overlay-buffer-black-and-grey ()
;;   (interactive)
;;   (overlay-buffer region-select-buffer-blackout-face))
;; 
;; (defun remove-buffer-overlay ()
;;   "Remove the overlay from the buffer, if it exists."
;;   (interactive)
;;   (when (and (boundp 'buffer-local-overlay) buffer-local-overlay)
;;     (delete-overlay buffer-local-overlay)
;;     (setq buffer-local-overlay nil)))

(provide 'region-select)
;;; region-select.el ends here
