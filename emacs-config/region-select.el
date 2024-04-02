;;; region-select.el  --- Summary

;;; Commentary:
;;; Version: 0.10.0

;;; Code:

(require 'cl-lib)

(defgroup region-select nil
  "Customize group for region-select.el."
  :group 'emacs)

(defcustom region-select-faces-old
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

;; Dracula theme colors
(defcustom region-select-faces
  '(
    (:background "#8be9fd" :foreground "#282a36")
    (:background "#50fa7b" :foreground "#282a36")
    (:background "#ffb86c" :foreground "#282a36")
    (:background "#ff79c6" :foreground "#282a36")
    (:background "#bd93f9" :foreground "#282a36")
    (:background "#ff5555" :foreground "#282a36")
    (:background "#f1fa8c" :foreground "#282a36")
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

(defun region-select-adjust-list-length (input-list target-length)
  "Adjust the length of INPUT-LIST to TARGET-LENGTH.
By repeating or truncating elements."
  (let ((result ())  ;; Initialize an empty list to store the result.
        (current-list input-list))  ;; Start with the initial list.
    ;; Loop and construct the list with the required elements.
    (cl-loop for i from 0 below target-length
             do (push (nth (mod i (length input-list)) input-list) result))
    ;; Reverse the list to maintain the original order and return.
    (nreverse result)))

;; :TODO: make it work with beginning and end of region
(defun region-select-dynamic-overlay-session (regions)
  "Start a session to dynamically overlay REGIONS and jump to the match."
  (interactive)
  (let* ((input "")
         (strings (region-select-expand-keyboard-characters region-select-keyboard-characters-list (length regions)))
         (faces (region-select-adjust-list-length region-select-faces (length regions)))
         (abort 'nil)
         (res 'nil)
         (overlays (mapcar (lambda (region)
                             (let* ((start (car region))
                                    (end (cdr region)))
                               (make-overlay start (+ start (length (car strings))))))
                           regions))
         (overlays2 (mapcar (lambda (region)
                             (let* ((start (car region))
                                    (end (cdr region)))
                               (make-overlay end (+ end (length (car strings))))))
                           regions)))
    ;; Initial overlay setup
    (cl-loop for ov in overlays
             for ov2 in overlays2
             for str in strings
             for face in faces
             do (overlay-put ov 'display str)
                (overlay-put ov2 'display str)
                (overlay-put ov 'face face)
                (overlay-put ov2 'face face))
    ;; User input loop
    (setq abort
          (catch 'exit
            (while t
              (let ((char (read-char-exclusive "Type next character (RET or ESC to abort): ")))
                ;; Exit on RET or ESC
                (if (or (eq char 13) (eq char 27)) ;; 13 = Return, 27 = Escape
                    (throw 'exit t)
                  (setq input (concat input (char-to-string char))))
                ;; Update or clear overlays based on input
                (cl-loop for ov in overlays
                         for ov2 in overlays2
                         for str in strings
                         if (string-prefix-p input str)
                         do (overlay-put ov 'display (substring str (length input)))
                            (overlay-put ov2 'display (substring str (length input)))
                         else do (delete-overlay ov)
                                 (delete-overlay ov2))
                ;; Check for completion
                (let ((remaining (cl-remove-if-not (lambda (s) (string-prefix-p input s)) strings)))
                  (when (= (length remaining) 1)
                    (let ((final-pos (nth (cl-position (car remaining) strings :test 'equal) regions)))
                      (setq res final-pos)
                      (mapc 'delete-overlay overlays)
                      (mapc 'delete-overlay overlays2)
                      (throw 'exit 'nil))))))))
    ;; Remove overlays if aborted
    (if (eq abort t)
        (progn
          (message "Aborted.")
          (mapc 'delete-overlay overlays)
          (mapc 'delete-overlay overlays2)))
    res))

(defun generate-random-visible-buffer-regions ()
  "Generate 5 random regions within the visible part of the current buffer."
  (interactive)
  (let ((regions '())
        (start (window-start))
        (end (window-end)))
    (dotimes (i 5 regions)
      (let* ((region-start (+ start (random (1+ (- end start)))))
             (region-end (+ region-start (random (1+ (- end region-start))))))
        (push (cons region-start region-end) regions)))
    regions))

(defun test-overlay ()
  (interactive)
  (let ((pos (region-select-dynamic-overlay-session (sort (generate-random-visible-buffer-regions) (lambda (a b) (< (car a) (car b)))))))
    (if pos (goto-char (car pos)))))

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
