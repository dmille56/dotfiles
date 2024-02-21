;;; treesit-jump --- Summary

;; Jump around using treesitter

;; requires Emacs 29+ for treesitter support
;; requires avy

;;; Commentary:

;; Notes:
;; :TODO: add code to jump between visibile classes/functions/loops/conditionals/etc.
;; :TODO: add code to allow deletion of classes/functoins/loops/etc. through avy
;; :TODO: add code to allow change inside like functionality of classes/functoins/loops/etc. through avy
;; :TODO: add compiled queries using treesit-query-compile for faster searching

; Here is a way to get an avy style jump tree to positions 1, 4 or 12. You could use any list of positions you want, e.g. calculated from some function.
; (avy-with my-jumper (avy--process '(1 4 12) (avy--style-fn avy-style)))

;; Useful links:
;; https://github.com/emacs-mirror/emacs/blob/master/admin/notes/tree-sitter/starter-guide
;; https://git.sr.ht/~meow_king/ts-query-highlight

;;; Code:

(require 'treesit)
(require 'avy)
(require 'cl-lib)

(defgroup treesit-jump-mode nil
  "Customize group for treesit-jump-mode.el."
  :group 'emacs)

(defcustom treesit-jump-queries '("(comment) @comment" "(function_statement) @func" "(if_statement) @if" "(else_clause) @else" "(elseif_clause) @elseif" "(class_statement) @class" "(param_block) @param" "(for_statement) @for" "(while_statement) @while" "(do_statement) @do" "(class_method_definition) @classmeth" "(foreach_statement) @for" "(try_statement) @try" "(catch_clause) @catch" "(finally_clause) @finally")
  "Queries to search for."
  :type '(repeat string)
  :group 'treesit-jump-mode)

(defcustom treesit-jump-queries-filter-list '("inner" "test" "param")
  "Query captures to filter out of results uses regex."
  :type '(repeat string)
  :group 'treesit-jump-mode)

(defcustom treesit-jump-queries-filter-func #'treesit-jump-queries-filter-default-func
  "Function used to filter matched treesit queries."
  :type 'function
  :group 'treesit-jump-mode)

(defcustom treesit-jump-positions-filter-func #'avy-process
  "Function used to select matched treesit queries on screen."
  :type 'function
  :group 'treesit-jump-mode)

(defun treesit-jump-queries-filter-default-func (query)
  (let* (
        (capture-name (symbol-name (car query)))
        (matches (seq-filter (lambda (s) (string-match s capture-name)) treesit-jump-queries-filter-list))
        )
    (if matches nil t)
    ))

(defun treesit-jump-query-get-captures (query-list)
  (let* (
         (start-window (window-start))
         (end-window (window-end (selected-window) t))
         (root-node (treesit-buffer-root-node))
         (raw-captures (apply #'append (mapcar (lambda (query) (treesit-query-capture root-node query start-window end-window)) query-list)))
         (captures (seq-filter (lambda (x) (funcall treesit-jump-queries-filter-func x)) raw-captures))
         )
    captures
    ))

(defun treesit-jump-query-select (query-list)
  (let* (
         (captures (treesit-jump-query-get-captures query-list))
         (positions (sort (mapcar #'treesit-node-start (mapcar #'cdr captures)) #'<))
         (selected-pos (funcall treesit-jump-positions-filter-func positions))
         )
    (if selected-pos (cl-find-if (lambda (x) (= (treesit-node-start (cdr x)) selected-pos)) captures) nil)
    ))

(defun treesit-jump-query-select-go-to (query-list)
  (interactive)
  (let* (
         (selected (treesit-jump-query-select query-list))
         (start (treesit-node-start (cdr selected)))
         )
    (when start
      (goto-char start)
      )))

(defun treesit-jump-query-select-visual (query-list)
  (interactive)
  (let* (
         (selected (treesit-jump-query-select query-list))
         (start (treesit-node-start (cdr selected)))
         (end (treesit-node-end (cdr selected)))
         )
    (when (and start end)
          (goto-char start)
          (set-mark end)
         )))

(defun treesit-jump-query-select-delete (query-list)
  (interactive)
  (let* (
         (selected (treesit-jump-query-select query-list))
         (start (treesit-node-start (cdr selected)))
         (end (treesit-node-end (cdr selected)))
         )
    (when (and start end)
      (delete-region start end))))

(defun treesit-jump-avy-jump ()
  (interactive)
  ;; (treesit-jump-query-select-go-to treesit-jump-python-queries)
  (treesit-jump-query-select-visual treesit-jump-python-queries)
)

(defun treesit-jump--get-query (language queries-dir top-level)
  "Get tree sitter query for `LANGUAGE' from `QUERIES-DIR'.
`TOP-LEVEL' is used to mention if we should load optional inherits."
  (let (
        (filename (concat queries-dir language "/textobjects.scm"))
        )
    (with-temp-buffer
      (if (file-exists-p filename)
          (progn
            (insert-file-contents filename)
            (goto-char (point-min))
            (let ((inherits-line (evil-textobj-tree-sitter--get-inherits-line filename)))
              (if inherits-line
                  (insert (string-join (mapcar (lambda (x)
                                                 (if (string-prefix-p "(" x)
                                                     (if top-level
                                                         (treesit-jump--get-queries (substring x 1 -1)
                                                                                                       queries-dir nil))
                                                   (treesit-jump--get-queries x queries-dir nil)))
                                               (split-string inherits-line ","))
                                       "\n"))))
            (buffer-string))))))

(defun treesit-jump-test-scm-queries ()
  (interactive)
  (let* (
        (query (treesit-jump--get-query "python" "~/dotfiles/emacs-config/treesit-queries/" t))
        )
    (treesit-jump-query-select-go-to (list query))
    ;; (treesit-jump-query-select-visual (list query))
    ;; (treesit-jump-query-select-delete (list query))
    ))

;; :TODO: remove this global set-key
;; (global-set-key (kbd "<f9>") 'treesit-jump-avy-jump)
;; 
(global-set-key (kbd "<f9>") 'treesit-jump-test-scm-queries)

(provide 'treesit-jump)
;;; treesit-jump.el ends here
