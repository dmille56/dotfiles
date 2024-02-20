;;; avy-ts --- Summary

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

(defgroup avy-ts-mode nil
  "Customize group for avy-ts-mode.el."
  :group 'emacs)

(defvar avy-ts-python-queries '(
"(function_definition body: (block)? @function.inner) @function.outer"
"(class_definition body: (block)? @class.inner) @class.outer"
"(parameter ((_) @parameter.inner . \",\"? @parameter.outer) @parameter.outer)"
"(lambda_parameters ((_) @parameter.inner . \",\"? @parameter.outer) @parameter.outer)"
"(argument_list ((_) @parameter.inner . \",\"? @parameter.outer) @parameter.outer)"
"(comment) @comment.inner"
"(comment)+ @comment.outer"
;; "((function_definition name: (identifier) @_name body: (block)? @test.inner) @test.outer (#match \"^test_\" @_name))"
"((function_definition name: (identifier) @name body: (block)? @test.inner) @test.outer)"
"(for_statement body: (_) @loop.inner) @loop.outer"
"(while_statement body: (_) @loop.inner) @loop.outer"
"(if_statement consequence: (_) @conditional.inner) @conditional.outer"
))

(defcustom avy-ts-queries '("(comment) @comment" "(function_statement) @func" "(if_statement) @if" "(else_clause) @else" "(elseif_clause) @elseif" "(class_statement) @class" "(param_block) @param" "(for_statement) @for" "(while_statement) @while" "(do_statement) @do" "(class_method_definition) @classmeth" "(foreach_statement) @for" "(try_statement) @try" "(catch_clause) @catch" "(finally_clause) @finally")
  "Queries to search for."
  :type '(repeat string)
  :group 'avy-ts-mode)

(defun avy-ts-query-get-positions (query-list)
  (let* (
         (start-window (window-start))
         (end-window (window-end (selected-window) t))
         (root-node (treesit-buffer-root-node))
         (captures-list (mapcar (lambda (query) (treesit-query-capture root-node query start-window end-window t)) query-list))
         (positions (sort (mapcar #'treesit-node-start (apply #'append captures-list)) #'<))
         )
    positions
    ))

(defun avy-ts-query-avy-jump (query-list)
  (interactive)
  (let* (
         (positions (avy-ts-query-get-positions query-list))
         )
    (avy-with avy-ts-query-avy-jump (avy--process positions (avy--style-fn avy-style)))
    ))

(defun avy-ts-avy-jump ()
  (interactive)
  ;;(avy-ts-query-avy-jump avy-ts-queries)
  (avy-ts-query-avy-jump avy-ts-python-queries)
)

(defun avy-ts--get-query (language queries-dir top-level)
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
                                                         (avy-ts--get-queries (substring x 1 -1)
                                                                                                       queries-dir nil))
                                                   (avy-ts--get-queries x queries-dir nil)))
                                               (split-string inherits-line ","))
                                       "\n"))))
            (buffer-string))))))

(defun avy-ts-test-scm-queries ()
  (interactive)
  (let* (
        (query (avy-ts--get-query "python" "~/dotfiles/emacs-config/treesit-queries/" t))
        )
    (avy-ts-query-avy-jump (list query))
    ))

;; :TODO: remove this global set-key
;; (global-set-key (kbd "<f9>") 'avy-ts-avy-jump)
;; 
;; (global-set-key (kbd "<f9>") 'avy-ts-test-scm-queries)

(provide 'avy-ts)
;;; avy-ts.el ends here
