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
  (avy-ts-query-avy-jump avy-ts-queries)
)

;; :TODO: remove this global set-key
(global-set-key (kbd "<f9>") 'avy-ts-avy-jump)

(provide 'avy-ts)
;;; avy-ts.el ends here
