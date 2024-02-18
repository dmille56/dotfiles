;;; avy-ts --- Summary

;; Jump around using treesitter

;; requires Emacs 29+ for treesitter support
;; requires avy

;;; Commentary:

;; Notes:
;; :TODO: add code to jump between visibile classes/functions/loops/conditionals/etc.
;; :TODO: add code to allow deletion of classes/functoins/loops/etc. through avy
;; :TODO: add code to allow change inside like functionality of classes/functoins/loops/etc. through avy

;; Useful links:
;; https://github.com/emacs-mirror/emacs/blob/master/admin/notes/tree-sitter/starter-guide
;; https://git.sr.ht/~meow_king/ts-query-highlight

;;; Code:

(require 'treesit)
(require 'avy)
(require 'evil-textobj-tree-sitter-core)

(defun my/query-get-positions (query-list)
  (let* (
         (start-window (window-start))
         (end-window (window-end (selected-window) t))
         (root-node (treesit-buffer-root-node))
         (captures-list (mapcar (lambda (query) (treesit-query-capture root-node query start-window end-window t)) query-list))
         (positions (sort (mapcar #'treesit-node-start (apply #'append captures-list)) #'<))
         )
    positions
    ))

(defun my/query-jump (query-list)
  (interactive)
  (let* (
         (positions (my/query-get-positions query-list))
         )
    (avy-with my/query-jump (avy--process positions (avy--style-fn avy-style)))
    ))

(defun my/query-i ()
  (interactive)
  (my/query-jump '("(comment) @comment" "(function_statement) @function" "(if_statement) @if"))
)

; Answering my own question... Here is a way to get an avy style jump tree to positions 1, 4 or 12. You could use any list of positions you want, e.g. calculated from some function.
; 
; (avy-with my-jumper (avy--process '(1 4 12) (avy--style-fn avy-style)))

;; (treesit-search-subtree) (treesit-search-forward) (treesit-induce-sparse-tree)

;; (treesit-buffer-root-node)

;; ((comment) @comment
;;  (function_statement) @func
;;  (if_statement) @if
;;  (else_clause) @else
;;  (elseif_clause) @elseif
;;  (class_statement) @class
;;  (param_block) @param)

(global-set-key (kbd "<f9>") 'my/query-i)

(provide 'avy-ts)
;;; avy-ts.el ends here
