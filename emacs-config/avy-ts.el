;;; avy-ts --- Summary

;; Jump around using treesitter

;; requires Emacs 29+ for treesitter support
;; requires avy

;;; Commentary:

;; Notes:
;; :TODO: add code to jump between visibile classes/functions/loops/conditionals/etc.

;;; Code:

(require 'treesit)
(require 'avy)
(require 'evil-textobj-tree-sitter-core)

(defun my/query-tree-sitter-ast (query-patterns-string)
  (interactive "sEnter your Tree-sitter query patterns (separated by ';'): ")
  ;; Ensure tree-sitter-mode is active in the current buffer
  (unless (bound-and-true-p tree-sitter-mode)
    (tree-sitter-mode))

  (let ((tree tree-sitter-tree))
    ;; Check if a valid tree is returned
    (if tree
        (let ((root-node (tsc-root-node tree))
              ;; Split the input string into a list of patterns
              (patterns (split-string query-patterns-string ";" t "\\s-+")))
          (dolist (pattern patterns)
            (let* ((query (tsc-make-query tree-sitter-language pattern))
                   (captures (tsc-query-captures query root-node nil))
                   (captures-list (append captures nil)))
              (dolist (capture captures-list)
                (let ((node (car capture)))
                  ;; Ensure node is valid
                  (when node
                    (let ((start (tsc-node-start-position node))
                          (end (tsc-node-end-position node)))
                      (message "Pattern: %s, Match: %s, Start: %d, End: %d"
                               pattern
                               (buffer-substring-no-properties start end)
                               start end)))))))
      (message "Tree-sitter is not active or not available for the current buffer.")))))

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
    (avy-with my/query-jump(avy--process positions (avy--style-fn avy-style)))
    ))

(defun my/query-i ()
  (interactive)
  (my/query-jump '("(comment) @comment" "(function_statement) @function" "(if_statement) @if"))
)

(defun my/query-tree-small ()
  (interactive)
  (let* (
         (start-window (window-start))
         (end-window (window-end (selected-window) t))
         (root-node (treesit-buffer-root-node))
         (query "(comment) @comment")
         (captures (treesit-query-capture root-node query start-window end-window t))
         (captures-list (append captures nil))
         (positions (mapcar #'treesit-node-start captures-list))
         )
    (avy-with my/query-tree-small (avy--process positions (avy--style-fn avy-style)))
    ))
    ;; (dolist (capture captures-list)
    ;;  (let ((node capture))
    ;;    (let ((start (treesit-node-start node))
    ;;          (text (treesit-node-text node))
    ;;          )
    ;;      (message "Query: %s, Match: %s, Start: %d" query text start)
    ;;      )))))

; Answering my own question... Here is a way to get an avy style jump tree to positions 1, 4 or 12. You could use any list of positions you want, e.g. calculated from some function.
; 
; (avy-with my-jumper (avy--process '(1 4 12) (avy--style-fn avy-style)))

(defun my/query-ts ()
  (interactive)
  (let* (
         (root-node (treesit-buffer-root-node))
         ;;(query "(comment) @comment")
         (query "(function_statement) @function")
         (captures (treesit-query-capture root-node query))
         (captures-list (append captures nil))
         (capture (car captures-list))
         (capture-node (cdr capture))
         (start (treesit-node-start capture-node))
         (end (treesit-node-end capture-node))
         (text (treesit-node-text capture-node t))
         (start-window (window-start))
         (end-window (window-end (selected-window) t))
         )
    (if (and (>= start start-window) (<= start end-window))
        (message (format "Start: %s. end %s. text: %s." start end text)))
    ;;(message (format "Window start: %s. end %s." start-window end-window))
    ;; (goto-char start)
    ))

;; (treesit-search-subtree) (treesit-search-forward) (treesit-induce-sparse-tree)

;; (treesit-buffer-root-node)

;; ((comment) @comment
;;  (function_statement) @func
;;  (if_statement) @if
;;  (else_clause) @else
;;  (elseif_clause) @elseif
;;  (class_statement) @class
;;  (param_block) @param)

(global-set-key (kbd "<f9>") 'my/query-tree-small)

(global-set-key (kbd "<f9>") 'my/query-i)

 ;; (class_definition class)
 ;; (function_definition def)

         ;; (query (tsc-make-query tree-sitter-language f-query))
         ;; (matches (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties))

(provide 'avy-ts)
;;; avy-ts.el ends here
