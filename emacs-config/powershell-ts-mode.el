;;; powershell-ts-mode --- Summary

;; Powershell mode using treesitter
;; treesitter grammar at: https://github.com/airbus-cert/tree-sitter-powershell

;;; Commentary:

;;; Code:

;; :TODO: add syntax highlighting... fix all the features being functions...
;; :TODO: add indentation support
;; :TODO: add imenu support

(require 'treesit)
(require 'prog-mode)

(defvar powershell-ts-font-lock-rules
  '(
    :language powershell
    :feature comment
    ((comment) @font-lock-comment-face)

    :language powershell
    :feature variable
    ((unary_expression (variable) @font-lock-variable-name-face))

    :language powershell
    :feature string
    ((string_literal (verbatim_string_characters) @font-lock-string-face))

    :language powershell
    :feature string
    :override t
    ((string_literal (expandable_string_literal) @font-lock-string-face))

    :language powershell
    :feature string
    :override t
    ((string_literal (expandable_here_string_literal) @font-lock-string-face))

    :language powershell
    :feature function
    ((function_statement "function" @font-lock-operator-face (function_name) @font-lock-function-name-face))
    
    :language powershell
    :feature function
    :override t
    ((command command_name: (command_name) @font-lock-function-call-face))
    
    ;; parameter
    :language powershell
    :feature function
    :override t
    ((command_parameter) @font-lock-constant-face)

    ;; comparison operator
    :language powershell
    :feature function
    :override t
    ((comparison_operator) @font-lock-operator-face)

    ;; if statement
    :language powershell
    :feature function
    :override t
    ((if_statement "if" @font-lock-operator-face))

    ;; else statement
    :language powershell
    :feature function
    :override t
    ((else_clause "else" @font-lock-operator-face))


    ;; foreach statement
    :language powershell
    :feature function
    :override t
    ((foreach_statement "foreach" @font-lock-operator-face))

    ;; for statement
    :language powershell
    :feature function
    :override t
    ((for_statement "for" @font-lock-operator-face))

    ;; while statement
    :language powershell
    :feature function
    :override t
    ((while_statement "while" @font-lock-operator-face))

    ;; do while statement
    :language powershell
    :feature function
    :override t
    ((do_statement "do" @font-lock-operator-face "while" @font-lock-operator-face))

    ;; do until statement
    :language powershell
    :feature function
    :override t
    ((do_statement "do" @font-lock-operator-face "until" @font-lock-operator-face))

    ;; flow control statements (continue/break)
    :language powershell
    :feature function
    :override t
    ((flow_control_statement "continue" @font-lock-operator-face))

    :language powershell
    :feature function
    :override t
    ((flow_control_statement "break" @font-lock-operator-face))
))

(defun powershell-ts-setup ()
  "Setup treesit for powershell-ts-mode."
  (interactive)

  (setq-local treesit-font-lock-settings
               (apply #'treesit-font-lock-rules
                    powershell-ts-font-lock-rules))

  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-level 5)

  (setq-local treesit-font-lock-feature-list
              '((comment)
                (variable)
                (string)
                ( function )))

  ;; (setq-local treesit-simple-indent-rules powershell-ts-indent-rules)

  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode powershell-ts-mode prog-mode "PS[ts]"
  "Major mode for editing Powershell with tree-sitter."
  :syntax-table prog-mode-syntax-table
  
  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'powershell)
    (treesit-parser-create 'powershell)
    (powershell-ts-setup)))

(provide 'powershell-ts-mode)
;;; powershell-ts-mode.el ends here