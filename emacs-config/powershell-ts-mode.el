;;; powershell-ts-mode --- Summary

;; Powershell mode using treesitter

;; To install treesitter grammar (requires Emacs 29, git, C compiler, C++ compiler):
;; M-x treesit-install-language-grammer
;; Enter in powershell as your language
;; Enter yes to build recipe for powershell interactively
;; Enter in the url of the grammar: https://github.com/airbus-cert/tree-sitter-powershell
;; Stick to the defaults for git branch, "src" directory, C compiler, and C++ compilers

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
    :feature variable
    ((variable) @font-lock-variable-name-face)

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
    ((comparison_operator) @font-lock-builtin-face)

    ;; if statement
    :language powershell
    :feature function
    :override t
    ((if_statement "if" @font-lock-keyword-face))

    ;; else statement
    :language powershell
    :feature function
    :override t
    ((else_clause "else" @font-lock-keyword-face))

    ;; foreach statement
    :language powershell
    :feature function
    :override t
    ((foreach_statement "foreach" @font-lock-keyword-face))

    ;; for statement
    :language powershell
    :feature function
    :override t
    ((for_statement "for" @font-lock-keyword-face))

    ;; while statement
    :language powershell
    :feature function
    :override t
    ((while_statement "while" @font-lock-keyword-face))

    ;; do while statement
    :language powershell
    :feature function
    :override t
    ((do_statement "do" @font-lock-keyword-face "while" @font-lock-keyword-face))

    ;; do until statement
    :language powershell
    :feature function
    :override t
    ((do_statement "do" @font-lock-keyword-face "until" @font-lock-keyword-face))

    ;; flow control statements (continue/break)
    :language powershell
    :feature function
    :override t
    ((flow_control_statement "continue" @font-lock-keyword-face))

    :language powershell
    :feature function
    :override t
    ((flow_control_statement "break" @font-lock-keyword-face))

    ;; type [System.Data] like syntax
    :language powershell
    :feature function
    :override t
    ((type_literal) @font-lock-type-face)
))

(defun powershell-ts-imenu-func-node-p (node)
  "Return non-nil if the NODE is a function definition."
  (and (equal (treesit-node-type node) "function_name")
       (equal (treesit-node-type (treesit-node-parent node)) "function_statement")))

(defun powershell-ts-imenu-func-name-function (node)
  "Return the text of a function name from a function definition NODE."
  (treesit-node-text node))

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
  
  (setq-local treesit-simple-imenu-settings
              `(("Function" powershell-ts-imenu-func-node-p nil powershell-ts-imenu-func-name-function)))

  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

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
