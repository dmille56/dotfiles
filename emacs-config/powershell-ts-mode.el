;;; powershell-ts-mode --- Summary

;; Powershell mode using treesitter
;; treesitter grammar at: https://github.com/airbus-cert/tree-sitter-powershell

;;; Commentary:

;;; Code:

;; :TODO: add syntax highlighting
;; :TODO: add indentation support
;; :TODO: add imenu support

(require 'treesit)
(require 'prog-mode)

;; strings
;; (string_literal (expandable_string_literal))))))))))))))
;; (string_literal (verbatim_string_characters))))))))))))))

;; variable
;; (unary_expression (variable)))))))))))

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
    ((string_literal (expandable_string_literal) @font-lock-string-face))

    :language powershell
    :feature string
    :override t
    ((string_literal (verbatim_string_characters) @font-lock-string-face))
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
))

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
