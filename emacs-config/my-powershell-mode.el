;; Define the PowerShell mode

(defvar powershell-mode-syntax-table nil "Syntax table for `my-powershell-mode'.")

(setq powershell-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; PowerShell comment syntax: # (single-line)
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        synTable))

;; Function to apply syntax properties for multi-line comments
;; :TODO: fix to make the trailing > also have the correct syntax highlighting
(defun powershell-syntax-propertize-function (start end)
  "Apply syntax properties for multi-line comments from START to END."
  (goto-char start)
  (while (re-search-forward "<#\\|\\#>" end t)
    (let ((match (match-string 0)))
      (put-text-property (match-beginning 0) (match-end 0)
                         'syntax-table
                         (if (equal match "<#")
                             '(11 . nil)  ;; Comment start
                             '(12 . nil)) ;; Comment end
                         ))))

;; Keywords for syntax highlighting
(defvar powershell-font-lock-keywords
  `(
    ;; Keywords
    ("\\_<\\(function\\|param\\|if\\|else\\|foreach\\|while\\|switch\\|return\\|True\\|False\\|Begin\\|Process\\|End\\)\\_>" . font-lock-keyword-face)
    
    ;; Cmdlets
    ("\\_<\\(Get-\\|Set-\\|New-\\|Remove-\\|Start-\\|Stop-\\|Out-\\|Invoke-\\)[A-Za-z0-9-]+\\_>" . font-lock-function-name-face)
    
    ;; Variables
    ("\\_<\\($[a-zA-Z0-9_]+\\)\\_>" . font-lock-variable-name-face)

    ;; Operators
    ("\\_<\\(-eq\\|-ne\\|-lt\\|-le\\|-gt\\|-ge\\|-like\\|-notlike\\|-match\\|-notmatch\\|-contains\\|-notcontains\\|-in\\|-notin\\)\\_>" . font-lock-builtin-face)

    ;; Add more patterns here
    ))

;; Define the major mode
(define-derived-mode my-powershell-mode fundamental-mode "my-pwsh"
  "Major mode for editing PowerShell scripts."
  :syntax-table powershell-mode-syntax-table
  (set (make-local-variable 'syntax-propertize-function)
       'powershell-syntax-propertize-function)
  (setq font-lock-defaults '((powershell-font-lock-keywords))))

;; Associate .ps1 files with powershell-mode
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . my-powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm1\\'" . my-powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psd1\\'" . my-powershell-mode))

(provide 'my-powershell-mode)
