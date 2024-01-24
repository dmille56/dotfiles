;;; my-powershell-mode --- Summary

;; Simple mode for powershell syntax highlighting

;;; Commentary:

;;; Code:

(defvar powershell-mode-syntax-table nil "Syntax table for `my-powershell-mode'.")

(setq powershell-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; PowerShell comment syntax: # (single-line)
        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        ;; PowerShell single line single quote strings
        (modify-syntax-entry ?' "\"" synTable)
        synTable))

;; Function to apply syntax properties for multi-line comments
;; :TODO: fix to make the trailing > also have the correct syntax highlighting
(defun powershell-syntax-propertize-comments (start end)
  "Apply syntax properties for multi-line comments from START to END."
  (goto-char start)
  (while (re-search-forward "<#\\|\\#>" end t)
    (let ((match (match-string 0)))
      (put-text-property (match-beginning 0) (match-end 0)
                         'syntax-table
                         (if (equal match "<#")
                             '(11 . nil)  ;; Comment start
                             '(12 . nil)) ;; Comment end
                         )
      )))

(defun powershell-syntax-propertize-strings (start end)
  "Apply syntax properties for multi-line strings from START to END.
For double quotes."
  (goto-char start)
  (while (re-search-forward "@\"\n\\|\\\n\"@" end t)
    (let ((match (match-string 0)))
      (put-text-property (match-beginning 0) (match-end 0)
                         'syntax-table (string-to-syntax "|")
                         ))))

(defun powershell-syntax-propertize-strings-single-quote (start end)
  "Apply syntax properties for multi-line strings from START to END.
For single quotes."
  (goto-char start)
  (while (re-search-forward "@\'\n\\|\\\n\'@" end t)
    (let ((match (match-string 0)))
      (put-text-property (match-beginning 0) (match-end 0)
                         'syntax-table (string-to-syntax "|")
                         ))))

(defun powershell-syntax-propertize-function (start end)
  "Wrapper function to apply syntax properties for comments and strings.
Go from START to END."
  (powershell-syntax-propertize-comments start end)
  (powershell-syntax-propertize-strings start end)
  (powershell-syntax-propertize-strings-single-quote start end)
  )

;; Taken from About_Keywords
(defvar powershell-keywords
  (concat "\\_<"
          (regexp-opt
           '(
             "begin" "break" "catch" "class"
             "continue" "data" "define" "do"
             "dynamicparam" "else" "elseif" "end"
             "exit" "filter" "finally" "for"
             "foreach" "from" "function" "if"
             "in" "inlinescript" "parallel" "param"
             "process" "return" "switch" "throw"
             "trap" "try" "until" "using"
             "var" "while" "workflow"
             )
           t)
          "\\_>")
  "PowerShell keywords.")

(defvar powershell-operators
  (concat "\\_<"
          (regexp-opt
           '("-eq" "-ne" "-gt" "-ge" "-lt" "-le"
             ;; case sensitive versions
             "-ceq" "-cne" "-cgt" "-cge" "-clt" "-cle"
             ;; explicitly case insensitive
             "-ieq" "-ine" "-igt" "-ige" "-ilt" "-ile"
             "-band" "-bor" "-bxor" "-bnot"
             "-and" "-or" "-xor" "-not" "!"
             "-like" "-notlike" "-clike" "-cnotlike" "-ilike" "-inotlike"
             "-match" "-notmatch" "-cmatch" "-cnotmatch" "-imatch" "-inotmatch"
             "-contains" "-notcontains" "-ccontains" "-cnotcontains"
             "-icontains" "-inotcontains"
             "-replace" "-creplace" "-ireplace"
             "-is" "-isnot" "-as" "-f"
             "-in" "-cin" "-iin" "-notin" "-cnotin" "-inotin"
             "-split" "-csplit" "-isplit"
             "-join"
             "-shl" "-shr"
             ;; Questionable --> specific to certain contexts
             "-casesensitive" "-wildcard" "-regex" "-exact" ;specific to case
             "-begin" "-process" "-end" ;specific to scriptblock
             ) t)
          "\\_>")
  "PowerShell operators.")

(defvar powershell-scope-names
  '("global"   "local"    "private"  "script"   )
  "Names of scopes in PowerShell mode.")

(defvar powershell-variable-drive-names
  (append '("env" "function" "variable" "alias" "hklm" "hkcu" "wsman") powershell-scope-names)
  "Names of scopes in PowerShell mode.")

(defconst powershell-variables-regexp
  ;; There are 2 syntaxes detected: ${[scope:]name} and $[scope:]name
  ;; Match 0 is the entire variable name.
  ;; Match 1 is scope when the former syntax is found.
  ;; Match 2 is scope when the latter syntax is found.
  (concat
   "\\_<$\\(?:{\\(?:" (regexp-opt powershell-variable-drive-names t)
   ":\\)?[^}]+}\\|"
   "\\(?:" (regexp-opt powershell-variable-drive-names t)
   ":\\)?[a-zA-Z0-9_]+\\_>\\)")
  "Identifies legal powershell variable names.")

(defconst powershell-function-names-regex
  ;; Syntax detected is [scope:]verb-noun
  ;; Match 0 is the entire name.
  ;; Match 1 is the scope if any.
  ;; Match 2 is the function name (which must exist)
  (concat
   "\\_<\\(?:" (regexp-opt powershell-scope-names t) ":\\)?"
   "\\([A-Z][a-zA-Z0-9]*-[A-Z0-9][a-zA-Z0-9]*\\)\\_>")
  "Identifies legal function & filter names.")

;; Keywords for syntax highlighting
(defvar powershell-font-lock-keywords
  `(
    ;; Keywords
    (powershell-keywords . font-lock-keyword-face)
    
    ;; Functions
    (powershell-function-names-regex . font-lock-function-name-face)
    
    ;; Variables
    (powershell-variables-regexp . font-lock-variable-name-face)

    ;; Operators
    (powershell-operators . font-lock-builtin-face)

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
;; (add-to-list 'auto-mode-alist '("\\.ps1\\'" . my-powershell-mode))
;; (add-to-list 'auto-mode-alist '("\\.psm1\\'" . my-powershell-mode))
;; (add-to-list 'auto-mode-alist '("\\.psd1\\'" . my-powershell-mode))

(provide 'my-powershell-mode)
;;; my-powershell-mode.el ends here
