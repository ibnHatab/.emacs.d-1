;;; init-git.el --- major git modes for config, ignore and attributes.
;;;
;;; Code: down below
;;; 
;;; Commentary: based on magit/git-modes
;;; init-git.el ends here

(require 'conf-mode)
(require 'easymenu)
(require 'thingatpt)
(require 'rx)
(require 'conf-mode)

(defun gitconfig-line-indented-p ()
  "Return t if the current line is indented correctly."
  (save-excursion
    (beginning-of-line)
    (or (looking-at (rx line-start "["
                        symbol-start
                        (minimal-match (zero-or-more not-newline))
                        symbol-end "]"))
        (looking-at (concat (rx line-start)
                            (gitconfig-indentation-string)
                            (rx symbol-start (or (syntax word)
                                                 (syntax symbol)))))
        (looking-at (rx (zero-or-one "\t") (or "#" ";"))))))

(defun gitconfig-point-in-indentation-p ()
  "Return if the point is in the indentation of the current line."
  (save-excursion
    (let ((pos (point)))
      (back-to-indentation)
      (<= pos (point)))))

(defun gitconfig-indent-line ()
  "Indent the current line."
  (interactive)
  (if (gitconfig-line-indented-p)
      (when (gitconfig-point-in-indentation-p)
        (back-to-indentation))
    (let ((old-point (point-marker))
          (was-in-indent (gitconfig-point-in-indentation-p)))
      (beginning-of-line)
      (delete-horizontal-space)
      (unless (equal (char-after) ?\[)
        (insert (gitconfig-indentation-string)))
      (if was-in-indent
          (back-to-indentation)
        (goto-char (marker-position old-point)))
      (set-marker old-point nil))))

(defun gitconfig-indentation-string ()
  (if indent-tabs-mode "\t" (make-string tab-width ?\ )))

(defvar gitconfig-mode-syntax-table
  (let ((table (make-syntax-table conf-unix-mode-syntax-table)))
    ;; ; is a comment in .gitconfig
    (modify-syntax-entry ?\; "<" table)
    ;; ' is not used for string quoting
    (modify-syntax-entry ?\' "." table)
    table)
  "Syntax table to use in .gitconfig buffers.")

(defvar gitconfig-mode-font-lock-keywords
  `(
    ;; Highlight section and subsection gitconfig headers, and override
    ;; syntactic fontification in these.
    (,(rx line-start (zero-or-more (syntax whitespace))
          "[" symbol-start
          (group (one-or-more (or (syntax word) (syntax symbol))))
          symbol-end
          (optional (one-or-more (syntax whitespace))
                    (group (syntax string-quote)
                           (minimal-match (one-or-more not-newline))
                           (syntax string-quote)))
          "]" (zero-or-more not-newline) line-end)
     (1 'font-lock-type-face t nil)
     (2 'font-lock-function-name-face t t))
    (,(rx line-start (zero-or-more (syntax whitespace)) symbol-start
          (group alphanumeric
                 (zero-or-more (or (syntax word) (syntax symbol))))
          symbol-end (zero-or-more (syntax whitespace))
          (optional "=" (zero-or-more not-newline)) line-end)
     (1 'font-lock-variable-name-face))
    ;; Highlight booleans and numbers
    (,(rx "="
          (zero-or-more (syntax whitespace)) word-start
          (group (or "yes" "no" "true" "false" "on" "off"))
          word-end (zero-or-more (syntax whitespace)) line-end)
     (1 'font-lock-keyword-face))
    (,(rx "="
          (zero-or-more (syntax whitespace)) word-start
          (group (one-or-more digit))
          word-end (zero-or-more (syntax whitespace)) line-end)
     (1 'font-lock-constant-face))))

;;;###autoload
(define-derived-mode gitconfig-mode conf-unix-mode "Gitconfig"
  "A major mode for editing .gitconfig files."
  ;; .gitconfig is indented with tabs only
  (conf-mode-initialize "#" gitconfig-mode-font-lock-keywords)
  (setq indent-tabs-mode t)
  (set (make-local-variable 'indent-line-function)
       'gitconfig-indent-line))

;;;###autoload
(dolist (pattern '("/\\.gitconfig\\'"      "/\\.git/config\\'"
                   "/modules/.*/config\\'" "/git/config\\'"
                   "/\\.gitmodules\\'"     "/etc/gitconfig\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))
(defvar gitignore-mode-font-lock-keywords
  '(("^\\s<.*$"   . font-lock-comment-face)
    ("^!"         . font-lock-negation-char-face) ; Negative pattern
    ("/"          . font-lock-constant-face)      ; Directory separators
    ("[*?]"       . font-lock-keyword-face)       ; Glob patterns
    ("\\[.+?\\]"  . font-lock-keyword-face)))     ; Ranged glob patterns

;;;###autoload
(define-derived-mode gitignore-mode conf-unix-mode "Gitignore"
  "A major mode for editing .gitignore files."
  (conf-mode-initialize "#")
  ;; Disable syntactic font locking, because comments are only valid at
  ;; beginning of line.
  (setq font-lock-defaults '(gitignore-mode-font-lock-keywords t t))
  (set (make-local-variable 'conf-assignment-sign) nil))

;;;###autoload
(dolist (pattern (list "/\\.gitignore\\'"
                       "/info/exclude\\'"
                       "/git/ignore\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

(defgroup gitattributes-mode nil
  "Edit .gitattributes files."
  :link '(url-link "https://github.com/magit/git-modes")
  :prefix "gitattributes-mode-"
  :group 'tools)

(defcustom gitattributes-mode-enable-eldoc t
  "Enable `eldoc-mode' when loading `gitattributes-mode'.
This provides documentation for known variables in the echo area.
Alternatively add `turn-on-eldoc-mode' to the mode hook."
  :type 'boolean
  :group 'gitattributes-mode)

(defcustom gitattributes-mode-man-function #'man
  "Function to open the gitattributes(5) manpage."
  :type '(choice (const :tag "Man" #'man)
                 (const :tag "Woman" #'woman)
                 (function :tag "Function"))
  :group 'gitattributes-mode)

(defun gitattributes-mode-help ()
  "Open the gitattributes(5) manpage using `gitattributes-mode-man-function'."
  (interactive)
  (funcall gitattributes-mode-man-function "gitattributes"))

(defconst gitattributes-mode-attributes
  '(("text"
     "This attribute enables and controls end-of-line normalization."
     (t "auto"))
    ("eol"
     "This attribute sets a specific line-ending style to be used in the \
working directory."
     ("crlf" "lf"))
    ("ident" "Handle $Id$." t)
    ("filter"
     "A filter attribute can be set to a string value that names a filter \
driver specified in the configuration."
     string)
    ("diff"
     "The attribute diff affects how Git generates diffs for particular files."
     (t string "ada" "bibtex" "cpp" "csharp" "fortran" "html" "java"
        "matlab" "objc" "pascal" "perl" "php" "python" "ruby" "tex"))
    ("merge"
     "The attribute merge affects how three versions of a file are merged."
     (t string "text" "binary" "union"))
    ("conflict-marker-size"
     "This attribute controls the length of conflict markers left in the work \
tree file during a conflicted merge."
     (number))
    ("whitespace"
     "The core.whitespace configuration variable allows you to define what \
diff and apply should consider whitespace errors for all paths in the project."
     (t string))
    ("export-ignore"
     "Files and directories with the attribute export-ignore won’t be added to \
archive files."
     t)
    ("export-subst"
     "If the attribute export-subst is set for a file then Git will expand \
several placeholders when adding this file to an archive."
     t)
    ("delta"
     "Delta compression will not be attempted for blobs for paths with the \
attribute delta set to false."
     t)
    ("encoding"
     "The encoding used for the file in GUI Tools (e.g., gitk(1) and \
git-gui(1))."
     string))
  "List of known attributes.
Format (NAME DOC ALLOWED-STATES).
NAME should be the name as a string.
DOC should be a short doc-string.
ALLOWED-STATE should be a list or single symbol or string of allowed values.
t means the attribute can be Set or Unset.  `string' means the symbol value
can be any string and `number' means the value should be a number.")

(defun gitattributes-mode-eldoc (&optional no-state)
  "Support for `eldoc-mode'.
If NO-STATE is non-nil then do not print state."
  (let (entry)
    (when (and (thing-at-point-looking-at "\\s-+\\(-\\|!\\)?\\(\\(?:\\sw-?\\)+\\)\\(=\\)?")
               (setq entry (assoc-string (match-string 2)
                                         gitattributes-mode-attributes)))
      (concat (unless no-state
                (cond
                 ((string= (match-string 1) "-") "[Unset] ")
                 ((string= (match-string 1) "!") "[Unspecified] ")
                 ((string= (match-string 3) "=") "[Set to a value] ")
                 (t "[Set] ")))
              (cadr entry)))))

(defvar gitattributes-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?- "_." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `gitattributes-mode'.")

(defun gitattributes-mode--highlight-1st-field (regexp)
  "Highlight REGEXP in the first field only."
  `(lambda (limit)
     (let ((old-limit limit))
       (save-excursion
         (beginning-of-line)
         (while (and (not (eobp)) (looking-at "^\\s-*$"))
           (forward-line))
         (when (re-search-forward "[[:space:]]" limit 'noerror)
           (setq limit (point))))
       (unless (< limit (point))
         (if (re-search-forward ,regexp limit 'noerror)
             t
           (forward-line)
           (when (< (point) old-limit)
             (gitattributes-mode--highlight-1st-field old-limit)))))))

(defvar gitattributes-mode-font-lock-keywords
  `(("^\\[attr]" . 'font-lock-function-name-face)
    ("\\s-+\\(-\\|!\\)[[:word:]]+" (1 'font-lock-negation-char-face))
    ("\\s-+\\(?:-\\|!\\)?\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)=?"
     (1 'font-lock-variable-name-face))
    ;; Pattern highlight similar to `gitignore-mode-font-lock-keywords'
    (,(gitattributes-mode--highlight-1st-field "/") . 'font-lock-constant-face)
    (,(gitattributes-mode--highlight-1st-field "[*?]") . 'font-lock-keyword-face)
    (,(gitattributes-mode--highlight-1st-field "\\[.+?]") . 'font-lock-keyword-face))
  "Keywords for highlight in `gitattributes-mode'.")

(defun gitattributes-mode-forward-field (&optional arg)
  "Move point ARG fields forward.
If ARG is omitted or nil, move point forward one field."
  (interactive "p")
  (if (< arg 0)
      (gitattributes-mode-backward-field (- arg))
    (dotimes (_ (or arg 1))
      (re-search-forward "\\s-[!-]?\\<" nil 'move))))

(defun gitattributes-mode-backward-field (&optional arg)
  "Move point ARG fields backward.
If ARG is omitted or nil, move point backward one field."
  (interactive "p")
  (if (< arg 0)
      (gitattributes-mode-forward-field (- arg))
    (dotimes (_ (or arg 1))
      (re-search-backward "\\s-[!-]?\\<" nil 'move))))

(defvar gitattributes-mode-map (make-sparse-keymap)
  "Keymap for `gitattributes-mode'.")

(easy-menu-define gitattributes-mode-menu
  gitattributes-mode-map
  "Menu for `gitattributes-mode'."
  '("Git Attributes"
    ["Forward Field" forward-sexp :active t
     :help "Move forward across one field"]
    ["Backward Field" backward-sexp :active t
     :help "Move backward across one field"]
    ["Kill Field Forward" kill-sexp :active t
     :help "Kill field following cursor"]
    ["Kill Field Backward" backward-kill-sexp :active t
     :help "Kill field preceding cursor"]
    "--"
    ["Help" gitattributes-mode-help :active t
     :help "Open gitattributes(5) manpage"]))

;;;###autoload
(define-derived-mode gitattributes-mode text-mode "Gitattributes"
  "A major mode for editing .gitattributes files.
\\{gitattributes-mode-map}"
  :group 'gitattributes-mode
  :syntax-table gitattributes-mode-syntax-table
  (setq font-lock-defaults '(gitattributes-mode-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local eldoc-documentation-function #'gitattributes-mode-eldoc)
  (setq-local forward-sexp-function #'gitattributes-mode-forward-field)
  (when (and gitattributes-mode-enable-eldoc
             (require 'eldoc nil 'noerror))
    (eldoc-mode 1)))

;;;###autoload
(dolist (pattern '("/\\.gitattributes\\'"
                   "/info/attributes\\'"
                   "/git/attributes\\'"))
  (add-to-list 'auto-mode-alist (cons pattern #'gitattributes-mode)))

(provide 'init-git)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; gitignore-mode.el ends here
