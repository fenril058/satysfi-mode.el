;;; satysfi-mode.el --- SATySFi mode commands        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  ril

;; Author: ril <fenril.nh@gmail.com>
;; Keywords: satysfi
;; URL: https://github.com/fenril058/satysfi-mode.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'rx)
(require 'outline)
(require 'imenu)
(require 'font-lock)
(require 'compile)

(defgroup satysfi nil
  "Major mode for editing text files in SATySFi format."
  :prefix "satysfi-"
  :prefix "satysfi-mode-"
  :group 'text)

(defconst satysfi-mode-version "0.1.1"
  "SATySFi mode version number.")


;;; Faces
(defgroup satysfi-faces nil
  "Faces used in SATySFi mode."
  :prefix "satysfi-"
  :group 'satysfi
  :group 'faces)

(defface satysfi-keyword-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used to hilghlight reserved word."
  :group 'satysfi-faces)

(defface satysfi-inline-command-face
  '((((class color) (background dark)) :foreground "#8888ff")
    (t (:inherit font-lock-variable-name-face)))
  "Face used to hilghlight inline command."
  :group 'satysfi-faces)

(defface satysfi-block-command-face
  '((((class color) (background dark)) :foreground "#ff8888")
    (t (:inherit font-lock-function-name-face)))
  "Face used to hilight block command."
  :group 'satysfi-faces)

(defface satysfi-math-command-face
  '((((class color) (background dark)) :foreground "#8888ff")
    (t (:inherit font-lock-variable-name-face)))
  "Face used to hilight math commands."
  :group 'satysfi-faces)

(defface satysfi-var-in-string-face
  '((((class color) (background dark)) :foreground "#44ff88")
    (t (:inherit font-lock-string-face)))
  "Faces used to hilight variable in string."
  :group 'satysfi-faces)

(defface satysfi-escaped-character-face
  '((((class color) (background dark)) :foreground "#cc88ff")
    (t (:inherit font-lock-constant-face)))
  "Faces used to hilight escaped character."
  :group 'satysfi-faces)

(defface satysfi-literal-area
  '((((class color) (background light)) :foreground "#aaaa00")
    (((class color) (background dark)) :foreground "#ffff44"))
  "Faces used to hilight literal area."
  :group 'satysfi-faces)


;;; Customizable Variables
(defcustom satysfi-name-string "SATySFi "
  "Name displayed in modeline."
  :type 'mode-line-format
  :group 'satysfi)

(defcustom satysfi-typeset-command "satysfi -b"
  "Command used by `stysfi-mode-typeset' to typeset."
  :type 'string
  :group 'satysfi)

(defcustom satysfi-view-command "open"
  "Command used by `stysfi-mode-view' to view PDF."
  :type 'string
  :group 'satysfi)

(defcustom satysfi-enable-electric-pair-mode t
  "If non-nil enable `electric-pair-mode'."
  :type 'boolean
  :group 'satysfi)

(defcustom satysfi-enable-whitespace-mode nil
  "If non-nil enable `whitespace-mode'."
  :type 'boolean
  :group 'satysfi)

(defcustom satysfi-enable-which-function-mode t
  "If non-nil enable `which-function-mode'."
  :type 'boolean
  :group 'satysfi)

(defcustom satysfi-imenu-indent-string ". "
  "String to add repeated in front of nested sectional units for Imenu.
An alternative value is \" . \", if you use a font with a narrow period."
  :type 'string
  :group 'satysfi)


;;; Syntax support
(defvar satysfi-mode-syntax-table-plist
  '((?% "<")     ; %   : Comment starters
    (?\n ">")    ; \n  : Comment enders
    (?\r ">")    ; \r  : Comment enders
    (?\f ">")    ; ^L  : Comment enders
    (?\" ".")    ; "   : Punctuation characters
    (?\\ "\\")   ; \   : Escape-syntax characters
    (?\C-@ "w")  ; ^@  : Word constituents
    (?@ "_")     ; @   : Symbol constituents
    (?* "_")     ; *   : Symbol constituents
    (?\t " ")    ; Tab : Whitespace characters
    (?& ".")     ; &   : Punctuation characters
    (?_ ".")     ; _   : Symbol constituents
    (?^ ".")     ; ^   : Punctuation characters
    (?\# "'")    ; #   : Expression prefixes
    )
  )

(defvar satysfi-mode-syntax-table
  (let ((st (make-syntax-table prog-mode-syntax-table)))
    (dolist (plist satysfi-mode-syntax-table-plist st)
      (let ((char (car plist))
            (syntax (nth 1 plist)))
        (modify-syntax-entry char syntax st))))
  "Syntax table used while in `satysfi-mode'.")

(eval-and-compile
  (defconst satysfi-syntax-propertize-rules
   (syntax-propertize-precompile-rules
    ("\\('\\)<"                         ; '<
     (1 "'"))
    ("\\(\\$\\){"                       ; ${
     (1 "'"))
    ("\\(!\\)[(<[{]"                    ; !( or !< or !{
     (1 "'"))
    ;; ("\\(?:\\sw\\|\\s_\\)\\(-+\\)"
    ;;  (1 "_"))
    )
   "Syntax-propertize rules for `satysfi-mode'"))

(defun satysfi-syntax-propertize (beg end)
  "Syntax propertize the region between BEG and END.

Originally from:
https://github.com/hanazuki/satysfi.el"

  (remove-list-of-text-properties beg end '(satysfi-lexing-context))
  (funcall (syntax-propertize-rules satysfi-syntax-propertize-rules) beg end)

  (save-excursion
    (catch 'exit
      (goto-char beg)
      (while (< (point) end)
        (let ((ppss (syntax-ppss)))
          (if (eq (nth 3 ppss) t)
              ;; if this is inside ` string
              (let ((open-marker
                     (save-excursion
                       (goto-char (nth 8 ppss))
                       (looking-at (rx (? ?#) (group (+ ?`))))
                       (match-string 1))))
                (unless (search-forward open-marker end t)
                  (throw 'exit t))
                (unless (looking-at-p "#")
                  (forward-char -1))
                (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "|"))
                (forward-char +1))
            ;; if this is outside ` string
            (while (and
                    (< (point) end)
                    (forward-comment 1)))
            (unless (and
                     (< (point) end)
                     (re-search-forward (rx (any "#`<>([{@")) end t))
              (throw 'exit t))
            (goto-char (match-beginning 0))

            (pcase (following-char)
              ((guard (looking-at-p (rx (? ?#) (+ ?`))))
               (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "|"))
               (goto-char (match-end 0)))
              (?<
               (let ((ctx (car (satysfi--lexing-context (point)))))
                 (cond
                  ((memq ctx '(block inline))
                   (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "(>"))
                   (put-text-property (point) (1+ (point)) 'satysfi-lexing-context (satysfi--lexing-context-transition ctx (point))))
                  ((eq (preceding-char) ?')
                   (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax "(>"))
                   (put-text-property (point) (1+ (point)) 'satysfi-lexing-context (satysfi--lexing-context-transition ctx (point))))))
               (forward-char 1))
              (?>
               (if (eq (car (satysfi--lexing-context (point))) 'block)
                   (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax ")<")))
               (forward-char 1))
              ((or ?\( ?\[ ?\{)
               (let ((ctx (car (satysfi--lexing-context (point)))))
                 (put-text-property (point) (1+ (point)) 'satysfi-lexing-context (satysfi--lexing-context-transition ctx (point))))
               (forward-char 1))
              (?@
               (let ((eol (min end (line-end-position))))
                 (when (re-search-forward (rx ":" (0+ " ")) eol t)
                   (while (re-search-forward (rx (1+ (not (syntax word)))) eol t)
                     (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax "_"))))
                 (goto-char eol)))
              (_
               (forward-char 1)))))))))

(defun satysfi--lexing-context-transition (current-context pos)
  "Given CURRENT-CONTEXT, compute the next lexing context transition at POS.

It was originally from
https://github.com/hanazuki/satysfi.el.

From
https://github.com/gfngfn/SATySFi/blob/master/src/frontend/lexer.mll:

The SATySFi lexer is stateful; the transitions are:
| to \ from |program|block |inline|active |  math  |
|-----------|-------|------|------|-------|--------|
|  program  | (   ) |      |      | (   ) | !(   ) |
|           | (| |) |      |      | (| |) | !(| |) |
|           | [   ] |      |      | [   ] | ![   ] |
|  block    | '<  > | <  > | <  > | <     | !<   > |
|  inline   | {   } | {  } | {  } | {     | !{   } |
|  active   |       | +x ; | \x ; |       |        |
|           |       | #x ; | #x ; |       |        |
|  math     | ${  } |      | ${ } |       | {    } |
Note that the active-block and active-inline transitions are one-way."

  (let ((ch (char-after pos)))
    (pcase current-context
      ('program
       (pcase ch
         (?\( 'program)
         (?\[ 'program)
         (?\< 'block)
         (?\{ (if (eq (char-before pos) ?$) 'math 'inline))))
      ('block
          (pcase ch
            (?\( (if (satysfi--active-p pos) 'program))
            (?\[ (if (satysfi--active-p pos) 'program))
            (?\< 'block)
            (?\{ 'inline)))
      ('inline
        (pcase ch
          (?\( (if (satysfi--active-p pos) 'program))
          (?\[ (if (satysfi--active-p pos) 'program))
          (?\< 'block)
          (?\{ (if (eq (char-before pos) ?$) 'math 'inline))))
      ('math
       (if (eq (char-before pos) ?!)
           (pcase ch
             (?\( 'program)
             (?\[ 'program)
             (?\< 'block)
             (?\{ 'inline))
         (pcase ch
           (?\{ 'math)))))))

(defun satysfi--lexing-context (pos)
  "Return lexing context at POS and the position of enclosing open parenthesis."

  ; find innermost paren with lexing context transition
  (let ((ppss (syntax-ppss pos)))
    (cond
     ((nth 3 ppss) (cons 'string (nth 8 ppss)))
     ((nth 4 ppss) (cons 'comment (nth 8 ppss)))
     (t
      (catch 'exit
        (dolist (p (reverse (nth 9 (syntax-ppss pos))))
          (let ((ctx (get-text-property p 'satysfi-lexing-context)))
            (when ctx
              (throw 'exit (cons ctx p)))))
        (cons 'program 0))))))

(defun satysfi--list-context-p (pos)
  "Return whether POS is in { | }."
  (pcase-let ((`(,ctx . ,pos) (satysfi--lexing-context pos)))
    (if (memq ctx '(inline math))
        (save-excursion
          (goto-char (1+ pos))
          (forward-comment (buffer-size))
          (eq (following-char) ?|)))))

(defun satysfi--active-p (pos &optional skip-block-inline)
  "Return active command if POS is an active position.
Skip preceding block and inline area if SKIP-BLOCK-INLINE."

  (save-excursion
    (goto-char pos)
    (catch 'exit
      (let* ((ppss (syntax-ppss))
             (bos (nth 1 ppss)))
        ;; never active at toplevel, which is in program context
        (unless bos
          (throw 'exit nil))

        (when skip-block-inline
          (forward-comment (- (buffer-size)))
          (while (memq (preceding-char) '(?\> ?\}))
            ;; backtrack to matching paren (unless the closing paren is escaped)
            (let ((ppss0 (syntax-ppss (1- (point)))))
              (if (eq (nth 0 ppss) (nth 0 ppss0))
                  (throw 'exit nil)
                (goto-char (nth 1 ppss0))))))

        (while t
          (forward-comment (- (buffer-size)))
          (cond
           ((memq (preceding-char) '(?\) ?\]))
            ;; backtrack to matching paren (unless the closing paren is escaped)
            (let ((ppss0 (syntax-ppss (1- (point)))))
              (if (eq (nth 0 ppss) (nth 0 ppss0))
                  (throw 'exit nil)
                (goto-char (nth 1 ppss0)))))
           ((looking-back (rx (or "?*" "?:")) 2)
            (forward-char -2))
           (t
            ;; Check if the last token is a command
            (throw 'exit
                   (and
                    (looking-back
                     (rx (any "\\+#") (1+ (or (syntax word) (syntax symbol))))
                     (- (point) bos))
                    (match-string 0))))))))))

(defun satysfi--active-command (pos)
  "Return enclosing command name at POS."
  (let* ((tmp (satysfi--lexing-context pos))
         (ctx (car tmp))
         (pos (cdr tmp)))
    (if (memq ctx '(block inline))
        (satysfi--active-p pos t))))

(defun satysfi-current-context ()
  "Print the lexing context at point (for debugging)."
  (interactive)
  (message "%s" (satysfi--lexing-context (point))))

(defun satysfi-current-activation ()
  "Print active command name at point (for debugging)."
  (interactive)
  (message (satysfi--active-p (point))))

(defun satysfi-current-command ()
  "Print enclosing command name at point (for debugging)."
  (message (satysfi--active-command (point))))


;;; Font-Lock support
(defun satysfi-syntactic-face (state)
  "Return font-lock face for characters with given STATE.
See `font-lock-syntactic-face-function' for details."
  (let ((in-comment (nth 4 state)))
    (cond
     (in-comment font-lock-comment-face)
     (t nil))))

(defvar satysfi-reserved-word-list
  '("let" "let-rec" "let-mutable" "let-inline" "let-block" "let-math" "in" "and"
    "match" "with" "when" "as" "if" "then" "else" "fun"
    "type" "constraint" "val" "direct" "of"
    "module" "struct" "sig" "end"
    "before" "while" "do"
    "controls" "cycle")
  "List of the command names.")

(defvar satysfi-reserved-word-regexp
  (regexp-opt satysfi-reserved-word-list 'words)
  "Regular expression which matches any words in `satysfi-reserved-word-list'.")

(defvar satysfi-inline-command-regexp
  "\\(\\\\\\(?:\\\\\\\\\\)*\\([a-zA-Z0-9\\-]+\\.\\)*[a-zA-Z0-9\\-]+\\)\\>")

(defvar satysfi-block-command-regexp
  "\\(\\+\\([a-zA-Z0-9\\-]+\\.\\)*[a-zA-Z0-9\\-]+\\)\\>")

(defvar satysfi-var-in-string-regexp
  "\\(@[a-z][0-9A-Za-z\\-]*\\)\\>")

(defvar satysfi-escaped-character-regexp
  "\\(\\\\\\(?:@\\|`\\|\\*\\| \\|%\\||\\|;\\|{\\|}\\|<\\|>\\|\\$\\|#\\|\\\\\\)\\)")

(defvar satysfi-header-keywords-regexp
  (rx bol (0+ (syntax whitespace)) (group "@" (or "import" "require" "stage")) ":"))

(defun satysfi--match-contextual-keywords (contexts keywords-regexp)
  "Only in CONTEXTS, find next match of KEYWORDS-REGEXP.

Originally from:
https://github.com/hanazuki/satysfi.el"

  (letrec ((re (symbol-value keywords-regexp))
           (matcher
            (lambda (limit)
              (and
               (re-search-forward re limit t)
               (or
                (memq (save-match-data (car (satysfi--lexing-context (point)))) contexts)
                (funcall matcher limit))))))
    matcher))


(defvar satysfi-font-lock-keywords
  `((,(satysfi--match-contextual-keywords '(program) 'satysfi-reserved-word-regexp)
     1 'satysfi-keyword-face)
    (,(satysfi--match-contextual-keywords '(program) 'satysfi-header-keywords-regexp)
     1 'satysfi-keyword-face)
    (,(satysfi--match-contextual-keywords '(block) 'satysfi-block-command-regexp)
     1 'satysfi-block-command-face)
    (,(satysfi--match-contextual-keywords '(inline) 'satysfi-inline-command-regexp)
     1 'satysfi-inline-command-face)
    (,(satysfi--match-contextual-keywords '(math) 'satysfi-inline-command-regexp)
     1 'satysfi-math-command-face)
    (,(satysfi--match-contextual-keywords '(inline math) 'satysfi-escaped-character-regexp)
     1 'satysfi-escaped-char-face))
  "Font-lock keywords for `satysfi-mode'.")


;;; Outline support
(defvar satysfi-section-alist
  '(("+chapter" . 1)
    ("+section" . 2)
    ("+subsection" . 3)
    ("+subsubsection" . 4))
  "Assosiation list which elements is the names of the sections used by SATySFi.")

(defvar satysfi-outline-regexp
  (concat "^.+?" (regexp-opt (cl-mapcar #'car satysfi-section-alist) t) "{\\(.+?[^\\]\\)}")
  "Regular expression which matches SATySFi sections.")

(defun satysfi-outline-level ()
  "Return the outline level.

This function was originally derived from `latex-outline-level'
from `tex-mode.el'."
  (interactive)
  (if (looking-at satysfi-outline-regexp)
      (1+ (or (cdr (assoc (match-string 1) satysfi-section-alist)) -1))
    1000))

(defun satysfi-current-defun-name ()
  "Return the name of the SATySFi section or chapter at point, or nil.

This function was originally derived from
`tex-current-defun-name' from `tex-mode.el'."
  (save-excursion
    (when (re-search-backward satysfi-outline-regexp nil t)
      (goto-char (match-beginning 0))
      (buffer-substring-no-properties
       (1+ (point))                     ; without initial "+"
       (match-end 0)))))


;;; Imenu support
(defun satysfi-imenu-create-index ()
  "Generate an alist for imenu from a SATySFi buffer."
  (let (menu)
    (goto-char (point-min))
    (while (re-search-forward satysfi-outline-regexp (point-max) t)
      (let ((i (cdr (assoc (match-string 1) satysfi-section-alist))))
        (condition-case nil
            (push
             (cons
              (concat (apply #'concat
					         (make-list i satysfi-imenu-indent-string))
                      (match-string 2))
              (match-beginning 2))
             menu)
          (error nil))))
    (nreverse menu)))


;;; Indentation
(defcustom satysfi-basic-offset 2
  "Amount of basic offset."
  :type 'integer
  :group 'satysfi)

(defconst satysfi-default-offsets-alist
  '((program . +)
    (block . +)
    (inline . +)
    (math . +)
    (list-separator . -)))

(defcustom satysfi-offsets-alist nil
  "Indentation offset customization."
  :type '(alist :key-type symbol
                :value-type (choice
                             (const :tag "Package default" nil)
                             (const :tag "Basic offset" :+)
                             (const :tag "2 * basic offset" :++)
                             (const :tag "-1 * basic offset" :-)
                             (const :tag "-2 * basic offset" :--)
                             (integer :tag "Relative offset")
                             (vector :tag "Absolute offset" integer))
                :options (program
                          block
                          inline
                          math
                          list-separator))
  :group 'satysfi)

(defvar satysfi-find-command-indent-function-alist
  '(("+listing" . satysfi-find-itemize-indent)
    ("\\listing" . satysfi-find-itemize-indent)))

(defun satysfi-get-indentation (symbol &optional current-indentation)
  "Return indentation for syntax SYMBOL.
If the surrounding indent is not given as CURRENT-INDENTATION,
it sets to 0"

  (cl-block indent
    (+ (or current-indentation 0)
       (let ((offset
              (cdr (or (assq symbol satysfi-offsets-alist)
                       (assq symbol satysfi-default-offsets-alist)
                       (error "Unknown syntax symbol: %s" symbol)))))
         (pcase offset
           ('+ satysfi-basic-offset)
           ('++ (* 2 satysfi-basic-offset))
           ('- (- satysfi-basic-offset))
           ('-- (* -2 satysfi-basic-offset))
           ((pred integerp) offset)
           (`[,abs-offset] (cl-return-from indent abs-offset))
           (_ (error "Illegal offset value in satysfi-offsets-alist for %s: %s" symbol offset)))))))

(defun satysfi-indent ()
  "Indent current line as SATySFi code."
  (interactive)
  (let ((indent (satysfi-find-indent (point))))
    (when indent
      (let ((orig-column (current-column))
            (orig-indent (current-indentation)))
        (indent-line-to (max 0 indent))
        (when (< orig-indent orig-column)
          (move-to-column (+ (current-indentation) (- orig-column orig-indent))))))))

(defun satysfi-find-indent (pos)
  "Find indentation level for the line at POS."

  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (pcase (satysfi--lexing-context (point))
      (`(,'string . ,_) (message "!") nil)  ; TODO: what about indentation in multiline string literals?
      (`(,'comment . ,_) nil)  ; this should never happen, though
      (`(,_ . ,paren-pos)
       (let* ((tmp (satysfi-find-base-indent))
              (indent (car tmp))
              (chain (cdr tmp))
              (command (satysfi--active-command (point)))
              (indent-fun
               (or
                (cdr (assoc command satysfi-find-command-indent-function-alist))
                (and (satysfi--list-context-p (point))
                     'satysfi-find-list-indent))))
         (if (and chain indent-fun)
             (save-restriction
               (narrow-to-region (1+ paren-pos) (line-end-position))
               (funcall indent-fun indent))
           indent))))))

(defun satysfi-find-base-indent ()
  "Find indentation level for the line at point."

  (save-excursion
    (back-to-indentation)
    (pcase-let ((`(,ctx . ,open-pos) (satysfi--lexing-context (point))))
      (if (eq open-pos 0)
          ;; no indent for toplevel
          (cons
           (if (fboundp 'prog-first-column) (prog-first-column) 0)
           nil)
        (let ((content-alignment
               (save-excursion
                 (goto-char open-pos)
                 (if (looking-at-p (rx "(|"))
                     (forward-char 2)
                   (forward-char 1))
                 ;; check if content exists after open paren
                 (let ((line (line-number-at-pos)))
                   (forward-comment 1)
                   (if (and
                        (eq (line-number-at-pos) line)
                        (not (eq (current-column) (line-end-position))))
                     (current-column))))))
          (if content-alignment  ; TODO: make vertical alignment configurable?
              (cons content-alignment t)
            (let ((open-indentaion
                   (save-excursion
                     (goto-char open-pos)
                     (current-indentation)))
                  (at-close
                   (save-excursion
                     (or
                      (and (looking-at-p (rx "|)"))
                           (progn (forward-char 2) t))
                      (and (eq (syntax-class (syntax-after (point))) 5)  ; 5 for close parenthesis
                           (get-text-property (nth 1 (syntax-ppss (point))) 'satysfi-lexing-context)
                           (progn (forward-char 1) t))))))
              (if at-close
                  (cons open-indentaion nil)
                (cons (satysfi-get-indentation ctx open-indentaion) t)))))))))

(defun satysfi-find-list-indent (first-column)
  "Find indentation level inside { | } context, when aligned to FIRST-COLUMN."

  (let ((current-column
         (lambda ()
           (if (= (line-number-at-pos (point)) 1)
               (+ (1- first-column) (current-column))
             (current-column)))))
    (save-excursion
      (back-to-indentation)
      (or
       (catch 'exit
         (if (eq (following-char) ?|)
             (while (not (bobp))
               (forward-line -1)
               (back-to-indentation)
               (when (eq (following-char) ?|)
                 (throw 'exit (funcall current-column))))

           (while (not (bobp))
             (forward-line -1)
             (back-to-indentation)
             (unless (eolp)
               (skip-chars-forward "|")
               (skip-syntax-forward "-")
               (throw 'exit (funcall current-column))))))
       (satysfi-get-indentation 'list-separator first-column)))))

(defun satysfi-find-itemize-indent (first-column)
  "Find indentation level inside \\listing context, when aligned to FIRST-COLUMN."

  (let ((current-column
         (lambda ()
           (if (= (line-number-at-pos (point)) 1)
               (+ (1- first-column) (current-column))
             (current-column)))))
    (save-excursion
      (back-to-indentation)
      (or
       (catch 'exit
         (if (looking-at (rx (1+ ?*)))
             (let ((level (- (match-end 0) (match-beginning 0))))
               (while (not (bobp))
                 (forward-line -1)
                 (back-to-indentation)
                 (when (looking-at (rx (1+ ?*)))
                   (let ((l (- (match-end 0) (match-beginning 0))))
                     (cond
                      ((= l level)
                       (throw 'exit (funcall current-column)))
                      ((< l level)
                       (goto-char (match-end 0))
                       (skip-syntax-forward "-")
                       (throw 'exit (funcall current-column))))))))

           (while (not (bobp))
             (forward-line -1)
             (back-to-indentation)
             (unless (eolp)
               (skip-chars-forward "*")
               (skip-syntax-forward "-")
               (throw 'exit (funcall current-column))))))
       first-column))))


;;; Commands
(defun satysfi-typeset ()
  "Run command to typeset the current document."
  (interactive)
  (let ((compile-command
         (cond
          ((file-exists-p "Makefile") "make")
          (t (concat satysfi-typeset-command " " (buffer-file-name))))
         ))
    (call-interactively 'compile)))

(defun satysfi-view ()
  "Run command to view the generated PDF if it exists."
  (interactive)
  (let ((pdf (concat (file-name-sans-extension buffer-file-name) ".pdf")))
    (if (file-exists-p pdf)
        (async-shell-command (format "%s %s\n" satysfi-view-command pdf))
      (message "The PDF not found.")
      )))

(defun satysfi-at-heading-p (&optional _)
  "Non-nil when on a headline."
  (outline-on-heading-p t))

(defun satysfi-outline-cycle (&optional arg)
  "TAB-action and visibility cycling for  SATySFi mode.

If cursor is at heading, then call `outline-cycle' which cycles between
'hide all', 'headings only' and 'show all'.  Otherewise, call
`indent-for-tab-command' with ARG."
  (interactive "P")
  (cond
   ((satysfi-at-heading-p)
    (outline-cycle))
   (t
    (indent-for-tab-command arg))))

(defalias 'satysfi-outline-cycle-buffer 'outline-cycle-buffer)


;;; Keymap
(defvar satysfi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'satysfi-typeset)
    (define-key map "\C-c\C-f" 'satysfi-view)
    ;; (define-key map (kbd "<tab>") 'satysfi-outline-cycle)
    (define-key map (kbd "<backtab>") 'satysfi-outline-cycle-buffer)
    map)
  "Keymap for `satysfi-mode'.")


;;; Mode Definitions
;;;###autoload
(define-derived-mode satysfi-mode outline-mode
  satysfi-name-string
  "Major mode for editing SATySFi formated text."

  ;; Use tab indentation
  (setq-local indent-tabs-mode 1)
  (setq-local tab-width 4)

  ;; Regexp isearch should accept newline and formfeed as whitespace.
  (setq-local search-whitespace-regexp "[ \t\r\n\f]+")

  ;; Syntax
  (setq-local syntax-propertize-function #'satysfi-syntax-propertize)

  ;; Outline
  (setq-local outline-heading-alist satysfi-section-alist)
  (setq-local outline-regexp satysfi-outline-regexp)
  (setq-local outline-level  #'satysfi-outline-level)

  ;; Which function
  (setq-local add-log-current-defun-function #'satysfi-current-defun-name)

  ;; Imenu
  (setq-local imenu-create-index-function #'satysfi-imenu-create-index)

  ;; Indent
  (setq-local indent-line-function #'satysfi-indent)

  ;; Comments
  (setq-local comment-start "%")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-style 'plain)

  ;; Font-lock.
  (setq-local font-lock-defaults
              '(satysfi-font-lock-keywords
    	        nil nil nil nil
                (font-lock-multiline . t)
    	        (font-lock-mark-block-function . mark-paragraph)
    	        (font-lock-syntactic-face-function . satysfi-syntactic-face)
                ))

  ;; Commands
  (setq-local compile-command satysfi-typeset-command)

  ;; keymap
  (use-local-map satysfi-mode-map)

  ;; Misc
  (when satysfi-enable-electric-pair-mode
    (electric-pair-mode 1))
  (when satysfi-enable-whitespace-mode
    (whitespace-mode 1))
  (when satysfi-enable-which-function-mode
    (which-function-mode 1))

  ;; hook
  (run-hooks 'satysfi-mode-hook))


;; Associate .saty files with satysfi-mode
;;;###autoload
(setq auto-mode-alist (append '(("\\.saty$" . satysfi-mode)) auto-mode-alist))

(provide 'satysfi-mode)
;;; satysfi-mode.el ends here
