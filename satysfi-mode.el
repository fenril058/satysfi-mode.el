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

(require 'outline)
(require 'imenu)
(require 'font-lock)
(require 'compile)

(defgroup satysfi nil
  "Major mode for editing text files in SATySFi format."
  :prefix "satysfi-"
  :prefix "satysfi-mode-"
  :group 'text)

(defconst satysfi-mode-version "0.1.0"
  "SATySFi mode version number.")

;;; Faces
(defgroup satysfi-faces nil
  "Faces used in SATySFi mode."
  :prefix "satysfi-"
  :group 'satysfi
  :group 'faces)

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

(defcustom satysfi-noindent-commands '("emph" "footnote")
  "Commands for which `satysfi-indent-basic' should not be used."
  :type '(repeat string)
  :safe (lambda (x) (not (memq nil (mapcar #'stringp x))))
  :group 'satysfi)

(defcustom satysfi-indent-within-escaped-parens nil
  "Non-nil means add extra indent to text within escaped parens.
When this is non-nil, text within matching pairs of escaped
parens is indented at the column following the open paren.  The
default value does not add any extra indent thus providing."
  :type 'boolean
  :group 'satysfi)


;;; Syntax support
(defvar satysfi-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?% "<" st)     ; %
    (modify-syntax-entry ?< "(>" st)    ; <
    (modify-syntax-entry ?> ")<" st)    ; >
    (modify-syntax-entry ?\" "." st)    ; "
    (modify-syntax-entry ?\\ "\\" st)   ; \
    (modify-syntax-entry ?\r ">" st)    ; \r
    (modify-syntax-entry ?\n ">" st)    ; \n
    (modify-syntax-entry ?\f ">" st)    ; ^L
    (modify-syntax-entry ?\C-@ "w" st)  ; ^@
    (modify-syntax-entry ?@ "_" st)     ; @
    (modify-syntax-entry ?* "_" st)     ; *
    (modify-syntax-entry ?\t " " st)    ; Tab
    (modify-syntax-entry ?& "." st)     ; &
    (modify-syntax-entry ?_ "." st)     ; _
    (modify-syntax-entry ?^ "." st)     ; ^
    st)
  "Syntax table used while in SATySFi mode.")

(eval-and-compile
  (defconst satysfi-mode-syntax-propertize-rules
   (syntax-propertize-precompile-rules
    ("${\\(.+?\\)}"
     (1 "w")))
   "Syntax-propertize rules for satysfi-mode"))

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

(defvar satisfi-inline-command-regexp
  "\\(\\\\\\(?:\\\\\\\\\\)*\\([a-zA-Z0-9\\-]+\\.\\)*[a-zA-Z0-9\\-]+\\)\\>")

(defvar satysfi-block-command-regexp
  "\\(\\+\\([a-zA-Z0-9\\-]+\\.\\)*[a-zA-Z0-9\\-]+\\)\\>")

(defvar satysfi-var-in-string-regexp
  "\\(@[a-z][0-9A-Za-z\\-]*\\)\\>")

(defvar satysfi-escaped-character-regexp
  "\\(\\\\\\(?:@\\|`\\|\\*\\| \\|%\\||\\|;\\|{\\|}\\|<\\|>\\|\\$\\|#\\|\\\\\\)\\)")

(defvar satysfi-font-lock-list
  `((,satysfi-reserved-word-regexp
     (0 'font-lock-keyword-face))
    (,satisfi-inline-command-regexp
     (1 'satysfi-inline-command-face t))
    (,satysfi-block-command-regexp
     (1 'satysfi-block-command-face t))
    (,satysfi-var-in-string-regexp
     (1 'satysfi-var-in-string-face t))
    (,satysfi-escaped-character-regexp
     (1 'satysfi-escaped-character-face t))
    )
  "Default expressions to highlight in SATySFi mode.

Any words in `satysfi-reserved-word-list' get highlighted in this
simple implementation.  Of course, This is NOT the desired behavior.
To highlight only appropriate keywords is a future task.")


;;; Outline support
(defvar satysfi-section-alist
  '(("+chapter" . 1)
    ("+section" . 2)
    ("+subsection" . 3)
    ("+subsubsection" . 4))
  "Assosiation list which elements is the names of the sections used by SATySFi.")

(defvar satysfi-outline-regexp
  (concat (regexp-opt (cl-mapcar #'car satysfi-section-alist) t) "{\\(.+?[^\\]\\)}")
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


;;; Indent
(defvar satysfi-indent-allhanging t)
(defvar satysfi-indent-arg 4)
(defvar satysfi-indent-basic 2)
(defvar satysfi-indent-item satysfi-indent-basic)
(defvar satysfi-handle-escaped-parens t)

(defvar satysfi-indent-syntax-table
  (let ((st (make-syntax-table satysfi-mode-syntax-table)))
    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?\( "." st)
    (modify-syntax-entry ?\) "." st)
    st)
  "Syntax table used while computing indentation.

This variable was originally derived from `tex-latex-indent-syntax-table'.")

(defun satysfi-indent (&optional _arg)
  "This function was originally derived from `latex-indent'."
  (if (and (eq (get-text-property (if (and (eobp) (bolp))
                                      (max (point-min) (1- (point)))
                                    (line-beginning-position))
                                  'face)
	           'tex-verbatim))
      'noindent
    (with-syntax-table satysfi-indent-syntax-table
      ;; TODO: Rather than ignore $, we should try to be more clever about it.
      (let ((indent
	         (save-excursion
	           (beginning-of-line)
	           (satysfi-find-indent))))
	    (if (< indent 0) (setq indent 0))
	    (if (<= (current-column) (current-indentation))
	        (indent-line-to indent)
	      (save-excursion (indent-line-to indent)))))))

(defun satysfi-find-indent (&optional virtual)
  "Find the proper indentation of text after point.
VIRTUAL if non-nil indicates that we're only trying to find the
indentation in order to determine the indentation of something
else.  There might be text before point.

This was originally derived from `latex-find-indent'."
  (let ((satysfi-handle-escaped-parens
         satysfi-indent-within-escaped-parens))
    (save-excursion
      (skip-chars-forward " \t")
      (or
       ;; Stick the first line at column 0.
       (and (= (point-min) (line-beginning-position)) 0)
       ;; Trust the current indentation, if such info is applicable.
       (and virtual (save-excursion (skip-chars-backward " \t&") (bolp))
	        (current-column))

       ;; Put leading close-paren where the matching open paren would be.
       (let (escaped)
	     (and (or (eq (satysfi-syntax-after) ?\))
		          ;; Try to handle escaped close parens but keep
		          ;; original position if it doesn't work out.
		          (and satysfi-handle-escaped-parens
		               (setq escaped (looking-at "\\\\\\([])}]\\)"))))
	          (ignore-errors
	            (save-excursion
		          (when escaped
		            (goto-char (match-beginning 1)))
		          (satysfi-skip-close-parens)
		          (satysfi-backward-sexp-1)
		          (satysfi-find-indent 'virtual)))))
       ;; Default (maybe an argument)
       (let ((pos (point))
	         (indent 0)
	         up-list-pos)
	     ;; Find the previous point which determines our current indentation.
	     (condition-case err
	         (progn
	           (satysfi-backward-sexp-1)
	           (while (> (current-column) (current-indentation))
		         (satysfi-backward-sexp-1)))
	       (scan-error
	        (setq up-list-pos (nth 2 err))))
	     (cond
	      ((= (point-min) pos) 0) ; We're really just indenting the first line.
	      ((integerp up-list-pos)
	       ;; Have to indent relative to the open-paren.
	       (goto-char up-list-pos)
	       (if (and (not satysfi-indent-allhanging)
		            (save-excursion
		              ;; Make sure we're an argument to a macro and
		              ;; that the macro is at the beginning of a line.
		              (condition-case nil
			              (progn
			                (while (eq (char-syntax (char-after)) ?\()
			                  (forward-sexp -1))
			                (and (eq (char-syntax (char-after)) ?/)
				                 (progn (skip-chars-backward " \t&")
					                    (bolp))))
			            (scan-error nil)))
		            (> pos (progn (satysfi-down-list)
				                  (forward-comment (point-max))
				                  (point))))
	           ;; Align with the first element after the open-paren.
	           (current-column)
             ;; We're the first element after a hanging brace.
	         (goto-char up-list-pos)
	         (+ (if (if (eq (char-after) ?\{)
                        (save-excursion
                          (skip-chars-backward " \t")
                          (let ((end (point)))
                            (skip-chars-backward "a-zA-Z")
                            (and (eq (char-before) ?\\)
                                 (member (buffer-substring (point) end)
                                         satysfi-noindent-commands))))
                      )
		            0
                  satysfi-indent-basic)
		        indent (satysfi-find-indent 'virtual))))
          ;; We're now at the "beginning" of a line.
	      ((not (and (not virtual) (eq (char-after) ?\\)))
	       ;; Nothing particular here: just keep the same indentation.
	       (+ indent (current-column)))
	      (t
	       (let ((col (current-column)))
	         (if (or (not (eq (char-syntax (or (char-after pos) ?\s)) ?\())
		             ;; Can't be an arg if there's an empty line in between.
		             (save-excursion (re-search-forward "^[ \t]*$" pos t)))
		         ;; If the first char was not an open-paren, there's
		         ;; a risk that this is really not an argument to the
		         ;; macro at all.
		         (+ indent col)
	           (forward-sexp 1)
	           (if (< (line-end-position)
		              (save-excursion (forward-comment (point-max))
				                      (point)))
		           ;; we're indenting the first argument.
		           (min (current-column) (+ satysfi-indent-arg col))
		         (skip-syntax-forward " ")
		         (current-column)))))))))))

(defun satysfi-down-list ()
  (down-list 1))

(defun satysfi-syntax-after ()
  (char-syntax (char-after)))

(defun satysfi-skip-close-parens ()
  (skip-syntax-forward " )"))

(defmacro satysfi-search-noncomment (&rest body)
  "Execute BODY as long as it return non-nil and point is in a comment.
Return the value returned by the last execution of BODY.

This was originally derived from `tex-search-noncomment'."
  (declare (debug t))
  (let ((res-sym (make-symbol "result")))
    `(let (,res-sym)
       (while
	   (and (setq ,res-sym (progn ,@body))
		(save-excursion (skip-chars-backward "^\n%") (not (bolp)))))
       ,res-sym)))

(defun satysfi-backward-sexp-1 ()
  (backward-sexp 1))

;;; Commands
(defun satysfi-typeset ()
  "Run command to typeset the current document."
  (interactive)
  (let ((compile-command
         (cond
          ((file-exists-p "Makefile") "make")
          (t (concat satysfi-typeset-command " " (buffer-file-name))))
         ))
    (call-interactively 'compile))) ; 要改善

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
  (setq-local syntax-propertize-function
    (syntax-propertize-rules satysfi-mode-syntax-propertize-rules))

  ;; Outline
  (setq-local add-log-current-defun-function #'satysfi-current-defun-name)
  (setq-local outline-heading-alist satysfi-section-alist)
  (setq-local outline-regexp satysfi-outline-regexp)
  (setq-local outline-level  #'satysfi-outline-level)

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
              '((satysfi-font-lock-list)
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
