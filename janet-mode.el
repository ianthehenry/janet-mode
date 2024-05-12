;;; janet-mode.el --- Defines a major mode for Janet -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Adam Schwalm

;; Author: Adam Schwalm <adamschwalm@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/ALSchwalm/janet-mode
;; Package-Requires: ((emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines a major mode for the janet language: https://janet-lang.org/

;;; Code:

(require 'cl-lib)

(defgroup janet nil
  "A mode for Janet"
  :group 'languages)

(defvar janet-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Comments start with a '#' and end with a newline
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; For keywords, make the ':' part of the symbol class
    (modify-syntax-entry ?: "_" table)

    ;; Backtick is a string delimiter
    (modify-syntax-entry ?` "\"" table)

    ;; Other chars that are allowed in symbols
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?@ "_" table)

    table))

(defconst janet-symbol '(one-or-more (or (syntax word) (syntax symbol)))
  "Regex representation of a Janet symbol.
A Janet symbol is a collection of words or symbol characters as determined by
the syntax table.  This allows us to keep things like '-' in the symbol part of
the syntax table, so `forward-word' works as expected.")

(defconst janet-start-of-sexp '("(" (zero-or-more (or space "\n"))))

(defconst janet-macro-decl-forms '("defmacro" "defmacro-"))

(defconst janet-normal-function-decl-forms '("defn" "defn-"))

(defconst janet-function-decl-forms
  `(,@janet-normal-function-decl-forms ,@janet-macro-decl-forms "varfn" "fn"))

(defconst janet-function-pattern
  (rx-to-string `(sequence ,@janet-start-of-sexp
                  (or ,@janet-function-decl-forms)
                  (one-or-more space) (group ,janet-symbol) symbol-end))
  "The regex to identify janet function names.")

(defconst janet-var-decl-forms
  '("var" "var-" "def" "def-" "defglobal" "varglobal" "default" "dyn"))

(defconst janet-variable-declaration-pattern
  (rx-to-string `(sequence ,@janet-start-of-sexp
                  (or ,@janet-var-decl-forms)
                  (one-or-more space) (group ,janet-symbol)))
  "The regex to identify variable declarations.")

(defconst janet-keyword-pattern
  (rx-to-string `(group symbol-start ":" ,janet-symbol)))

(defconst janet-error-pattern
  (rx-to-string `(sequence ,@janet-start-of-sexp (group symbol-start "error" symbol-end))))

(defconst janet-constant-pattern
  (rx-to-string `(group symbol-start (group (or "true" "false" "nil")) symbol-end)))

(defconst janet-imenu-generic-expression
  `((nil
     ,(rx-to-string `(sequence line-start ,@janet-start-of-sexp
                               (or ,@janet-normal-function-decl-forms)
                               (one-or-more space)
                               (group ,janet-symbol)))
     1)
    ("Variables"
     ,(rx-to-string `(sequence line-start ,@janet-start-of-sexp
                               (or ,@janet-var-decl-forms)
                               (one-or-more space)
                               (group ,janet-symbol)))
     1)
    ("Macros"
     ,(rx-to-string `(sequence line-start ,@janet-start-of-sexp
                               (or ,@janet-macro-decl-forms)
                               (one-or-more space)
                               (group ,janet-symbol)))
     1)))

(defcustom janet-special-forms
  `(
    ;; Not all explicitly special forms, but included for
    ;; symmetry with other lisp-modes

    "->"
    "->>"
    "-?>"
    "-?>>"
    "as->"
    "as?->"
    "break"
    "cond"
    "coro"
    "do"
    "each"
    "fn"
    "for"
    "generate"
    "if"
    "if-let"
    "if-not"
    "import"
    "let"
    "loop"
    "match"
    "quasiquote"
    "quote"
    "require"
    "seq"
    "set"
    "setdyn"
    "splice"
    "try"
    "unless"
    "unquote"
    "var"
    "when"
    "when-let"
    "while"
    "with"
    "with-dyns"
    "with-syms"
    "with-vars"

    ,@janet-var-decl-forms
    ,@janet-function-decl-forms)
  "List of Janet special forms."
  :type '(repeat string)
  :group 'janet)

(defconst janet-special-form-pattern
  (let ((builtins (cons 'or janet-special-forms)))
    (rx-to-string `(sequence ,@janet-start-of-sexp (group ,builtins) symbol-end)))
  "The regex to identify builtin Janet special forms.")

(defconst janet-highlights
  `((,janet-special-form-pattern . (1 font-lock-keyword-face))
    (,janet-function-pattern . (1 font-lock-function-name-face))
    (,janet-variable-declaration-pattern . (1 font-lock-variable-name-face))
    (,janet-error-pattern . (1 font-lock-warning-face))
    (,janet-constant-pattern . (1 font-lock-constant-face))
    (,janet-keyword-pattern . (1 font-lock-builtin-face))))

;; The janet-mode indentation logic borrows heavily from
;; racket-mode and clojure-mode

(defcustom janet-indent 2
  "The number of spaces to add per indentation level."
  :type 'integer
  :group 'janet)

(defun janet--soft-bol ()
  (beginning-of-line)
  (skip-chars-forward " \t"))

(defun janet-indent-line ()
  "Indent current line as Janet code."
  (interactive)
  (pcase (janet--calculate-indent)
    (`()  nil)
    ;; When point is within the leading whitespace, move it past the
    ;; new indentation whitespace. Otherwise preserve its position
    ;; relative to the original text.
    (amount (let ((pos (- (point-max) (point)))
                  (beg (progn (beginning-of-line) (point))))
              (skip-chars-forward " \t")
              (unless (= amount (current-column))
                (delete-region beg (point))
                (indent-to amount))
              (when (< (point) (- (point-max) pos))
                (goto-char (- (point-max) pos)))))))

; there are some contexts where you actually want to start an indent
; relative to the open paren -- that is, where you don't want the soft-bol here.
; the only one i can think of is a let:
;
; (let [x (foo
;           1 2)])
;
;
; I want to indent the arguments to foo relative to foo's opening paren.
; But it's fine to just... not write multi-line lets like this.
(defun janet--simple-indent ()
  (janet--soft-bol)
  (+ (current-column) janet-indent))

; returns nil if the current sexp is empty
(defun janet--start-of-first-child ()
  (ignore-errors (down-list) (forward-sexp) (backward-sexp) t))

(defun janet--calculate-indent ()
  "Calculate the appropriate indentation for the current Janet line."
  (save-excursion
    (beginning-of-line)
    (if (ignore-errors (backward-up-list) t)
        (let ((opener (char-after)))
          (cond
           ((eq opener ?\() (janet--simple-indent))
           ((or (eq opener ?\[) (eq opener ?\{))
            (if (janet--start-of-first-child)
                (current-column)
              (janet--simple-indent)))
           (t (message "other") 0)
           ))
      0)))

;;;###autoload
(define-derived-mode janet-mode prog-mode "janet"
  "Major mode for the Janet language"
  :syntax-table janet-mode-syntax-table
  (setq-local font-lock-defaults '(janet-highlights))
  (setq-local indent-line-function #'janet-indent-line)
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-generic-expression janet-imenu-generic-expression))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.janet\\'" . janet-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("janet" . janet-mode))

(provide 'janet-mode)
;;; janet-mode.el ends here
