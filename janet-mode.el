;;; janet-mode.el --- Defines a major mode for Janet

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

(defgroup janet-mode nil
  "A mode for Janet"
  :prefix "janet-mode-"
  :group 'languages)

(defvar janet-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Comments start with a '#' and end with a newline
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; For keywords, make the ':' part of the symbol class
    (modify-syntax-entry ?: "_" table)

    table))

(defconst janet-symbol '(one-or-more (or (syntax word) (syntax symbol)))
  "Regex representation of a Janet symbol.
A Janet symbol is a collection of words or symbol characters as determined by
the syntax table.  This allows us to keep things like '-' in the symbol part of
the syntax table, so `forward-word' works as expected.")

(defconst janet-start-of-statement '(sequence "(" (zero-or-more space))
  "Regex to match the beginning of an Janet statement.")

(defconst janet-function-pattern
  (eval `(rx (or "defn" "defn-") (one-or-more space) (group ,janet-symbol) symbol-end))
  "The regex to identify janet function names.")

(defconst janet-variable-declaration-pattern
  (eval `(rx (or "var") (one-or-more space) (group ,janet-symbol)))
  "The regex to identify variable declarations.")

(defconst janet-keyword-pattern
  (eval `(rx (group symbol-start ":" ,janet-symbol))))

(defconst janet-start-of-sexp '(sequence "(" (zero-or-more space)))

;; Maybe 'try' should be here
(defconst janet-error-pattern (eval `(rx "error")))

(defconst janet-constant-pattern
  (eval `(rx symbol-start (group (or "true" "false" "nil")) symbol-end)))

(defcustom janet-special-forms
  '(
    "defn"
    "def"
    "var"
    "dyn"
    "fn"
    "do"
    "quote"
    "if"
    "splice"
    "while"
    "break"
    "set"
    "quasiquote"
    "unquote"
    "fn"

    ;; Not explicitly listed as special forms, but included for
    ;; symmetry with other lisp-modes
    "defmacro"
    "import"
    "defn-"
    "def-"
    "cond"
    "switch"
    "when"
    "each"
    "let"
    "for"
    "match"
    "try")
  "List of Janet special forms."
  :type 'list
  :group 'janet-mode)

(defconst janet-special-form-pattern
  (let ((builtins (cons 'or janet-special-forms)))
    (eval `(rx ,janet-start-of-sexp (group ,builtins) symbol-end)))
  "The regex to identify builtin Janet special forms.")

(defconst janet-highlights
  `((,janet-special-form-pattern . (1 font-lock-keyword-face))
    (,janet-function-pattern . (1 font-lock-function-name-face))
    (,janet-variable-declaration-pattern . (1 font-lock-variable-name-face))
    (,janet-error-pattern . font-lock-warning-face)
    (,janet-constant-pattern . (1 font-lock-constant-face))

    ;; Builtin seems like a strange choice, but it is used in lisp-mode
    (,janet-keyword-pattern . (1 font-lock-builtin-face))))

(defcustom janet-indent 2
  "The number of spaces to add per indentation level."
  :type 'integer
  :group 'janet-mode)

(defun janet-looking-at-closing-char ()
  "Test whether the char at point is a closing delemeter."
  (eq (char-syntax (char-after (point))) ?\)))

(defun janet-indent-function ()
  "This function is normally the value of 'indent-line-function' in Janet.
The indent is currently calculated via 'syntax-ppss'.  This could
be switched to using SMIE.  This approach also does not attempt to
align s-expressions."
  (save-excursion
    (back-to-indentation)
    (indent-line-to (* janet-indent
                       (- (car (syntax-ppss))
                          (if (janet-looking-at-closing-char)
                              1
                            0)
                          ))))
  (if (< (current-column)
	 (save-excursion
	   (back-to-indentation)
	   (current-column)))
      (back-to-indentation)))

;;;###autoload
(define-derived-mode janet-mode prog-mode "janet"
  "Major mode for the Janet language"
  :syntax-table janet-mode-syntax-table
  (setq-local font-lock-defaults '(janet-highlights))
  (setq-local indent-line-function #'janet-indent-function)
  (setq-local comment-start "#")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.janet\\'" . janet-mode))

(provide 'janet-mode)
;;; janet-mode.el ends here
