;;; caredit-core.el --- Core functionality of Caredit

;; Copyright (C) 2016 Chris Gregory czipperz@gmail.com

;; This file is part of Caredit.
;;
;; Caredit is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Caredit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Caredit.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Chris Gregory "czipperz"
;; Email: czipperz@gmail.com
;; Version: 0.0.1

;;; Commentary:
;; Extracting this into a seperate file allows dependency management
;; that doesn't loop.
;;
;; If this was in `caredit.el', files that needed that functionality
;; would depend on Caredit, which depends on them.

;;; Code:
;;; Consts:
(defconst caredit-version "0.0.1")

;; Declaring the variables at compilation removes warnings.
(eval-when-compile
  (defvar caredit--open-chars)
  (defvar caredit--close-chars))

(setq caredit--open-chars (list ?\( ?\[ ?\{))
(setq caredit--close-chars (list ?\) ?\] ?\}))

(require 'caredit-macros)


;;; Utility functions:
(defun caredit--list-level ()
  "Get the number of times `backward-up-list' works."
  (nth 0 (syntax-ppss)))

(defun caredit--forward-over-whitespace ()
  "Move forward while on whitespace."
  (while (and (not (eobp)) (caredit--whitespace-p (char-after)))
    (forward-char))
  (point))

(defun caredit--backward-over-whitespace ()
  "Move backward while after whitespace."
  (while (and (not (bobp)) (caredit--whitespace-p (char-before)))
    (backward-char))
  (point))


;;; Predicates:
(defun caredit--whitespace-p (ch)
  "Test if CH is whitespace."
  (or (= ch ? )
      (= ch ?\t)
      (= ch ?\n)))

(defun caredit--in-string-p ()
  "Test if point is in a string.

\"asd|f\"  =>  t
\"asdf|\"  =>  t
\"asdf\"|  =>  nil
|\"asdf\"  =>  nil"
  (= ?\" (or (nth 3 (syntax-ppss)) 0)))

(defun caredit--in-comment-p ()
  "Test if point is in a comment.

//| asdf  =>  t
// a|sdf  =>  t
/|/ asdf  =>  nil"
  (if (nth 4 (syntax-ppss)) t nil))

(defun caredit--in-char-p ()
  "Test if point is in a character literal.

'|a'  =>  t
'a|'  =>  t
|'a'  =>  nil
'a'|  =>  nil"
  (= ?\' (or (nth 3 (syntax-ppss)) 0)))


;;; Define functions for manipulating default pairs:
;; This design of binding functions originated in Paredit.
(eval-and-compile
  (defun caredit--conc-name (&rest strings)
    "Concatenate STRINGS and tern them into an atom."
    (intern (apply 'concat strings))))

(defun caredit-get-current-statement ()
  "Return a pair of the beginning of the current statement and the end."
  (save-excursion
    (let (b e)
      ;; There is a possibility one fails, so call in both orders.  In
      ;; this order so goes forward over spaces first before going
      ;; backwards.
      (caredit--orelse
          (progn (setq e (caredit-end-of-balanced-statement))
                 (setq b (caredit-beginning-of-balanced-statement)))
        (progn (setq b (caredit-beginning-of-balanced-statement))
               (setq e (caredit-end-of-balanced-statement))))
      (cons b e))))

(defmacro caredit--define-wrap (open close name)
  "Define function caredit-wrap-NAME to wrap the following sexp in OPEN and CLOSE."
  `(defun ,(caredit--conc-name "caredit-wrap-" name) ()
     ,(concat "Wrap the following S-expression.

Inserts " (list open) " before and " (list close) " after.

|aaa;  =>  " (list open) "|aaa;" (list close))
     (interactive)
     (let ((b (point)) e)
       (insert ,open)
       (save-excursion
         ;; use `caredit-forward-sexp'?
         (forward-sexp)
         (insert ,close)
         (setq e (point)))
       (indent-region b e))))

(defmacro caredit--define-pair (open close name)
  "Define functions for manipulting OPEN and CLOSE pairs appending NAME."
  `(progn
     (defun ,(caredit--conc-name "caredit-open-" name) ()
       ,(concat "Insert a balanced " name " pair.
If in a string or a comment, insert a single " name ".
If in a character literal, do nothing.  This prevents changing what was
  in the character literal to a meaningful delimiter unintentionally.")
       (interactive)
       (insert ,open)
       (unless (or (caredit--in-string-p)
                   (caredit--in-char-p)
                   (caredit--in-comment-p))
         (insert ,close)
         (backward-char)))

     (defun ,(caredit--conc-name "caredit-close-" name) ()
       ,(concat "Move past one closing delimiter and reindent.
\(Agnostic to the specific closing delimiter.)
If in a string or comment, insert a single closing " name ".")
       (interactive)
       ;; this is never called with CLOSE as ?\' or ?\" because it is
       ;; handled by caredit-open-NAME
       (if (= (char-after) ,close)
           (forward-char)
         (insert ,close)))

     (caredit--define-wrap ,open ,close ,name)))

(defmacro caredit--define-double-pair (open name)
  "Defun caredit-open-NAME to correctly insert OPEN in balanced pairs."
  (let ((pred (caredit--conc-name "caredit--in-" name "-p")))
    `(progn
       (defun ,(caredit--conc-name "caredit-open-" name) ()
         ,(concat "Insert a balanced " name " pair.
If in a string or a comment, insert a single " name ".
If in a character literal, do nothing.  This prevents changing what was
  in the character literal to a meaningful delimiter unintentionally.")
         (interactive)
         (if (,pred)
             ;; in NAME literal and want to insert OPEN
             ;; "|"  =>  "\"|"
             ;; "|  "  =>  "\"|  "
             ;; "  |"  =>  "  "|
             (let ((prev-in
                    (save-excursion
                      (backward-char)
                      (,pred)))
                   (next-in
                    (save-excursion
                      (forward-char)
                      (,pred))))
               (if (or (not prev-in)
                       next-in)
                   ;; escape it as in a NAME literal
                   (insert "\\" ,open)
                 (forward-char)))
           ;; |)  =>  '|')  when (= ,open ?')
           (insert ,open ,open)
           (backward-char)))

       (caredit--define-wrap ,open ,open ,name))))

;;; Define pair functions for opening, closing, wrapping.
(caredit--define-pair ?\( ?\) "paren")
(caredit--define-pair ?\[ ?\] "square")
(caredit--define-pair ?\{ ?\} "curly")
(caredit--define-pair ?\< ?\> "angle")
(caredit--define-double-pair ?\" "string")
(caredit--define-double-pair ?\' "char")


;;; Sexp control:
;; (defun caredit-forward-sexp ()
;;   "Go to next AST tree."
;;   (interactive)
;;   ;; go past whitespace
;;   (caredit--forward-over-whitespace)
;;   (cond ((= ?\" (char-after))
;;          ;; go forward over double quote pair
;;          (forward-char)
;;          (while (caredit--in-string-p)
;;            (forward-char)))
;;         ((= ?\' (char-after))
;;          ;; go forward over single quote pair
;;          (forward-char)
;;          (while (caredit--in-char-p)
;;            (forward-char)))
;;         (t
;;          ;; go forward over `f(a, b)'
;;          (forward-sexp)
;;          (while (member (char-after) caredit--open-chars)
;;            (forward-sexp))))
;;   (point))

;; (defun caredit--backward-sexp ()
;;   "Go to previous AST tree."
;;   (interactive)
;;   ;; go past whitespace
;;   (caredit--backward-over-whitespace)
;;   (cond ((= ?\" (char-before))
;;          (backward-char)
;;          (while (caredit--in-string-p)
;;            (backward-char)))
;;         ((= ?\' (char-before))
;;          (backward-char)
;;          (while (caredit--in-string-p)
;;            (backward-char)))
;;         (t
;;          (let ((prev-is-statement
;;                 (save-excursion
;;                   (c-beginning-of-statement 2)
;;                   (let ((is-brace
;;                          (progn
;;                            (c-end-of-statement 1)
;;                            (= ?\{ (char-after)))))
;;                     ;; this will be { if in first statement in block
;;                     (= ?\; (char-before)))))))
;;          (backward-sexp)
;;          (while (member (char-before) caredit--close-chars)
;;            (backward-sexp)))))


;;; Slurp:
(defun caredit--slurp-forward-string ()
  "Slurp next expression into string."
  (save-excursion
    (caredit--assert (caredit--in-string-p)
                     (concat
                      "Must be in string for"
                      " `caredit--slurp-forward-string' to work."))
    ;; get out of string
    (while (caredit--in-string-p)
      (forward-char))
    ;; "|.  Now delete quotes, go forward sexp, then insert quote.
    (let ((ch (char-before))
          (new-pt (save-excursion (caredit-forward-sexp))))
      (delete-char -1)
      (goto-char new-pt)
      (insert ch))))

(defun caredit--slurp-forward-statement ()
  "Slurp statement into braces.

"
  (save-excursion
    (caredit--dowhile (/= ?\} (char-before))
      (up-list))
    ;; }|
    (let (b e reg)
      (save-excursion
        ;; don't slurp dangling elses/dangling do/while
        (when (or (caredit--before-keyword "else")
                  (caredit--after-do-brackets))
          (caredit-end-of-balanced-statement))
        ;; }| a;
        (setq b (point))
        (caredit-end-of-balanced-statement)
        ;; } a;|
        (setq e (point)))
      (setq reg (buffer-substring-no-properties b e))
      (delete-region b e)
      ;; }|
      (backward-char)
      (caredit--backward-over-whitespace)
      (insert reg)
      (indent-region (point) (+ (point) (- e b))))))

(defun caredit--slurp-forward-argument ()
  "Slurp function argument into inner sexp."
  (save-excursion
    (backward-up-list)
    (forward-list)
    ;; now at )|
    (let ( ;; |)
          (start-paren
           (save-excursion
             (backward-char)
             (caredit--backward-over-whitespace)))
          ;; )|
          (end-paren (point)))
      (caredit--forward-over-whitespace)
      (let ((reg (buffer-substring-no-properties start-paren end-paren)))
        (when (= ?, (char-after))
          ;; examples of where this branch is taken:
          ;; g(f(x, |y), z)  =>  g(f(x, |y, z))

          ;; go over comma
          (forward-char)
          (caredit--forward-over-whitespace))

        (caredit-forward-sexp)

        ;; now we are past next arg, insert paren again
        (delete-region start-paren end-paren)
        (insert reg)))))

(defun caredit-slurp-forward ()
  "Slurp function argument into inner sexp or statement into inner block."
  (interactive)
  (if (or (caredit--in-comment-p)
          (caredit--in-string-p))
      (caredit--slurp-forward-string)
    (let ((up-char
           (save-excursion
             (backward-up-list)
             (char-after))))
      (if (= ?\( up-char)
          ;; if slurping an argument fails, slurp a statement instead
          (caredit--orelse (caredit--slurp-forward-argument)
                           (caredit--slurp-forward-statement))
        (caredit--slurp-forward-statement)))))


;;; Wrap:
(defun caredit-wrap-statement (open close)
  "Wrap current expression by insert OPEN before and CLOSE after it."
  (caredit--error-save-excursion
    (caredit-end-of-balanced-statement)
    (let ((e (ponit)))
      (caredit-beginning-of-balanced-statement)
      (insert open)
      (goto-char e)
      (insert close)
      (goto-char old-point))))


;;; Simple mapping commands:
(defun caredit-semicolon ()
  "Insert semicolon, new line, indent."
  (interactive)
  (if (= ?\; (char-after))
      (progn (forward-char)
             (caredit--forward-over-whitespace))
    (insert ";\n"))
  (indent-for-tab-command))

(provide 'caredit-core)
;;; caredit-core.el ends here
