;;; caredit.el --- basic paredit for c-like languages

;; Copyright (C) 2005--2016 Taylor R. Campbell
;; Copyright (C) 2013-2015 zk_phi
;; Copyright (C) 2016 Chris Gregory czipperz@gmail.com

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Chris Gregory "czipperz"
;; Email: czipperz@gmail.com
;; Version: 0.0.1

;;; Commentary:
;; caredit --- basic paredit for c-like languages

;; This program defines paredit-like functions for use with c like
;; languages.

;;; Code:

;; c-beginning-of-statement, c-end-of-statement
(require 'cc-mode)

(defconst caredit-version "0.0.1")

(defmacro caredit--dowhile (prop &rest body)
  "Eval BODY in order then repeat while PROP is truthy."
  `(progn ,@body (while ,prop ,@body)))

(defmacro caredit--orelse (fst snd)
  "Try to eval FST and return the result.  If it threw an error,
SND is evaled and returned."
  `(condition-case err ,fst (error ,snd)))

(defun caredit--list-level ()
  "Get the number of times `backward-up-list' works."
  (nth 0 (syntax-ppss)))

(defun caredit--beginning-of-statement (&optional num)
  "Move backward to the beginning of NUM (or 1) statements.

c; { a; b; } |d;  =>  c; { a; |b; } d;"
  (interactive)
  (c-beginning-of-statement (or num
                                1)
                            (point-min) nil)
  (point))

(defun caredit--end-of-statement (&optional num)
  "Move backward to the beginning of NUM (or 1) statements.

c; |{ a; b; } |d;  =>  c; { a;| b; } d;"
  (interactive)
  (c-end-of-statement (or num
                          1)
                      (point-max) nil)
  (point))

(defun caredit--beginning-of-balanced-statement (&optional num)
  "Move backward to the beginning of NUM (or 1) balanced statements.

c; { a; b; } |d;  =>  c; |{ a; b; } d;"
  (interactive)
  (let ((init-list-level (caredit--list-level)))
    (caredit--dowhile (not (or (bobp)
                               (= (caredit--list-level)
                                  init-list-level)))
                      (caredit--beginning-of-statement)))
  (point))

(defun caredit--end-of-balanced-statement (&optional num)
  "Move forward to the end of NUM (or 1) balanced statements.

c; |{ a; b; } d;  =>  c; { a; b; }| d;"
  (interactive)
  (let ((init-list-level (caredit--list-level)))
    (caredit--dowhile (not (or (eobp)
                               (= (caredit--list-level)
                                  init-list-level)))
                      (caredit--end-of-statement)))
  (point))

(defmacro caredit--assert (exp &optional message)
  "Assert EXP is truthy, throwing an error if it didn't."
  (setq message (or message
                    (format "assertion failed: %s" exp)))
  `(unless ,exp (error ,message)))

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

;;; Taken from paredit.  Have to evaluate at compile time so it can be
;;; used in other macros safely.
(eval-and-compile
  (defun caredit--conc-name (&rest strings)
    "Concatenate STRINGS and tern them into an atom."
    (intern (apply 'concat strings))))

(defmacro caredit--define-wrap (open close name)
  "Define function caredit-wrap-NAME to wrap the following sexp in OPEN and CLOSE."
  `(defun ,(caredit--conc-name "caredit-wrap-" name) ()
     ,(concat "Wrap the following S-expression.

Inserts " (list open) " before and " (list close) " after.

|aaa  =>  " (list open) "|aaa" (list close))
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
       `(progn
          (insert ,open)
          (unless (or (caredit--in-string-p)
                      (caredit--in-char-p)
                      (caredit--in-comment-p))
            (insert ,close)
            (backward-char))))

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
  "caredit--define-pair for string and char"
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

(defun caredit--whitespace-p (ch)
  "Test if CH is whitespace."
  (or (= ch ? )
      (= ch ?\t)
      (= ch ?\n)))

(defun caredit--forward-over-whitespace ()
  "Move forward while on whitespace."
  (while (and (not (eobp)) (caredit--whitespace-p (char-after)))
    (forward-char)))

(defun caredit--backward-over-whitespace ()
  "Move backward while after whitespace."
  (while (and (not (bobp)) (caredit--whitespace-p (char-before)))
    (backward-char)))

(defun caredit--get-region (mark point)
  "Get region MARK POINT."
  (interactive (list (mark) (point)))
  (save-excursion (let ((reg (get-register ?r)))
                    (copy-to-register ?r mark point)
                    (prog1 (get-register ?r)
                      (set-register ?r reg)))))

(defconst caredit--open-chars (list ?\( ?\[ ?\{))
(defconst caredit--close-chars (list ?\) ?\] ?\}))

(defun caredit-forward-sexp ()
  "Go to next AST tree."
  (interactive)
  ;; go past whitespace
  (caredit--forward-over-whitespace)
  (cond ((= ?\" (char-after))
         ;; go forward over double quote pair
         (forward-char)
         (while (caredit--in-string-p)
           (forward-char)))
        ((= ?\' (char-after))
         ;; go forward over single quote pair
         (forward-char)
         (while (caredit--in-char-p)
           (forward-char)))
        (t
         ;; go forward over `f(a, b)'
         (forward-sexp)
         (while (member (char-after) caredit--open-chars)
           (forward-sexp))))
  (point))

(defun caredit--backward-sexp ()
  "Go to previous AST tree."
  (interactive)
  ;; go past whitespace
  (caredit--backward-over-whitespace)
  (cond ((= ?\" (char-before))
         (backward-char)
         (while (caredit--in-string-p)
           (backward-char)))
        ((= ?\' (char-before))
         (backward-char)
         (while (caredit--in-string-p)
           (backward-char)))
        (t
         (let ((prev-is-statement
                (save-excursion
                  (c-beginning-of-statement 2)
                  (let ((is-brace
                         (progn
                           (c-end-of-statement)
                           (= ?\{ (char-after)))))
                    ;; this will be { if in first statement in block
                    (= ?\; (char-before)))))))
         (backward-sexp)
         (while (member (char-before) caredit--close-chars)
           (backward-sexp)))))

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
          (new-pt (caredit-forward-sexp)))
      (delete-char -1)
      (caredit-forward-sexp)
      (insert ch))))

(defun caredit--slurp-forward-statement ()
  "Slurp statement into braces."
  (save-excursion
    (backward-up-list)
    (while (/= ?\{ (char-after))
     (backward-up-list))
    (forward-list)
    ;; }|
    (let* ((e (point))
           (b
            (save-excursion
              ;; go back past last semicolon
              (caredit--beginning-of-statement)
              (caredit--end-of-statement)
              ;; foo;|   }| bar;
              ;; second | is `e'
              ;; first | is `b'
              (point)))
           (reg (caredit--get-region b e)))
      ;; }| bar;
      (let ((end (save-excursion
                   (caredit--end-of-balanced-statement)
                   ;; } bar;|
                   (point))))
        ;; assert not } EOF
        (save-excursion
          (caredit--forward-over-whitespace)
          (caredit--assert (/= end (point))
                           "Nothing to slurp (end of buffer)"))
        ;; } bar;|
        (goto-char end))
      ;; remove |   }|
      (delete-region b e)
      (indent-region b (point))
      (insert reg))))

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
             (caredit--backward-over-whitespace)
             (point)))
          ;; )|
          (end-paren (point)))
      (caredit--forward-over-whitespace)
      (let ((reg (caredit--get-region start-paren end-paren)))
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

(defun caredit-semicolon ()
  "Insert semicolon, new line, indent."
  (interactive)
  (if (= ?\; (char-after))
      (progn (forward-char)
             (caredit--forward-over-whitespace))
    (insert ";\n"))
  (indent-for-tab-command))

(defmacro caredit--map-key (open close name)
  "Define keys in local buffer for inserting balanced OPEN CLOSE pairs, to functions with NAME postfixes.

OPEN is set to caredit-open-NAME,
M-OPEN is set to caredit-wrap-NAME,
CLOSE is set to caredit-close-NAME."
  `(progn
     (caredit--map-double-key ,open ,name)
     (local-set-key (kbd (concat (list ,close)))
                    (caredit--conc-name "caredit-close-" ,name))))

(defmacro caredit--map-double-key (open name)
  "Define keys in local buffer for inserting balanced OPEN (CLOSE) pairs, to functions with NAME postfixes.

This facilitates DRY principles by extracting out common logic
with `caredit--map-key' for NAME=string and NAME=char bindings.

OPEN is set to caredit-open-NAME,
M-OPEN is set to caredit-wrap-NAME."
  `(progn
     (local-set-key (kbd (concat (list ,open)))
                    (caredit--conc-name "caredit-open-" ,name))
     (local-set-key (kbd (concat "M-" (list ,open)))
                    (caredit--conc-name "caredit-wrap-" ,name))))

(defun caredit-use-default-mappings-this-buffer ()
  "Use caredit default mappings in this buffer."
  (interactive)

  (local-set-key (kbd "C-)") 'caredit-slurp-forward)
  ;; (local-set-key (kbd "C-}") 'caredit-barf-forward)

  (caredit--map-key ?\( ?\) "paren")
  (caredit--map-key ?\{ ?\} "curly")
  (caredit--map-key ?\[ ?\] "square")
  (caredit--map-double-key ?\" "string")
  (caredit--map-double-key ?\' "char")

  (local-set-key (kbd ";") 'caredit-semicolon))

(provide 'caredit)
;;; caredit.el ends here