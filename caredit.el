;;; caredit.el --- basic paredit for c-like languages

;; Copyright (C) 2005--2016 Taylor R. Campbell
;; Copyright (C) 2013-2015 zk_phi
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
;; caredit --- basic paredit for c-like languages

;; This program defines paredit-like functions for use with c like
;; languages.

;;; Code:


;;; Requires:
;; c-beginning-of-statement, c-end-of-statement
(require 'cc-mode)
(require 'cl-lib)


;;; Consts:
(defconst caredit-version "0.0.1")

;; Declaring the variables at compilation removes warnings.
(eval-when-compile
  (defvar caredit--open-chars)
  (defvar caredit--close-chars))

(setq caredit--open-chars (list ?\( ?\[ ?\{))
(setq caredit--close-chars (list ?\) ?\] ?\}))


;;; Macros:
(defmacro caredit--assert (exp &optional message should-format)
  "Assert EXP is truthy, throwing an error if it didn't.

The error message is MESSAGE if it is given.
MESSAGE will be formated with EXP if SHOULD-FORMAT isn't null.

If MESSAGE is null, the error message is \"Assertion failed: %s\", and
SHOULD-FORMAT is t."
  `(unless ,exp
     (error
      ,(if message
           (if should-format
               (format message exp)
             message)
         (format "Assertion failed: %s" exp)))))

(defmacro caredit--did-point-move (&rest body)
  "Evaluate BODY, return t if point is not equal to what is was before that execution."
  (declare (indent 0))
  (let ((old-point (cl-gensym)))
    `(let ((,old-point (point)))
       ,@body
       (/= ,old-point (point)))))

(defmacro caredit--does-point-move (&rest body)
  "Wrap `caredit--did-point-move' in a `save-excursion'.

Evaluates BODY, returning t if point is not equal to what it was
before that execution, saving the excursion."
  (declare (indent 0))
  `(save-excursion
     (caredit--did-point-move ,@body)))

(defmacro caredit--where-does-point-move (&rest body)
  "Evaluate BODY and return the resulting point, saving excursion."
  (declare (indent 0))
  `(save-excursion
     ,@body
     (point)))

(defmacro caredit--error-save-excursion (&rest body)
  "Evaluate BODY, restoring the point if an error occured (then rethrowing).

This old point is stored in the variable `old-point'.
You can use it to change the way this function handles errors."
  (declare (indent 0))
  `(let ((old-point (point)))
     (condition-case err (progn ,@body)
       (error (goto-char old-point) (error (cadr err))))))

(defmacro caredit--dowhile (prop &rest body)
  "Eval BODY in order then repeat while PROP is truthy."
  (declare (indent 1))
  `(progn ,@body (while ,prop ,@body)))

(defmacro caredit--orelse (fst snd)
  "Try to eval FST and return the result.  If it threw an error, SND is evaled and returned."
  (declare (indent 1))
  `(condition-case err ,fst (error ,snd)))


;;; Utility functions:
(defun caredit--list-level ()
  "Get the number of times `backward-up-list' works."
  (nth 0 (syntax-ppss)))

(defun caredit--beginning-of-statement (&optional num)
  "Move backward to the beginning of NUM (or 1) statements.

Prefer `caredit--beginning-of-balanced-statement' over this function.

c; { a; b; } |d;  =>  c; { a; |b; } d;"
  (interactive)
  (c-beginning-of-statement (or num
                                1)
                            (point-min) nil)
  (point))

(defun caredit--end-of-statement (&optional num)
  "Move backward to the beginning of NUM (or 1) statements.

Prefer `caredit--end-of-balanced-statement' over this function.

c; |{ a; b; } |d;  =>  c; { a;| b; } d;"
  (interactive)
  (c-end-of-statement (or num
                          1)
                      (point-max) nil)
  (point))

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

(defun caredit--get-region (mark point)
  "Get region MARK POINT."
  (interactive (list (mark) (point)))
  (save-excursion (let ((reg (get-register ?r)))
                    (copy-to-register ?r mark point)
                    (prog1 (get-register ?r)
                      (set-register ?r reg)))))


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


;;; caredit--{beginning,end}-of-balanced-statement implementation:
(defun caredit--internal-at-do-while-while ()
  "DO NOT USE THIS.

Check while (...);"
  (save-excursion
    (caredit--forward-over-whitespace)
    (and (looking-at-p "while")
         (progn
           (forward-char 5)
           ;; while| (...);
           (caredit--forward-over-whitespace)
           (= ?\( (char-after)))
         (progn
           (forward-list)
           (= ?\) (char-before)))
         (progn
           (caredit--forward-over-whitespace)
           (= ?\; (char-after))))))

(defun caredit--internal-at-do-while ()
  "Test if cursor is looking at `while (...);' and behind cursor is `do {...}'."
  (ignore-errors
    (save-excursion
      (and
       ;; check | while (...);
       (caredit--internal-at-do-while-while)
       ;; check do {...}|
       (progn
         ;; }|
         (caredit--backward-over-whitespace)
         (and (= ?\} (char-before))
              (progn
                (backward-list)
                ;; do |{...}
                (= ?\{ (char-after)))
              (progn
                (caredit--backward-over-whitespace)
                ;; do|
                (= ?o (char-before)))
              (progn
                (backward-char)
                (= ?d (char-before)))))))))

(defun caredit--internal-at-do-keyword ()
  "Test if `do ' or `do{'."
  (save-excursion
    (and (looking-at-p "do")
         (progn (forward-char 2)
                (or (caredit--whitespace-p (char-after))
                    (= ?\{ (char-after)))))))

(defun caredit--fix-do-while-nobrace-beginning ()
  "do a;| while (...);  =>  |do a; while (...);

Return t if moved, nil otherwise."
  (ignore-errors
    (caredit--error-save-excursion
      (when ;; check | while (...);
          (caredit--internal-at-do-while-while)
        ;; check do a;|
        (backward-char)
        (caredit--beginning-of-balanced-statement)
        ;; |do a;
        (caredit--internal-at-do-keyword)))))

(defun caredit--fix-do-while-nobrace-end ()
  "do a;| while (...);  =>  do a; while (...);|"
  (ignore-errors
    (caredit--error-save-excursion
      (when ;; check | while (...);
          (caredit--internal-at-do-while-while)
        (save-excursion
          ;; check do a;|
          (backward-char)
          (caredit--beginning-of-balanced-statement)
          ;; |do a;
          (caredit--assert (caredit--internal-at-do-keyword)))
        (forward-list)
        (forward-char)))))

(defun caredit--move-backward-over-if/else-if ()
  "Move backwards before an if (and then possibly and else).

else if ()|  =>  |else if ()
if ()|  =>  |if ()"
  (ignore-errors
    (caredit--error-save-excursion
      (caredit--backward-over-whitespace)
      (caredit--assert (= ?\) (char-before)))
      (backward-list)
      (caredit--assert (= ?\( (char-after)))
      (caredit--backward-over-whitespace)
      (caredit--assert (= ?f (char-before)))
      (backward-char)
      (caredit--assert (= ?i (char-before)))
      (backward-char)
      (caredit--assert (or (caredit--whitespace-p (char-before))
                           (= ?/ (char-before))
                           (member (char-before) caredit--open-chars)
                           (member (char-before) caredit--close-chars)))
      (ignore-errors
        (caredit--error-save-excursion
          (caredit--backward-over-whitespace)
          (caredit--assert (= ?e (char-before)))
          (backward-char)
          (caredit--assert (= ?s (char-before)))
          (backward-char)
          (caredit--assert (= ?l (char-before)))
          (backward-char)
          (caredit--assert (= ?e (char-before)))
          (backward-char)
          (caredit--assert (or (caredit--whitespace-p (char-before))
                               (= ?/ (char-before))
                               (member (char-before)
                                       caredit--open-chars)
                               (member (char-before)
                                       caredit--close-chars))))))))

(defun caredit--beginning-of-balanced-statement ()
  "Move backward to the beginning of one balanced statements.

c; { a; b; } |d;  =>  c; |{ a; b; } d;

do { a; } while (0); d; e;|  =>  do { a; } while (0); d; |e;
                             =>  do { a; } while (0); |d; e;
                             =>  |do { a; } while (0); d; e;

do a; while (0); d;|  =>  do a; while (0); |d;
                      =>  |do a; while (0); d;"
  (interactive)
  (caredit--error-save-excursion
    (caredit--backward-over-whitespace)
    (catch 'done
      (caredit--assert (not (bobp)) "No more balanced statements")
      (cond ((= (char-before) ?\;)
             (backward-char))
            ((= (char-before) ?\})
             (backward-list)))
      (while t
        (cond ((bobp)
               (caredit--forward-over-whitespace)
               (caredit--assert (< (point) old-point)
                                "No more balanced statements")
               (throw 'done (point)))
              ((or (caredit--in-comment-p)
                   (caredit--in-char-p)
                   (caredit--in-string-p))
               (backward-char))
              ((= (char-before) ?\;)
               ;; properly detect |do a; while (...);
               ;; (go to |a; if point was in a;)
               (let ((p
                      (catch 'pt
                        (ignore-errors
                          (save-excursion
                            (let ((p (point)))
                              (caredit--forward-over-whitespace)
                              (when (caredit--internal-at-do-keyword)
                                (forward-char 2)
                                (caredit--forward-over-whitespace)
                                (let ((p (point)))
                                  (when (> old-point p)
                                    (caredit--end-of-balanced-statement)
                                    (when (< old-point (point))
                                      (throw 'pt p)))))))))))
                 (when p
                   (goto-char p)
                   (throw 'done p)))
               ;; properly detect do a;| while (...);
               ;; (go to |do a;.  Without this do a; |while)
               (let ((p (point)))
                 (caredit--fix-do-while-nobrace-beginning)
                 (when (and (/= p (point))
                            (< old-point p)
                            (>= old-point
                                (caredit--where-does-point-move
                                  (caredit--forward-over-whitespace)
                                  (forward-char 2)
                                  (caredit--forward-over-whitespace))))
                   (goto-char p)
                   (throw 'done (point))))
               (when (caredit--does-point-move
                       (caredit--forward-over-else))
                 ;; |else
                 (caredit--beginning-of-balanced-statement))
               (caredit--forward-over-whitespace)
               (throw 'done (point)))
              ((= (char-before) ?\})
               (when (caredit--internal-at-do-while)
                 ;; handle do {}| while ();  =>  |do {} while ();
                 (backward-list)
                 (backward-word))
               (caredit--move-backward-over-if/else-if)
               (when (caredit--does-point-move
                       (caredit--forward-over-else))
                 ;; |else
                 (caredit--beginning-of-balanced-statement))
               (caredit--forward-over-whitespace)
               (throw 'done (point)))
              ((member (char-before) caredit--close-chars)
               (backward-list))
              ((= (char-before) ?\{)
               (caredit--forward-over-whitespace)
               ;; old-point is from error-save-excursion
               (caredit--assert (< (point) old-point)
                                "No more balanced statements.")
               (throw 'done (point)))
              (t
               (backward-char)))))))

(defun caredit--forward-over-else ()
  "Go forward over else keyword.

| else a;  =>  else| a;
|a;  =>  |a;"
  (ignore-errors
    (caredit--error-save-excursion
      (caredit--forward-over-whitespace)
      (caredit--assert (looking-at-p "else"))
      (forward-char 4)
      (caredit--assert (caredit--whitespace-p (char-after)))
      (caredit--forward-over-whitespace))))

(defun caredit--fix-else-end ()
  "Assumes after if (`caredit--internal-at-if' was t)."
  (ignore-errors
    (caredit--error-save-excursion
      (let ((p (point)))
        (caredit--forward-over-else)
        (when (/= p (point))
          (caredit--end-of-balanced-statement)
          t)))))

(defun caredit--forward-over-if ()
  "Move forward over if statement.

|if (...)  =>  if (...)|"
  (caredit--error-save-excursion
    (caredit--forward-over-whitespace)
    (caredit--assert (looking-at-p "if")
                     "Must be looking at `if'")
    (forward-char 2)
    (caredit--forward-over-whitespace)
    (caredit--assert (= (char-after) ?\()
                     "Can't find `(' after `if' statement")
    (forward-list)
    ;; if (...)|
    ))

(defun caredit--internal-at-if ()
  "Test `if (...)'.  Cursor can be anywhere pretty much.

if| (1) 2;  =>  t
|if (1) 2;  =>  t
if (1) |2;  =>  nil
if (1) 2;|  =>  nil"
  (ignore-errors
    (let ((pt (point)))
      (save-excursion
        (if (member (char-after) caredit--open-chars)
            (forward-list)
          (unless (or (member (char-after) caredit--close-chars)
                      (= (char-after) ?\;))
            (forward-char)))
        (caredit--beginning-of-statement)
        (caredit--forward-over-else)
        (caredit--forward-over-if)
        (<= pt (point))))))

(defun caredit--end-of-balanced-statement ()
  "Move forward to the end of one balanced statements.

c; |{ a; b; } d;  =>  c; { a; b; }| d;"
  (interactive)
  (caredit--error-save-excursion
    (caredit--forward-over-whitespace)
    (let ((start-at-if (caredit--internal-at-if)))
      (catch 'done
        (while t
          (cond ((or (eobp) (= (char-after) ?\}))
                 (caredit--backward-over-whitespace)
                 (caredit--assert (> (point) old-point)
                                  "No more balanced statements.")
                 (throw 'done (point)))
                ((or (caredit--in-comment-p)
                     (caredit--in-char-p)
                     (caredit--in-string-p))
                 (forward-char))
                ((= (char-after) ?\;)
                 (forward-char)
                 (if start-at-if
                     (while (caredit--fix-else-end)))
                 ;; handle do ;| while ();
                 ;; do |a; while ();  =>  do a;| while ();
                 ;; do a;| while ();  =>  do a; while ();|
                 ;; d|o a; while ();  =>  do a; while ();|
                 ;; do a; w|hile ();  =>  do a; while ();|
                 (if (save-excursion
                       (when (caredit--fix-do-while-nobrace-beginning)
                         (forward-char 2)
                         (caredit--forward-over-whitespace)
                         (> (point) old-point)))
                     (caredit--fix-do-while-nobrace-end))
                 (if (caredit--internal-at-do-while)
                     (caredit--end-of-balanced-statement))
                 (throw 'done (point)))
                ((= (char-after) ?\{)
                 (forward-list)
                 (when (caredit--internal-at-do-while)
                   ;; handle do {}| while ();
                   (forward-word)
                   (forward-list)
                   (forward-char)
                   ;; while ();|
                   )
                 (when start-at-if
                   (while (caredit--fix-else-end))
                   (caredit--backward-over-whitespace))
                 (throw 'done (point)))
                ((member (char-after) caredit--open-chars)
                 (forward-list))
                (t
                 (forward-char))))))))


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
          (progn (setq e (caredit--end-of-balanced-statement))
                 (setq b (caredit--beginning-of-balanced-statement)))
        (progn (setq b (caredit--beginning-of-balanced-statement))
               (setq e (caredit--end-of-balanced-statement))))
      (cons b e))))

(defun caredit-wrap-curly ()
  "Wrap the statement in front of point in curly braces."
  (interactive)
  (let ((b (point)) e)
    (insert "{")
    (save-excursion)))

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
              ))
           (reg (caredit--get-region b e)))
      ;; }| bar;
      (let ((end (save-excursion
                   (caredit--end-of-balanced-statement)
                   ;; } bar;|
                   )))
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
             (caredit--backward-over-whitespace)))
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


;;; Wrap:
(defun caredit-wrap (open close)
  "Wrap current expression by insert OPEN before and CLOSE after it.")


;;; Simple mapping commands:
(defun caredit-semicolon ()
  "Insert semicolon, new line, indent."
  (interactive)
  (if (= ?\; (char-after))
      (progn (forward-char)
             (caredit--forward-over-whitespace))
    (insert ";\n"))
  (indent-for-tab-command))


;;; Mappings:
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
