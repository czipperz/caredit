;;; caredit-macros.el --- Core macros of Caredit

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
;; Define core macros used in other parts of the package.

;;; Code:
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

(provide 'caredit-macros)
;;; caredit-macros.el ends here
