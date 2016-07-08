;;; caredit-beginning-of-balanced-statement.el --- Provide name of module as a function
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
;;; Code:

(defmacro caredit--increment-var (var &optional num)
  "Increment VAR by NUM, or 1."
  `(setq ,var (+ ,var ,(or num 1))))

(defun caredit--after--do-brackets ()
  "Test if after `do {}'.

do |{}  =>  nil
do {}|  =>  t"
  (ignore-errors
    (save-excursion
      (backward-list)
      (caredit--assert (= ?\{ (char-after)))
      (caredit--backward-over-whitespace)
      (caredit--assert (= ?o (char-before)))
      (backward-char)
      (caredit--assert (= ?d (char-before)))
      (let ((p (point)))
        (backward-word))
      (caredit--assert (/= ?\_ (char-before)))
      t)))

(defun caredit--after--do-nobrackets ()
  "Test if after `do a;' (and before `while' keyword)."
  (ignore-errors
    (save-excursion
      (caredit--assert (caredit--before-keyword "while"))
      (caredit--backward-over-whitespace)
      (caredit--assert (= (char-before) ?\;))
      (backward-char)
      (caredit-beginning-of-balanced-statement)
      (caredit-beginning-of-balanced-statement)
      (caredit--assert (caredit--before-keyword "do"))
      t)))

(defun caredit--move-after-keyword (keyword)
  "Test if before KEYWORD.  If so, move forward over it and return t."
  (ignore-errors
    (caredit--error-save-excursion
      (caredit--forward-over-whitespace)
      (caredit--assert (or (bobp)
                           (member (char-before)
                                   (list ?\  ?\n ?\t ?\{ ?\}))))
      (caredit--assert (looking-at keyword))
      (goto-char (match-end 0))
      (caredit--assert (member (char-after)
                               (list ?\  ?\n ?\t ?\{ ?\()))
      t)))

(defun caredit--move-after-if (&optional old)
  "Test if before `if ()'.  If so, move over it and return t.

If OLD is given, only move if the resulting point is less than it."
  (let ((_p (point)))
    ;; fix:  if () a|;  =>  if () |a;
    ;; instead of           |if () a;
    (condition-case err
        (progn
          (caredit--assert
           (caredit--move-after-keyword "if"))
          (caredit--forward-over-whitespace)
          (caredit--assert (= (char-after) ?\())
          (forward-list)
          (caredit--assert (= (char-before) ?\)))
          (caredit--forward-over-whitespace)
          (when old (caredit--assert (> old (point))))
          t)
      (error (goto-char _p)
             nil))))

(defun caredit--before-keyword (keyword)
  "Test if before KEYWORD."
  (caredit--does-point-move
    (caredit--move-after-keyword keyword)))

(defun caredit--at-beginning-of-statement ()
  "Test if we are at the beginning of a statement."
  (save-excursion
    (or (bobp)
        (and
         (or
          (and (= (char-before) ?\;)
               (not (and (caredit--after--do-nobrackets)
                         (prog1 t (caredit--increment-var over-semi))
                         )))
          (and (= (char-before) ?\})
               (not (and (caredit--after--do-brackets)
                         (prog1 t (caredit--increment-var over-semi))
                         ))))
         (not (and (caredit--before-keyword "else")
                   (prog1 t
                     (when (= 0 over-semi)
                       (let ((p (point)))
                         (condition-case nil
                             (progn
                               (forward-word)
                               (if (caredit--move-after-if)
                                   (if (and (> old-point (point))
                                            (/= (char-after) ?\{))
                                       (throw 'done nil)
                                     (goto-char p))
                                 (caredit--forward-over-whitespace)
                                 (if (and (> old-point (point))
                                          (/= (char-after) ?\{))
                                     (throw 'done nil)
                                   (goto-char p))))
                           (error (goto-char p)))))
                     (caredit--increment-var over-semi)))))
        (= (char-before) ?\{))))

(defun caredit--beginning-error ()
  "Error for caredit-beginning-of-balanced-statement when no more statements."
  (error "No more balanced statements"))

(defun caredit--backward-balanced-char ()
  "Go backward a list if at end of a list.  Go backward a char otherwise."
  (cond ((= (char-before) ?\()
         (backward-list))
        ((member (char-before) caredit--close-chars)
         (backward-list))
        ((= (char-before) ?\{)
         (caredit--beginning-error))
        ((member (char-before) caredit--open-chars)
         (backward-up-list))
        (t
         (backward-char))))

(defun caredit--backward-out-of-parens ()
  "Go `backward-up-list' while it would take out of a parenthesized list."
  (let ((p (point)))
    (condition-case err
        (while t
          (backward-up-list)
          (if (= (char-after) ?\()
              (setq p (point))
            (error "")))
      (error (goto-char p)))))

(defun caredit-beginning-of-balanced-statement ()
  "Go to the beginning of the current statement."
  (caredit--error-save-excursion
    (caredit--backward-out-of-parens)
    (caredit--backward-over-whitespace)
    (let ((statements 1) (over-semi 0))
      (cond ((or (bobp) (= (char-before) ?\{))
             (caredit--beginning-error))
            ((= (char-before) ?\;)
             (backward-char)
             (caredit--increment-var over-semi))
            ((= (char-before) ?\})
             (backward-list)))

      (catch 'done
        (while (/= statements 0)
          (when (bobp)
            (if (= statements 1)
                (throw 'done nil)
              (caredit--beginning-error)))
          (if (= (char-before) ?\))
              (backward-list))
          (if (caredit--at-beginning-of-statement)
              (caredit--increment-var statements -1)
            (caredit--backward-balanced-char)))) ; while

      (when (= 0 over-semi)
        (let ((_p (point)))
          ;; fix:  do a|;  =>  do |a;
          ;; instead of        |do a;
          (condition-case nil
              (progn
                (caredit--assert
                 (caredit--move-after-keyword "do"))
                (caredit--forward-over-whitespace)
                (caredit--assert (> old-point (point)))
                (caredit--assert (/= (char-after) ?\{)))
            (error (goto-char _p)))))
      (when (= 0 over-semi)
        (let ((_p (point)))
          (condition-case nil
              (progn
                (caredit--move-after-keyword "else")
                (caredit--assert
                 (caredit--move-after-if old-point))
                (caredit--assert (/= (char-after) ?\{)))
            (error (goto-char _p))))))
    (caredit--forward-over-whitespace))
  (point))

(provide 'caredit-beginning-of-balanced-statement)
;;; caredit-beginning-of-balanced-statement.el ends here
