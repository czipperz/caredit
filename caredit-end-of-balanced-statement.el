;;; caredit-end-of-balanced-statement.el --- Provide name of module as a function

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
;;; Code:
(require 'caredit-beginning-of-balanced-statement)

(defun caredit--forward-out-of-parens ()
  "Go `up-list' while it would take out of a parenthesized list."
  (let ((p (point)))
    (condition-case err
        (while t
          (up-list)
          (if (= (char-before) ?\))
              (setq p (point))
            (error "")))
      (error (goto-char p)))))

(defun caredit--at-end-of-statement ()
  "Test if we are at the beginning of a statement."
  (and
   (member (char-before) (list ?\; ?\}))
   (prog1 t
     (when (and over-do
                (caredit--before-keyword "while"))
       (when (boundp 'debug-messages) (message "HI"))
       (caredit--end-stupid)))
   (if (and over-else
            (or (caredit--before-keyword "else")
                (caredit--before-keyword "if")))
       (progn
         (caredit--move-after-keyword "else")
         (caredit--move-after-if)
         (caredit-end-of-balanced-statement)
         (caredit--at-end-of-statement))
     t)))

(defun caredit--forward-balanced-char ()
  "Go forward a list if at beginning of a list.  Go backward a char otherwise."
  (cond ((member (char-after) caredit--open-chars)
         (forward-list))
        ((= (char-after) ?\})
         (caredit--beginning-error))
        ((member (char-after) caredit--close-chars)
         (up-list))
        (t
         (forward-char))))

(defun caredit--end-stupid ()
  "Go forward a balanced char while not past `;' or `}'."
  (caredit--dowhile (not (member (char-before) (list ?\; ?\})))
    (caredit--forward-balanced-char)))

(defun caredit--forward-over-nonwhitespace ()
  "Move forward while on whitespace."
  (while (and (not (eobp))
              (not (caredit--whitespace-p (char-after))))
    (forward-char))
  (point))

(defun caredit--backward-over-nonwhitespace ()
  "Move backward while after whitespace."
  (while (and (not (bobp))
              (not (caredit--whitespace-p (char-before))))
    (backward-char))
  (point))

(defun caredit-end-of-balanced-statement ()
  "Go to the end of he current statement."
  (caredit--error-save-excursion
    (caredit--backward-out-of-parens)
    (caredit--forward-over-whitespace)
    (let ((statements 1) (over-semi 0) over-else over-do stupid)
      ;; categorize what what we should do
      (save-excursion
        (unless (member (char-after) (list ?\; ?\}))
          (caredit--forward-balanced-char))
        (when (ignore-errors
                (caredit-beginning-of-balanced-statement))
          (cond ((or (caredit--before-keyword "if")
                     (caredit--before-keyword "else"))
                 (when (boundp 'debug-messages) (message "if"))
                 (setq over-else t))
                ((caredit--before-keyword "do")
                 (when (boundp 'debug-messages) (message "do"))
                 (setq over-do t))
                (t
                 (when (boundp 'debug-messages) (message "stupid"))
                 (setq stupid t)))))

      (if stupid
          (caredit--end-stupid)

        (when (or (eobp) (= (char-after) ?\}))
          (caredit--beginning-error))
        (when over-else
          (cond ((= (char-after) ?\()
                 ;; else if |()
                 (forward-list)
                 (caredit-end-of-balanced-statement))
                ((or (caredit--move-after-keyword "else")
                     (caredit--before-keyword "if"))
                 ;; |else if ()
                 (caredit--move-after-if)
                 (caredit--forward-over-whitespace)
                 (unless (= (char-after) ?\{)
                   (caredit-end-of-balanced-statement)
                   (caredit--forward-over-whitespace)))
                (t
                 ;; else if ()|
                 (caredit--backward-over-whitespace)
                 (cond ((= (char-before) ?\))
                        (caredit-end-of-balanced-statement))))))
        (cond ((or (eobp) (= (char-after) ?\}))
               (caredit--beginning-error))
              ((bobp)
               (forward-char))
              ((= (char-after) ?\;)
               (forward-char)
               (caredit--increment-var over-semi))
              ((= (char-after) ?\{)
               (forward-list)))

        (catch 'done
          (while (/= statements 0)
            (when (eobp)
              (caredit--beginning-error))
            (if (caredit--at-end-of-statement)
                (caredit--increment-var statements -1)
              (caredit--forward-balanced-char)))))))
  (point))

(provide 'caredit-end-of-balanced-statement)
;;; caredit-end-of-balanced-statement.el ends here
