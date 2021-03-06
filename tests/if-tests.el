;;; if-tests.el --- Testing output with if statements.

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
(require 'caredit)
(require 'test-utils)

(deftest caredit-end-of-balanced-statement--if-1
    "a; if (boo) b; c;\n"
  (caredit-end-of-balanced-statement)
  (should (looking-at-p " if (boo) b; c;$"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p " c;$"))

  (caredit-end-of-balanced-statement)
  (should (eolp))

  (should-error (caredit-end-of-balanced-statement)))

(deftest caredit-end-of-balanced-statement--if-2
    "a; if (boo) { b; c; } d;"
  (caredit-end-of-balanced-statement)
  (should (looking-at-p " if (boo) { b; c; } d;$"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p " d;$"))

  (caredit-end-of-balanced-statement)
  (should (eolp))

  (should-error (caredit-end-of-balanced-statement)))

(deftest caredit-end-of-balanced-statement--if-3
    "a; if (boo) a; else b; c;\n"
  (caredit-end-of-balanced-statement)
  (should (looking-at-p " if (boo) a; else b; c;$"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p " c;$"))

  (caredit-end-of-balanced-statement)
  (should (eolp))

  (should-error (caredit-end-of-balanced-statement))

  (goto-char 8)
  (should (looking-at-p "boo) a; else b; c;$"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p " c;$"))

  (goto-char 13)
  (should (looking-at-p "a; else b; c;$"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p " else b; c;$"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p " c;$"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p "$")))

(deftest caredit-end-of-balanced-statement--if-4
    "if (1) {
  2;
} else if (3) {
  do { 4; } while (0);
} else {
  5;
}"
  (caredit-end-of-balanced-statement)
  (should (eobp))

  (should-error (caredit-end-of-balanced-statement))

  (goto-char 10)
  (should (looking-at-p "  2;\n} else if (3) {\n  do { 4;"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p "\n} else if (3) {\n  do { 4;"))

  (should-error (caredit-end-of-balanced-statement))

  (goto-char 31)
  (should (looking-at-p "  do { 4; } while (0);\n} else {\n  5;\n}"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p "\n} else {\n  5;\n}"))

  (should-error (caredit-end-of-balanced-statement))

  (goto-char 37)
  (caredit-end-of-balanced-statement)
  (should (looking-at-p " } while (0);\n")))

(deftest caredit-beginning-of-balanced-statement--if-1
    "if (1) {
  2;
} else if (3) {
  do { 4; } while (0);
} else {
  5;
}\n"
  (goto-char (point-max))
  (caredit-beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit-beginning-of-balanced-statement))

  (goto-char 14)
  (should (looking-at-p "\n} else if (3) {\n  do { 4;"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "2;\n} else if (3) {\n  do { 4;"))

  (should-error (caredit-beginning-of-balanced-statement))

  (goto-char 53)
  (should (looking-at-p "\n} else {\n  5;\n}\n"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "do { 4; } while (0);\n"))

  (should-error (caredit-beginning-of-balanced-statement))

  (goto-char 40)
  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "4; } while (0);\n"))

  ;; test every point in top statement goes to `point-min'
  (dolist (p (list (+ 1 (point-min))
                   ;; | else if (3) {
                   16
                   ;; | else {
                   55))
    (goto-char p)
    (while (/= (char-before) ?\{)
      (caredit-beginning-of-balanced-statement)
      (should (bobp))
      (setq p (1+ p))
      (goto-char p)))

  ;; In `' pairs, caredit-beginning-of-balanced-statement should go to
  ;; |d`o '{ 4; }` while (0);\n'}
  (let ((p 34))
    (goto-char p)
    (should (looking-at-p "o { 4; } while (0);\n"))

    (while (/= (char-after) ?\})
      (caredit-beginning-of-balanced-statement)
      (should (looking-at-p "do { 4; } while (0);\n"))
      (setq p (1+ p))
      (goto-char p)
      (when (= (char-before) ?\{)
        (backward-char)
        (forward-list)
        (setq p (point))))))

(deftest caredit-beginning-of-balanced-statement--if-2
    "if (1) 2; else if (3) do { 4; } while (0); else 5;\n"
  (goto-char (point-max))
  (caredit-beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit-beginning-of-balanced-statement))

  (goto-char 9)
  (should (looking-at-p "; else if (3)"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "2; else if (3)"))

  (let ((p 24))
    (goto-char p)
    (should (looking-at-p "o { 4; } while (0);"))

    (while (/= (char-before) ?\;)
      (caredit-beginning-of-balanced-statement)
      (should (looking-at-p "do { 4; } while (0);"))
      (setq p (1+ p))
      (goto-char p)
      (when (= (char-before) ?\{)
        (backward-char)
        (forward-list)
        (setq p (point))))))

(deftest caredit-beginning-of-balanced-statement--if-3
    "if (1) 2; else if (3) { do { 4; } while (0); } else 5;\n"
  (goto-char (point-max))
  (caredit-beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit-beginning-of-balanced-statement)))

(deftest caredit-beginning-of-balanced-statement--if-4
    "if (1) 2; else if (3) do 4; while (0); else 5;\n"
  (goto-char (point-max))
  (caredit-beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit-beginning-of-balanced-statement))

  ;; i`f (1) '  should go to bob
  (let ((p (1+ (point-min))))
    (goto-char p)
    (while (/= (char-before) ?2)
      (caredit-beginning-of-balanced-statement)
      (should (bobp))
      (goto-char (setq p (1+ p)))))

  ;; if (1) 2|;  should go to  if (1) |2;
  (should (looking-at-p "; else if (3)"))
  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "2; else if (3)"))

  ;; 2;` else if (3) 'do 4;  should go to bob
  (let ((p 10))
    (goto-char p)
    (should (looking-at-p " else if (3) do 4;"))
    (while (/= (char-before) ?d)
      (caredit-beginning-of-balanced-statement)
      (should (bobp))
      (goto-char (setq p (1+ p)))))

  ;; d`o '4;` while (0)';  should go to `|do 4; while (0);'
  ;; do 4|; while (0);  should go to  do |4; while (0);
  (let ((p 24))
    (goto-char p)
    (should (looking-at-p "o 4; while (0);"))
    (while (/= (char-before) ?4)
      (caredit-beginning-of-balanced-statement)
      (should (looking-at-p "do 4; while (0);"))
      (goto-char (setq p (1+ p))))
    (caredit-beginning-of-balanced-statement)
    (should (looking-at-p "4; while (0);"))

    (goto-char (setq p (1+ p)))
    (should (looking-at-p " while (0);"))

    (caredit-beginning-of-balanced-statement)
    (should (looking-at-p "do 4; while (0);"))

    (goto-char (setq p (1+ p)))
    (should (looking-at-p "while (0);"))

    (while (/= (char-before) ?\;)
      (caredit-beginning-of-balanced-statement)
      (should (looking-at-p "do 4; while (0);"))
      (goto-char (setq p (1+ p)))))

  ;; while (0);` else '5;  should go to bob
  (let ((p 39))
    (goto-char p)
    (should (looking-at-p " else 5;"))
    (while (/= (char-before) ?5)
      (caredit-beginning-of-balanced-statement)
      (should (bobp))
      (goto-char (setq p (1+ p))))

    ;; else 5|;  should go to  else |5;
    (should (looking-at-p ";$"))
    (caredit-beginning-of-balanced-statement)
    (should (looking-at-p "5;$"))

    ;; else 5;|  should to go bob
    (goto-char (setq p (1+ p)))
    (caredit-beginning-of-balanced-statement)
    (should (bobp))))

(provide 'if-tests)
;;; if-tests.el ends here
