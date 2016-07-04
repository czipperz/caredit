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

(deftest caredit--end-of-balanced-statement--if-1
    "a; if (boo) b; c;\n"
  (caredit--end-of-balanced-statement)
  (should (looking-at-p " if (boo) b; c;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " c;$"))

  (caredit--end-of-balanced-statement)
  (should (eolp))

  (should-error (caredit--end-of-balanced-statement)))

(deftest caredit--end-of-balanced-statement--if-2
    "a; if (boo) { b; c; } d;"
  (caredit--end-of-balanced-statement)
  (should (looking-at-p " if (boo) { b; c; } d;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " d;$"))

  (caredit--end-of-balanced-statement)
  (should (eolp))

  (should-error (caredit--end-of-balanced-statement)))

(deftest caredit--end-of-balanced-statement--if-3
    "a; if (boo) a; else b; c;\n"
  (caredit--end-of-balanced-statement)
  (should (looking-at-p " if (boo) a; else b; c;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " c;$"))

  (caredit--end-of-balanced-statement)
  (should (eolp))

  (should-error (caredit--end-of-balanced-statement))

  (goto-char 8)
  (should (looking-at-p "boo) a; else b; c;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " c;$"))

  (goto-char 13)
  (should (looking-at-p "a; else b; c;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " else b; c;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " c;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p "$")))

(deftest caredit--end-of-balanced-statement--if-4
    "if (1) {
  2;
} else if (3) {
  do { 4; } while (0);
} else {
  5;
}"
  (caredit--end-of-balanced-statement)
  (should (eobp))

  (should-error (caredit--end-of-balanced-statement))

  (goto-char 10)
  (should (looking-at-p "  2;\n} else if (3) {\n  do { 4;"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p "\n} else if (3) {\n  do { 4;"))

  (should-error (caredit--end-of-balanced-statement))

  (goto-char 31)
  (should (looking-at-p "  do { 4; } while (0);\n} else {\n  5;\n}"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p "\n} else {\n  5;\n}"))

  (should-error (caredit--end-of-balanced-statement))

  (goto-char 37)
  (caredit--end-of-balanced-statement)
  (should (looking-at-p " } while (0);\n")))

(deftest caredit--beginning-of-balanced-statement--if-1
    "if (1) {
  2;
} else if (3) {
  do { 4; } while (0);
} else {
  5;
}\n"
  (goto-char (point-max))
  (caredit--beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit--beginning-of-balanced-statement))

  (goto-char 14)
  (should (looking-at-p "\n} else if (3) {\n  do { 4;"))

  (caredit--beginning-of-balanced-statement)
  (should (looking-at-p "2;\n} else if (3) {\n  do { 4;"))

  (should-error (caredit--beginning-of-balanced-statement))

  (goto-char 53)
  (should (looking-at-p "\n} else {\n  5;\n}\n"))

  (caredit--beginning-of-balanced-statement)
  (should (looking-at-p "do { 4; } while (0);\n"))

  (should-error (caredit--beginning-of-balanced-statement))

  (goto-char 40)
  (caredit--beginning-of-balanced-statement)
  (should (looking-at-p "4; } while (0);\n")))

(deftest caredit--beginning-of-balanced-statement--if-2
    "if (1) 2; else if (3) do { 4; } while (0); else 5;\n"
  (goto-char (point-max))
  (caredit--beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit--beginning-of-balanced-statement)))

(deftest caredit--beginning-of-balanced-statement--if-3
    "if (1) 2; else if (3) { do { 4; } while (0); } else 5;\n"
  (goto-char (point-max))
  (caredit--beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit--beginning-of-balanced-statement)))

(provide 'if-tests)
;;; if-tests.el ends here
