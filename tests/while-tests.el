;;; while-tests.el --- Testing output with while statements.

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
(require 'caredit)
(require 'test-utils)

(deftest caredit-beginning-of-balanced-statement--while-1
    "z; while (1) 2; d;\n"
  (goto-char (point-max))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "d;$"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "while (1) 2;"))

  (caredit-beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit-beginning-of-balanced-statement))

  (goto-char 15)
  (should (looking-at-p "; d;$"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "2; d;$"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "while (1) 2;")))

(deftest caredit-beginning-of-balanced-statement--while-2
    "z; while (1) { 2; } d;\n"
  (goto-char (point-max))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "d;$"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "while (1) { 2; }"))

  (caredit-beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit-beginning-of-balanced-statement))

  (goto-char 19)
  (should (looking-at-p "} d;$"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "2; } d;$"))

  (should-error (caredit-beginning-of-balanced-statement))

  (backward-up-list)
  (should (looking-at-p "{ 2; } d;"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "while (1) { 2; }")))

(provide 'while-tests)
;;; while-tests.el ends here
