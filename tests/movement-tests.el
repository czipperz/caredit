;;; movement-tests.el --- Test movement functions

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
(require 'caredit)
(require 'ert)
(require 'test-utils)

(deftest caredit--beginning-of-statement--movement-1
    "do { a, b; c; } d, e;\n"
  (goto-char (point-max))

  (caredit--beginning-of-statement)
  (should (looking-at-p "d, e;$"))

  (caredit--beginning-of-statement)
  (should (looking-at-p "c; } d, e;$"))

  (caredit--beginning-of-statement)
  (should (looking-at-p "a, b; c; } d, e;$"))

  (caredit--beginning-of-statement)
  (should (looking-at-p "{ a, b; c; } d, e;$"))

  (caredit--beginning-of-statement)
  (should (bobp)))

(deftest caredit--end-of-statement--movement-1
    "do { a, b; c; } d, e;\n"

  (caredit--end-of-statement)
  (should (looking-at-p " { a, b; c; } d, e;$"))

  (caredit--end-of-statement)
  (should (looking-at-p " c; } d, e;$"))

  (caredit--end-of-statement)
  (should (looking-at-p " } d, e;$"))

  (caredit--end-of-statement)
  (should (looking-at-p " d, e;$"))

  (caredit--end-of-statement)
  (should (eolp)))

(deftest caredit-beginning-of-balanced-statement--movement-1
    "while (0) { a, b; c; } d, e;\n"
  (goto-char (point-max))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "d, e;$"))

  (caredit-beginning-of-balanced-statement)
  (should (bobp))
  (should (looking-at-p "while (0) { a, b; c; } d, e;$"))

  (should-error (caredit-beginning-of-balanced-statement))
  (should (bobp)))

(deftest caredit--end-of-balanced-statement--movement-1
    "while (0) { a, b; c; } d, e;\n"

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " d, e;$"))

  (caredit--end-of-balanced-statement)
  (should (eolp)))

(provide 'movement-tests)
;;; movement-tests.el ends here
