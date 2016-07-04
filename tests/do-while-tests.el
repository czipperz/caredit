;;; do-while-tests.el --- Testing output with do while statements.

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
;; `do {} while ();' statements are particularly hard to parse.  These
;; tests are to test the integration of various functions with them.

;;; Code:
(require 'caredit)
(require 'test-utils)

(deftest caredit--beginning-of-balanced-statement--do-while-1
    "z; do { a; b; c; } while (0); d; e;\n"
  (goto-char (point-max))

  (caredit--beginning-of-balanced-statement)
  (should (looking-at-p "e;$"))

  (caredit--beginning-of-balanced-statement)
  (should (looking-at-p "d; e;$"))

  (caredit--beginning-of-balanced-statement)
  (should (looking-at-p "do { a; b; c; } while (0); d; e;$"))

  (caredit--beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit--beginning-of-balanced-statement)))

(deftest caredit--beginning-of-balanced-statement--do-while-2
    "z; do a; while (0); d; e;\n"
  (goto-char (point-max))

  (caredit--beginning-of-balanced-statement)
  (should (looking-at-p "e;$"))

  (caredit--beginning-of-balanced-statement)
  (should (looking-at-p "d; e;$"))

  (caredit--beginning-of-balanced-statement)
  (should (looking-at-p "do a; while (0); d; e;$"))

  (caredit--beginning-of-balanced-statement)
  (should (bobp))

  (should-error (caredit--beginning-of-balanced-statement)))

(deftest caredit--end-of-balanced-statement--do-while-1
    "z; do { a; b; c; } while (0); d; e;\n"
  (caredit--end-of-balanced-statement)
  (should (looking-at-p " do { a; b; c; } while (0); d; e;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " d; e;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " e;$"))

  (caredit--end-of-balanced-statement)
  (should (eolp))

  (should-error (caredit--end-of-balanced-statement)))

(deftest caredit--end-of-balanced-statement--do-while-2
    "z; do a; while (0); d; e;\n"
  (caredit--end-of-balanced-statement)
  (should (looking-at-p " do a; while (0); d; e;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " d; e;$"))

  (caredit--end-of-balanced-statement)
  (should (looking-at-p " e;$"))

  (caredit--end-of-balanced-statement)
  (should (eolp))

  (should-error (caredit--end-of-balanced-statement)))

(provide 'do-while-tests)
;;; do-while-tests.el ends here
