;;; slurp-tests.el --- Test slurp functions

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

;;; Commentary:
;; caredit-open-angle
;; caredit-open-curly
;; caredit-open-string
;; caredit-open-paren
;; caredit-open-char
;; caredit-open-square

;;; Code:
(require 'caredit)
(require 'ert)
(require 'test-utils)

(deftest caredit--slurp-forward-argument--slurp-1 "f(g(a, b), c, d);"
  ;; At bob, not in function args.  Slurp function arg should error.
  (should-error (caredit--slurp-forward-argument))
  (should (= (point) (point-min)))
  (should (looking-at-p "f(g(a, b), c, d);$"))

  (goto-char 5)
  (should (looking-at-p "a, b), c, d);$"))

  (caredit--slurp-forward-argument)
  (should (looking-at-p "a, b, c), d);$"))

  (caredit--slurp-forward-argument)
  (should (looking-at-p "a, b, c, d));$"))

  (should-error (caredit--slurp-forward-argument))
  (should (looking-at-p "a, b, c, d));$"))

  (should-bob-match "f(g(a, b, c, d));$"))

(deftest caredit--slurp-forward-statement--slurp-1
    "{ a; b; } c; d, e;"
  ;; Not in a block, should error
  (should-error (caredit--slurp-forward-statement))

  (forward-char)
  (should (looking-at-p " a; b; } c; d, e;$"))

  (caredit--slurp-forward-statement)
  (should (looking-at-p " a; b; c; } d, e;$"))

  (caredit--slurp-forward-statement)
  (should (looking-at-p " a; b; c; d, e; }$"))

  (should-bob-match "{ a; b; c; d, e; }$")

  (should-error (caredit--slurp-forward-statement)))

(deftest caredit--slurp-forward-statement--slurp-block
    "void f() {
  {
    a;
  }
  {
    b;
    c;
  }
}"
  (forward-line 2)
  (back-to-indentation)
  (should (looking-at-p "a;
  }
  {
    b;$"))

  (caredit--slurp-forward-statement)
  (should (looking-at-p "a;
    {
      b;
      c;
    }
  }
}$")))

(deftest caredit--slurp-forward-string--slurp-1
    "\"baba\" asdf"
  (goto-char (point-max))
  (should-error (caredit--slurp-forward-string))

  (goto-char (point-min))
  (should-error (caredit--slurp-forward-string))

  (forward-char)
  (should (looking-at-p "baba\" asdf$"))

  (caredit--slurp-forward-string)
  (should-bob-match "\"baba asdf\"$")

  (should-error (caredit--slurp-forward-string))
  (should-bob-match "\"baba asdf\"$"))

(provide 'slurp-tests)
;;; slurp-tests.el ends here
