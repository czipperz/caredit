;;; open-tests.el --- Test open pair functions

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

(deftest caredit-open-angle--open-1 "\"asdf\""
  (caredit-open-angle)
  (should (looking-at-p ">\"asdf\"$"))
  (should (= (point) (+ 1 (point-min))))
  (should-bob-match "<>\"asdf\"$")

  (forward-char 2)
  (should (looking-at-p "asdf\"$"))
  (should (caredit--in-string-p))

  (caredit-open-angle)
  (should (looking-at-p "asdf\"$"))
  (should-bob-match "<>\"<asdf\"$"))

(deftest caredit-open-curly--open-1 "\"asdf\""
  (caredit-open-curly)
  (should (looking-at-p "}\"asdf\"$"))
  (should (= (point) (+ 1 (point-min))))
  (should-bob-match "{}\"asdf\"$")

  (forward-char 2)
  (should (looking-at-p "asdf\"$"))
  (should (caredit--in-string-p))

  (caredit-open-curly)
  (should (looking-at-p "asdf\"$"))
  (should-bob-match "{}\"{asdf\"$"))

(deftest caredit-open-square--open-1 "\"asdf\""
  (caredit-open-square)
  (should (looking-at-p "\\]\"asdf\"$"))
  (should (= (point) (+ 1 (point-min))))
  (should-bob-match "\\[\\]\"asdf\"$")

  (forward-char 2)
  (should (looking-at-p "asdf\"$"))
  (should (caredit--in-string-p))

  (caredit-open-square)
  (should (looking-at-p "asdf\"$"))
  (should-bob-match "\\[\\]\"\\[asdf\"$"))

(deftest caredit-open-paren--open-1 "\"asdf\""
  (caredit-open-paren)
  (should (looking-at-p ")\"asdf\"$"))
  (should (= (point) (+ 1 (point-min))))
  (should-bob-match "()\"asdf\"$")

  (forward-char 2)
  (should (looking-at-p "asdf\"$"))
  (should (caredit--in-string-p))

  (caredit-open-paren)
  (should (looking-at-p "asdf\"$"))
  (should-bob-match "()\"(asdf\"$"))

(deftest caredit-open-string--open-1 "\"asdf\""
  (caredit-open-string)
  (should (looking-at-p "\"\"asdf\"$"))
  (should (= (point) (+ 1 (point-min))))
  (should-bob-match "\"\"\"asdf\"$")

  (forward-char 2)
  (should (looking-at-p "asdf\"$"))
  (should (caredit--in-string-p))

  (caredit-open-string)
  (should (looking-at-p "asdf\"$"))
  (should-bob-match "\"\"\"\\\\\"asdf\"$"))

(provide 'open-tests)
;;; open-tests.el ends here
