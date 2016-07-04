;;; predicate-tests.el --- Test predicates

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
;; caredit--in-string-p
;; caredit--in-comment-p
;; caredit--in-char-p

;;; Code:
(require 'caredit)
(require 'ert)
(require 'test-utils)

(deftest caredit--in-string-p--predicate-1 "asdf \"jkl\";"
  ;; `asdf ' are all not strings
  (while (/= ?\" (char-after))
    (should-not (caredit--in-string-p))
    (forward-char))

  (should (looking-at-p "\"jkl\";$"))

  (should-not (caredit--in-string-p))
  (forward-char)

  (should (looking-at-p "jkl\";$"))

  ;; `jkl"' are all strings
  (while (/= ?\" (char-after))
    (should (caredit--in-string-p))
    (forward-char))

  (should (looking-at-p "\";$"))

  (should (caredit--in-string-p))
  (forward-char)

  (should (looking-at-p ";$"))
  (should-not (caredit--in-string-p))

  (forward-char)
  (should-not (caredit--in-string-p)))

(deftest caredit--in-comment-p--predicate-1 "// asdf\n"
  ;; `//' aren't
  (should-not (caredit--in-comment-p))
  (forward-char)
  (should-not (caredit--in-comment-p))
  (forward-char)

  (should (looking-at-p " asdf$"))
  (while (not (eolp))
    (should (caredit--in-comment-p))
    (forward-char)))

(deftest caredit--in-char-p--predicate-1 "asdf 'j';\n"
  ;; `asdf ' are all not strings
  (while (/= ?\' (char-after))
    (should-not (caredit--in-char-p))
    (forward-char))

  (should (looking-at-p "'j';$"))

  (should-not (caredit--in-char-p))
  (forward-char)

  (should (looking-at-p "j';$"))

  ;; `j'' are all strings
  (while (/= ?\' (char-after))
    (should (caredit--in-char-p))
    (forward-char))

  (should (looking-at-p "';$"))

  (should (caredit--in-char-p))
  (forward-char)

  (should (looking-at-p ";$"))
  (should-not (caredit--in-char-p))

  (forward-char)
  (should-not (caredit--in-char-p)))

(provide 'predicate-tests)
;;; predicate-tests.el ends here
