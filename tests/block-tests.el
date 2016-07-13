;;; block-tests.el --- Test interaction between code blocks and commands.

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
(require 'test-utils)
(require 'caredit)

(deftest caredit-end-of-balanced-statement--block-1
    "a;
{
  b;
  c;
}
d;\n"
  (caredit-end-of-balanced-statement)
  (should (looking-at-p "\n{\n  b;\n"))

  (caredit-end-of-balanced-statement)
  (should (looking-at-p "\nd;\n"))

  (caredit-end-of-balanced-statement)
  (should (eolp))

  (should-not
   (caredit--does-point-move
     (should-error (caredit-end-of-balanced-statement))))

  (forward-char)
  (should (eobp)))

(deftest caredit-beginning-of-balanced-statement--block-1
    "a;
{
  b;
  c;
}
d;\n"
  (goto-char (point-max))
  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "d;\n"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "{\n  b;\n"))

  (caredit-beginning-of-balanced-statement)
  (should (looking-at-p "a;\n{\n  b;\n"))
  (should (bobp))

  (should-not
   (caredit--does-point-move
     (should-error (caredit-beginning-of-balanced-statement)))))

(provide 'block-tests)
;;; block-tests.el ends here
