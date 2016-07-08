;;; test-utils.el --- Test utilities

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
;;; Code:
(require 'cc-mode)
(require 'ert)
(require 'font-lock)

(defmacro should-bob-match (regex)
  "Assert that bob matches REGEX.

This is a macro so that in ERT errors, REGEX will be displayed
instead of 'regex'"
  (declare (indent 1))
  `(should
    (save-excursion
      (goto-char (point-min))
      (looking-at-p ,regex))))

(defmacro deftest (test-name file-contents &rest body)
  "Define a test TEST-NAME that inserts FILE-CONTENTS then runs BODY.

Sets up c++-mode, font locking, and goes to bob."
  (declare (indent 2))
  `(ert-deftest
       ,test-name ()
       (with-temp-buffer
         (c++-mode)
         (setq-local font-lock-beginning-of-syntax-function
                     'beginning-of-buffer)
         (font-lock-mode)
         (insert ,file-contents)
         (font-lock-fontify-block)
         (goto-char (point-min))
         ,@body)))

(provide 'test-utils)
;;; test-utils.el ends here
