;;; caredit-mappings.el --- Key mapping functions

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
(require 'caredit-core)

(defmacro caredit--map-key (open close name)
  "Define keys in local buffer for inserting balanced OPEN CLOSE pairs, to functions with NAME postfixes.

OPEN is set to caredit-open-NAME,
M-OPEN is set to caredit-wrap-NAME,
CLOSE is set to caredit-close-NAME."
  `(progn
     (caredit--map-double-key ,open ,name)
     (local-set-key (kbd (concat (list ,close)))
                    (caredit--conc-name "caredit-close-" ,name))))

(defmacro caredit--map-double-key (open name)
  "Define keys in local buffer for inserting balanced OPEN (CLOSE) pairs, to functions with NAME postfixes.

This facilitates DRY principles by extracting out common logic
with `caredit--map-key' for NAME=string and NAME=char bindings.

OPEN is set to caredit-open-NAME,
M-OPEN is set to caredit-wrap-NAME."
  `(progn
     (local-set-key (kbd (concat (list ,open)))
                    (caredit--conc-name "caredit-open-" ,name))
     (local-set-key (kbd (concat "M-" (list ,open)))
                    (caredit--conc-name "caredit-wrap-" ,name))))

(defun caredit-use-default-mappings-this-buffer ()
  "Use caredit default mappings in this buffer."
  (interactive)

  (local-set-key (kbd "C-)") 'caredit-slurp-forward)
  ;; (local-set-key (kbd "C-}") 'caredit-barf-forward)

  (caredit--map-key ?\( ?\) "paren")
  (caredit--map-key ?\{ ?\} "curly")
  (caredit--map-key ?\[ ?\] "square")
  (caredit--map-double-key ?\" "string")
  (caredit--map-double-key ?\' "char")

  (local-set-key (kbd ";") 'caredit-semicolon))

(provide 'caredit-mappings)
;;; caredit-mappings.el ends here
