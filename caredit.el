;;; caredit.el --- basic paredit for c-like languages

;; Copyright (C) 2005--2016 Taylor R. Campbell
;; Copyright (C) 2013-2015 zk_phi
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

;;; External Requires:
(require 'cl-lib)

;;; Internal Requires:
(require 'caredit-core)

(require 'caredit-beginning-of-balanced-statement)
(require 'caredit-end-of-balanced-statement)

(require 'caredit-mappings)

(provide 'caredit)
;;; caredit.el ends here
