;;; rm.el --- reactive memoization for Emacs Lisp. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Baygeldin

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Alexander Baygeldin <a.baygeldin@gmail.com>
;; Keywords: reactive, memoization, react, view, component, ui
;; Version: 0.0.1

;;; Commentary:

;; Reactive memoization for Emacs Lisp.

;;; Code:

(require 'dash)
(require 's)

;; Temporary

(defun m (msg)
  (message "%s" msg))

(provide 'org-retention)

;;; rm.el ends here
