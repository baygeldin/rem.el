;;; rem.el --- reactive memoization for Emacs Lisp. -*- lexical-binding: t -*-

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
(require 'ht)

;; Temporary

(defun m (msg)
  (message "%s" msg))

(defmacro rem-defview (name params &optional docstring &rest forms)
  "Define NAME as a new view with optional DOCSTRING.
PARAMS are used to render FORMS."
  (declare (indent defun))
  `(let ((rem--prev-hash (ht-create))
         (rem--next-hash (ht-create))
         (rem--deps-stack nil))
     (defun ,name ,params
       ,(if (stringp docstring) docstring)
       (prog1 (progn ,docstring ,@forms)
         (setq rem--prev-hash rem--next-hash)
         (setq rem--next-hash (make-hash-table :size (ht-size rem-prev-hash)))))))

(defun vertical-concat (a b)
  "vertical concat"
  (let* ((asplit (s-lines a))
         (bsplit (s-lines b))
         (amax (-max (-map 'length asplit))))
    (s-join "\n" (--map (concat (s-pad-right amax " " (car it)) (cdr it))
                        (-zip-fill (s-repeat amax " ") asplit bsplit)))))

(defmacro rem-defcomponent (name params &optional docstring &rest forms)
  "Define NAME as a new component with optional DOCSTRING.
PARAMS are used to render FORMS."
  (declare (indent defun))
  (let* ((fn (concat name "--fn"))
         (context '(rem--prev-hash rem--next-hash rem--deps-stack))
         (full-params (append context params))
         (form `(,fn ,full-params)))
    `(progn
       (defun ,fn ,full-params
         (when-let ((deps (car rem--deps-stack)))
           (push (list ,name ,params) deps))
         (if-let* ((component (ht-get rem--prev-hash ,name))
                   (memoized (ht-get component ,params)))
             ;;(ht-get rem--next-hash ,name)
             (ht-set! rem--next-hash ,name (make-hash-table :size (ht-size component)))
             ;;(ht-set! newhash ,params memoized)
           ())
         ;;
         ;; if current hash contains name -> params
         ;;    add memoized result + deps to new hash
         ;;    recursively do the same thing for all name -> params in deps
         ;;    return result
         ;; else
         ;;    add new list to deps stack
         ;;    call form, get result
         ;;    pop deps list from stack
         ;;    add current name -> params with result and deps to new hash
         ;;    return result
         )
       (defmacro ,name ,params ,(if (stringp docstring) docstring) ,form))))

(rem-defcomponent entry (e)
               (concat (entry-title e) (entry-desc e)))

(rem-defcomponent list (entries)
               (--map-indexed (entry it) entries))

(rem-defcomponent header ()
               "Hello!")

(rem-defcomponent body (store)
               (concat (header) (list (plist-get store :entries))))


(rem-bind "*my-buffer*" 'my-view '(action1 action2))

(provide 'org-retention)

;;; rem.el ends here
