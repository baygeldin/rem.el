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

(defun vertical-concat (a b)
  "vertical concat"
  (let* ((asplit (s-lines a))
         (bsplit (s-lines b))
         (amax (-max (-map 'length asplit))))
    (s-join "\n" (--map (concat (s-pad-right amax " " (car it)) (cdr it))
                        (-zip-fill (s-repeat amax " ") asplit bsplit)))))

(defmacro defcomponent (name params form)
  `(defun ,(concat name "-fn") ,(append '(blabla-prev blabla-next blabla-stack) params)
     ;; add name + params to current deps list (if exists)
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
     ;;
     ;; questions: how to access current hash, previous hash and deps stack?
     ;; maybe components should actually be macros that expand in component-fn function call with additional params: hashes and stack.
     ;; in order to render the component one would need to create an object with hashes and stack and call root component fn with them.
     ;; after that it should swap hashes and clear deps stack. also it should copy hash size from previous hash.
     ,form)
  (let ((form `(,(concat name "-fn") blabla--prev blabla--next blabla-stack)))
    `(defmacro ,name ,params ,form)))

(rem-defcomponent entry (e)
  (concat (entry-title e) (entry-desc e)))

(rem-defcomponent list (entries)
  (--map-indexed (entry it) entries))

(rem-defcomponent header ()
  "Hello!")

(rem-defcomponent body (store)
  (concat (header) (list (plist-get store :entries))))

(defmacro rem-defview (name params &optional docstring &rest forms)
  (declare (indent defun))
  `(let ((rem--prev-hash (ht-create))
         (rem--next-hash (ht-create))
         (rem--deps-stack nil))
     (defun ,name ,params
       ,docstring
       (prog1 (progn ,@forms)
         ;; NOTE: it would probably be better performance-wise to create
         ;; a new hash table with the same size as the previous hash table,
         ;; but `ht-create' doesn't provide such functionality for now.
         (setq rem--prev-hash rem--next-hash rem--next-hash (ht-create))))))

(rem-defview my-view ()
  "kek"
  (m rem--prev-hash)
  (ht-set! rem--next-hash 'lol 5))

(my-view)

(rem-bind "*my-buffer*" 'my-view '(action1 action2))

(provide 'org-retention)

;;; rem.el ends here
