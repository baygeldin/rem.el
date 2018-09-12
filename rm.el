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

(defun vertical-concat (a b)
  "vertical concat"
  (let* ((asplit (s-lines a))
         (bsplit (s-lines b))
         (amax (-max (-map 'length asplit))))
    (s-join "\n" (--map (concat (s-pad-right amax " " (car it)) (cdr it))
                        (-zip-fill (s-repeat amax " ") asplit bsplit)))))

(defmacro defcomponent (name params form)
  `(defun ,(concat name "-fn") ,(append '(blabla-prev blabla-next blabla-stack) params)
     ;; add name + params to current deps list
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

(rm-defcomponent entry (e)
  (concat (entry-title e) (entry-desc e)))

(rm-defcomponent list (entries)
  (--map-indexed (entry it) entries))

(rm-defcomponent header ()
  "Hello!")

(rm-defcomponent body (store)
  (concat (header) (list (plist-get store :entries))))

(rm-defview my-view ()
  (body store))

(defmacro rm-defview (name params &optional docstring &rest forms)
  ;; it should also provide local state...
  `(let ((blabla-prev)
         (blabla-next)
         (blabla-stack))
     ;; check if docstring stringp if not -- merge with forms
     ;; execute forms with progn
     ;; return last result with prog1
     ;; setq blabla-prev to blabla-next, set blabla-next to new hash with same size
     (defun ,name ,params ,docstring ,forms)))

(let ((kek "kek"))
  (defun lala ()
    (message kek)
    (setq kek (concat kek kek))))

(lala) ;; kekekkekekeke!
(message kek) ;; void variable! yuh!

(my-view)

(rm-bind "*my-buffer*" 'my-view '(action1 action2))



(provide 'org-retention)

;;; rm.el ends here
