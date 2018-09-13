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
  "Define NAME as a new view with an optional DOCSTRING.
PARAMS are used to render FORMS."
  (declare (indent defun))
  `(let ((rem--prev-hash (ht-create))
         (rem--next-hash (ht-create))
         (rem--deps-stack '(nil)))
     (defun ,name ,params
       ,(if (stringp docstring) docstring)
       (prog1 (progn ,docstring ,@forms)
         (setq rem--prev-hash rem--next-hash rem--deps-stack '(nil)
               rem--next-hash (make-hash-table :size (ht-size rem--prev-hash)))))))

(defun vertical-concat (a b)
  "vertical concat"
  (let* ((asplit (s-lines a))
         (bsplit (s-lines b))
         (amax (-max (-map 'length asplit))))
    (s-join "\n" (--map (concat (s-pad-right amax " " (car it)) (cdr it))
                        (-zip-fill (s-repeat amax " ") asplit bsplit)))))

(defun rem--child-ht (parent name &rest params)
  "Get child hash table of PARENT by NAME or create it with PARAMS."
  (or (ht-get parent name)
      (let ((child (apply 'make-hash-table params)))
        (ht-set! parent name child)
        child)))

(defun rem--copy-memo (prev-hash next-hash name params)
  "Copy memoized data from PREV-HASH to NEW-HASH.
It copies results of rendering component NAME with PARAMS along with its dependencies."
  (let* ((prev-component (ht-get prev-hash name))
         (next-component (rem--child-ht next-hash name
                                     :size (ht-size prev-component)))
         (memoized (ht-get prev-component params)))
    (ht-set! next-component params memoized)
    (dolist (dependency (cdr memoized))
      (rem--copy-memo prev-hash next-hash (car dependency) (cdr dependency)))))

(defmacro rem-defcomponent (name params &optional docstring &rest forms)
  "Define NAME as a new component with an optional DOCSTRING.
PARAMS are used to render FORMS."
  (declare (indent defun))
  (let* ((fn (intern (format "%s--fn" name)))
         (context '(rem--prev-hash rem--next-hash rem--deps-stack)))
    `(progn
       (defun ,fn ,(append context params)
         (push (cons ',name ',params) (car rem--deps-stack))
         (-if-let* ((component (ht-get rem--prev-hash ',name))
                    (memoized (ht-get component ',params)))
             (progn
               (rem--copy-memo rem--prev-hash rem--next-hash ',name ',params)
               (car memoized))
           (push nil rem--deps-stack)
           (let ((result (progn ,docstring ,@forms)))
             (ht-set! (rem--child-ht rem--next-hash ',name) ',params
                      (cons result (pop rem--deps-stack)))
             result)))
       (defmacro ,name ,params
         ,(if (stringp docstring) docstring)
         (let ((fn ',fn) (context ',context) (args (list ,@params)))
           `(,fn ,@context ,@args))))))

(rem-defcomponent entry (e)
  (format "%s: %s." (car e) (cdr e)))

(rem-defcomponent entry-list (entries)
  (s-join "\n" (--map (entry it) entries)))

(rem-defcomponent header ()
  "Hello!")

(rem-defcomponent body (entries)
  (concat (header) (entry-list entries)))

(rem-defview view ()
  (body entries))

(setq entries nil)
(setq i 0)

(defun add-entry ()
  (setq entries (cons (cons (format "title-%s" i) (format "description-%s" i)) entries))
  (setq i (+ i 1))
  (print (view)))

(add-entry)

;; (rem-bind "*my-buffer*" 'my-view '(action1 action2))

(provide 'org-retention)

;;; rem.el ends here
