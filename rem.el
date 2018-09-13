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
         ;; here params are for documentation...
         ;; but for each param here we have to get its data and merge with context params
         ;; smth like (-map 'symbol-value ',params) but that works with lexical value
         ,(if (stringp docstring) docstring)
         ;; NOTE: possibly exploiting a bug here:
         ;; https://stackoverflow.com/questions/17394638/nesting-backquote-and-in-emacs-lisp
         '(,fn ,@context ,@(--map (intern (format ",%s" it)) params))
         ))))

(defmacro foo (params)
  (let ((shit (--map (intern (format ",%s" it)) params))
        (fuck `(bar--fn prefix ,@params)))
    `(progn
       (defun bar--fn ,(append '(prefix) params)
         (format "I got your prefix %s" prefix))
       (defmacro bar ,params
         '(bar--fn prefix )
         '(apply 'bar--fn (append '(prefix) (--map (eval it t) (quote ,params))))))))


;; when I call
(foo (a b c))
;; I want it to define the following macro
(defmacro bar (a b c)
  `(bar--fn prefix ,a ,b ,c))
;; what kind of sorcery do I need?
(defmacro foo (params)
  (let ((xyz ()))
    `(defmacro bar ,params
       '(bar--fn prefix ,@xyz))))
;; if nested macro won't be able to output actual values it receives
;; then values are practically lost and there's now way to retrieve
;; (with whatever eval sorcery you decide to use)

(foo (a b c))
(let ((prefix 1))
  (bar 1 2 3))
(macroexpand '(bar 1 2 3))
(setq params '(a b c))
(eval ``(bar-fn prefix ,@,params))
(eval (print ``(a ,,(+ 1 2))))

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
