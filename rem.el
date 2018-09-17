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

;; Private

(defun rem--s-center (len padding s)
  "If S is shorter than LEN, pad it with PADDING so it is centered."
  (declare (pure t) (side-effect-free t))
  (let* ((extra (max 0 (- len (length s))))
         (char (string-to-char padding))
         (left (make-string (ceiling extra 2) char))
         (right (make-string (floor extra 2) char)))
    (concat left s right)))

(defun rem--params-ht (root component &rest keyword-args)
  "Get hash table with params for COMPONENT in ROOT hash table.
Additional arguments are specified as keyword/argument pairs."
  (or (ht-get root component)
      ;; NOTE: not sure if 'equal test method is the best option here.
      ;; Since components are supposed to be pure, it should be enough
      ;; to compare keys in params hash table (i.e. lists of params)
      ;; shallowly, but 'equal seems to be deep. Requires investigation.
      (let ((params (apply 'make-hash-table :test 'equal keyword-args)))
        (ht-set! root component params)
        params)))

(defun rem--copy-memo (prev-hash next-hash name params)
  "Copy memoized data from PREV-HASH to NEW-HASH.
It copies results of rendering component NAME with PARAMS along with its dependencies."
  (let* ((prev-component (ht-get prev-hash name))
         (next-component (rem--params-ht next-hash name
                                      :size (ht-size prev-component)))
         (memoized (ht-get prev-component params)))
    (ht-set! next-component params memoized)
    (dolist (dependency (cdr memoized))
      (rem--copy-memo prev-hash next-hash (car dependency) (cdr dependency)))))

;; Core

;;;###autoload
(defmacro rem-defview (name params &optional docstring &rest forms)
  "Define NAME as a new view with an optional DOCSTRING.
PARAMS are used to render FORMS."
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name lambda-list [&optional stringp] def-body)))
  `(let ((rem--prev-hash (ht-create))
         (rem--next-hash (ht-create))
         (rem--deps-stack '(nil)))
     (defun ,name ,params
       ,(if (stringp docstring) docstring)
       (prog1 (progn ,docstring ,@forms)
         (setq rem--prev-hash rem--next-hash rem--deps-stack '(nil)
               rem--next-hash (make-hash-table :size (ht-size rem--prev-hash)))))))

;;;###autoload
(defmacro rem-defcomponent (name params &optional docstring &rest forms)
  "Define NAME as a new component with an optional DOCSTRING.
PARAMS are used to render FORMS."
  (declare (indent defun)
           (doc-string 2)
           (debug (&define symbolp lambda-list [&optional stringp] def-body)))
  (let* ((render (intern (format "%s-render" name)))
         (handler (intern (format "%s--handler" name)))
         (context '(rem--prev-hash rem--next-hash rem--deps-stack))
         (refs (--remove (or (eq it '&optional) (eq it '&rest)) params)))
    `(progn
       (defun ,render ,params ,docstring ,@forms)
       (defun ,handler ,(append context refs)
         (let ((args (list ,@refs)))
           (push (cons ',name args) (car rem--deps-stack))
           (-if-let* ((component (ht-get rem--prev-hash ',name))
                      (memoized (ht-get component args)))
               (prog1 (car memoized)
                 (rem--copy-memo rem--prev-hash rem--next-hash ',name args))
             (push nil rem--deps-stack)
             (let ((result (apply ',render args)))
               (ht-set! (rem--params-ht rem--next-hash ',name) args
                        (cons result (pop rem--deps-stack)))
               result))))
       (defmacro ,name ,params
         ,(if (stringp docstring) docstring)
         (let ((handler ',handler) (context ',context) (args (list ,@refs)))
           `(,handler ,@context ,@args))))))

;; Components

(rem-defcomponent rem-vconcat (left right)
  "Concatenate LEFT and RIGHT blocks of text vertically."
(rem-defcomponent rem-block (content &optional align filler)
  "Pad each line of CONTENT with a white-space to form a rectangular.
ALIGN defines how padding is done (either 'left, 'right or 'center).
FILLER is a character that is used to pad text (white-space by default)."
  (let* ((lines (s-lines content))
         (length (-max (-map 'length lines)))
         (pad (case align ('right 's-pad-left) ('center 'rem--s-center) (t 's-pad-right)))
         (filler (or filler " ")))
    (s-join "\n" (--map (funcall pad length filler it) lines))))

  (let* ((left-lines (s-lines left))
         (right-lines (s-lines right))
         (left-max (-max (-map 'length left-lines))))
    (s-join "\n" (--map (concat (s-pad-right left-max " " (car it)) (cdr it))
                        (-zip-fill (s-repeat left-max " ") left-lines right-lines)))))

(rem-defcomponent entry (e)
  (print (format "entry called with %s" e))
  (format "%s: %s." (car e) (cdr e)))

(rem-defcomponent entry-list (entries)
  (print (format "entry list called"))
  (s-join "\n" (--map (entry it) entries)))

(rem-defcomponent header ()
  (print (format "header called"))
  "Hello!")

(rem-defcomponent body (entries)
  (print (format "body called"))
  (concat (header) (entry-list entries)))

(rem-defview view ()
  (body entries))

(setq entries nil)
(setq i 0)

(defun add-entry ()
  (setq entries (cons (cons (format "title-%s" i) (format "description-%s" i)) entries))
  (setq i (+ i 1))
  (view))

(add-entry)

(defun rem-update (buffer view &optional save-point)
  "Replace BUFFER contents with the result of calling VIEW.
If BUFFER doesn't exist, create one.
SAVE-POINT is a function that is called right before updating buffer contents
and returns an integer, a (row . column) cons or a lambda that returns one of these.
In case SAVE-POINT returned a lambda, it's called right after updating buffer contents.
The result is used to set the pointer. By default it restores previous row and column."
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t)
          (pos (if save-point (save-point)
                   (cons (line-number-at-pos) (current-column)))))
      (erase-buffer)
      (insert (funcall view))
      (let ((pos (if (functionp pos) (pos) pos)))
        (if (integerp pos) (goto-char pos)
          (goto-char (point-min))
          (forward-line (1- (car pos)))
          (ignore-errors (forward-char (cdr pos)))))
      (let ((p (point)))
        (--each (get-buffer-window-list buffer) (set-window-point it p))))))

(defun rem-bind (buffer view actions &optional save-point)
  "Advise `rem-update' for BUFFER, VIEW and optional SAVE-POINT after ACTIONS."
  (let ((handler (lambda () (rem-update buffer view save-point))))
    (dolist (fn actions) (advice-add fn :after handler))))

(rem-bind "*my-buffer*" 'view '(add-entry))

(provide 'org-retention)

;;; rem.el ends here
