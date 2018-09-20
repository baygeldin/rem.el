(require 'rem)

;; Helpers

(defun get (key)
  "Get KEY in store."
  (plist-get key todo-store))

(defun set (key value)
  "Set KEY in store."
  (setq todo-store (plist-put todo-store key value)))

(defmacro mutate (key form)
  "Mutate KEY in store with FORM."
  `(let ((it (get ,key))) (set ,key ,form)))

;; Store

(defvar todo-store '(:input "" :entries nil :current nil))

;; Actions

(defun todo-input (char)
  "Input CHAR."
  (mutate :input (concat it char)))

(defun todo-delete ()
  "Delete a character."
  (mutate :input (s-left (1- (length it) it))))

(defun todo-add ()
  "Add a new entry."
  (mutate :entries (append it (cons (get :input) nil)))
  (set :input ""))

(defun todo-toggle (entry))

(defun todo-show-all ())

(defun todo-show-completed ())

(defun todo-show-active ())

(rem-defcomponent todo-entry (entry)
  "Entry input field."
  (rem-block ))

(rem-defcomponent entry (e)
                  (print (format "entry called with %s" e))
                  (format "%s: %s." (propertize (car e) 'italic) (cdr e)))

(rem-defcomponent entry-list (entries)
                  (print (format "entry list called"))
                  (s-join "\n" (--map (entry it) entries)))

(rem-defcomponent header ()
                  (print (format "header called"))
                  (propertize "Hello!" 'face '(:foreground "red")))

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

(rem-bind "*my-buffer*" 'view '(add-entry))

(add-entry)

(with-current-buffer "*my-buffer*"
  (erase-buffer)
  ;; (insert (rem-padding "hello\nbih" -1 '(:background "pink")))
  (let ((b1 (rem-block-render "hehe biiiih hello"
                              :border '(:top 2 :bottom 2 :left 1 :right 1) :border-filler "f"
                              :border-props '(face (:background "pink"))
                              :min-height 5
                              :width 10
                              :valign 'middle
                              :halign 'right))
        (b2 (rem-block-render "yeahyeah"
                              :border 1
                              ))
        (b3 (rem-block-render "HAHAHHAHAH"
                              :border 2
                              :border-props '(face (:background "yellow"))
                              )))
    (insert (rem-join-render 'row 'end (rem-join-render 'column 'start b1 b2 b3) b1 b2))))
