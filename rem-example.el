;; -*- lexical-binding: t -*-

(require 'rem)

;; Store

(defvar todo-store
  '(:input "" :entries nil :filter nil :index 0)
  "Main application store.")

(defun todo-get (key)
  "Get KEY in store."
  (plist-get todo-store key))

(defun todo-set (key value)
  "Set KEY in store."
  (setq todo-store (plist-put todo-store key value)))

(defmacro todo-mutate (key form)
  "Mutate KEY in store with FORM."
  (declare (debug (symbolp form)))
  `(let ((it (todo-get ,key))) (todo-set ,key ,form)))

;; Actions

(defun todo-input ()
  "Input CHAR."
  (interactive)
  (todo-mutate :input (concat it (char-to-string last-command-event))))

(defun todo-backspace ()
  "Delete a character from input."
  (interactive)
  (todo-mutate :input (s-left (1- (length it)) it)))

(defun todo-add ()
  "Add a new entry."
  (interactive)
  (let ((input (todo-get :input)))
    (unless (s-blank? input)
      (todo-mutate :entries (push (cons input nil) it))
      (todo-set :input ""))))

(defun todo-toggle ()
  "Complete selected entry if it's active and vice versa."
  (interactive)
  (todo-mutate :entries (-replace todo-selected
                                  (cons (car todo-selected)
                                        (not (cdr todo-selected)))
                                  it)))

(defun todo-up ()
  "Go one entry up."
  (interactive)
  (todo-mutate :index (max 0 (1- it))))

(defun todo-down ()
  "Go one entry down."
  (interactive)
  (todo-mutate :index (min (1- (length todo-list)) (1+ it))))

(defun todo-show-all ()
  "Show all entries."
  (interactive)
  (todo-set :index 0)
  (todo-set :filter nil))

(defun todo-show-completed ()
  "Show only completed entries."
  (interactive)
  (todo-set :index 0)
  (todo-set :filter 'completed))

(defun todo-show-active ()
  "Show only active entries."
  (interactive)
  (todo-set :index 0)
  (todo-set :filter 'active))

;; View

(defvar todo-selected nil "A reference to the currently selected entry.")

(defvar todo-list nil "A reference to the currently shown entries.")

(rem-defcomponent todo-header ()
  "Header."
  (rem-block "todos" :width 25 :halign 'middle
             :props '(face (:weight extra-bold
                                    :foreground "white"
                                    :background "pink"))
             :border-props '(face (:background "pink"))))

(rem-defcomponent todo-input-field (content)
  "Input field with CONTENT."
  (rem-block content :width 25
             :props '(face (:weight bold
                                    :foreground "pink"
                                    :background "white"))
             :border '(:bottom 1)
             :border-props '(face (:foreground "white" :background "pink"))
             :border-filler "="))

(rem-defcomponent todo-entry (entry selected)
  "ENTRY. Highlighted if SELECTED."
  (when selected (setq todo-selected entry))
  (rem-block (car entry) :width 25
             :props `(face (:background ,(if selected "pink" "white")
                                        :strike-through ,(cdr entry)
                                        :foreground "black"))))

(rem-defcomponent todo-list (entries filter index)
  "List with ENTRIES. FILTER is applied. INDEX entry is selected"
  (let ((entries (cond ((eq filter 'active) (--remove (cdr it) entries))
                       ((eq filter 'completed) (--select (cdr it) entries))
                       (t entries))))
    (setq todo-list entries)
    (rem-join 'column 'left (--map-indexed
                             (todo-entry it (eq index it-index))
                             entries))))
(rem-defview todo-view ()
  "Main application view."
  (rem-block
   (rem-block (rem-join 'column 'left
                        (todo-header)
                        (todo-input-field (todo-get :input))
                        (todo-list (todo-get :entries)
                                   (todo-get :filter)
                                   (todo-get :index)))
              :border '(:left 1 :bottom 1 :right 1)
              :border-props '(face (:background "pink")))
   :border 2))

;; Major mode

(defconst todo-buffer "*todo-buffer*" "Main application buffer.")

(defvar todo-mode-map
  (let ((map (make-sparse-keymap)))
    (--each (number-sequence ?\s 255) (define-key map (vector it) 'todo-input))
    (define-key map (kbd "DEL") 'todo-backspace)
    (define-key map (kbd "C-c C-c") 'todo-show-all)
    (define-key map (kbd "C-c a") 'todo-show-active)
    (define-key map (kbd "C-c c") 'todo-show-completed)
    (define-key map (kbd "C-c t") 'todo-toggle)
    (define-key map (kbd "<up>") 'todo-up)
    (define-key map (kbd "<down>") 'todo-down)
    (define-key map (kbd "RET") 'todo-add)
    map)
  "Keymap for `todo-mode'.")

(define-derived-mode todo-mode
  special-mode  "ToDo"
  "Application major mode.
\\{todo-mode-map}")

(defun todo-init ()
  "Initialize the application."
  (interactive)
  (with-current-buffer (get-buffer-create todo-buffer)
    (evil-set-initial-state 'nov-mode 'emacs)
    (todo-mode)))

(rem-bind todo-buffer 'todo-view
          '(todo-init todo-input todo-backspace todo-add todo-toggle
                      todo-up todo-down todo-show-all todo-show-completed
                      todo-show-active)
          (lambda () 0))
