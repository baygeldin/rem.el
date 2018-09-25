;; -*- lexical-binding: t -*-

(require 'rem)

(describe "view"
  (it "should memoize results for components"
    (setq counter 0)
    (rem-defcomponent component (a b &optional c d &rest e)
      (setq counter (1+ counter))
      (apply '+ a b c d e))
    (rem-defview view (f) (component 1 2 3 4 5 6 f))
    (view 7)
    (view 7)
    (expect counter :to-be 1))

  (it "should memoize results for components only for the previous render"
    (setq counter 0)
    (rem-defcomponent component (bar)
      (setq counter (1+ counter))
      (concat "foo" bar))
    (rem-defview view (bar) (component bar))
    (view "bar")
    (view "xyz")
    (view "bar")
    (expect counter :to-be 3))

  (it "should memoize results for components that were re-used indirectly"
    (setq item-counter 0)
    (setq list-counter 0)
    (rem-defcomponent item-component (entry)
      (setq item-counter (1+ item-counter))
      entry)
    (rem-defcomponent list-component (entries)
      (setq list-counter (1+ list-counter))
      (apply 'concat (--map (item-component it) entries)))
    (rem-defview view (entries) (list-component entries))
    (view '("foo" "bar"))
    (view '("foo" "bar"))
    (view '("bar" "foo"))
    (expect list-counter :to-be 2)
    (expect item-counter :to-be 2))

  (it "should not interfere with memoization of other views"
    (setq counter 0)
    (rem-defcomponent component () (setq counter (1+ counter)))
    (rem-defview first-view () (component))
    (rem-defview second-view () (component))
    (first-view)
    (second-view)
    (expect counter :to-be 2))

  (it "should not consider equal strings with different properties equal"
    (setq counter 0)
    (rem-defcomponent component (str)
      (setq counter (1+ counter))
      str)
    (rem-defview view (str) (component str))
    (view "foo")
    (view (propertize "foo" 'face 'italic))
    (view (propertize "foo" 'face 'bold))
    (expect counter :to-be 3)))

(describe "component"
  (describe "block"
    (it "should return a single line unchanged"
      (rem-defview view ()
        (rem-block "foo"))
      (expect (view) :to-equal "foo"))

    (it "should make all lines the same length"
      (rem-defview view ()
        (rem-block "f\nfo\nfoo"))
      (expect (view) :to-equal "f  \nfo \nfoo"))

    (it "should support all stated parameters"
      (rem-defview view ()
        (rem-block "foo bar"
                   :halign 'middle
                   :valign 'middle
                   :filler "+"
                   :props '(face italic)
                   :border '(:top 1 :left 1)
                   :border-filler "="
                   :border-props '(face bold)
                   :height 5
                   :width 5
                   :wrap-words t))

      (expect (view) :to-equal
              #("=======\n==+++++\n==+foo+\n==+bar+\n==+++++\n==+++++"
                0 7 (face bold) 8 10 (face bold) 10 15 (face italic)
                16 18 (face bold) 18 23 (face italic) 24 26 (face bold)
                26 31 (face italic) 32 34 (face bold) 34 39 (face italic)
                40 42 (face bold) 42 47 (face italic)))))

  (describe "join"
    (it "should join blocks vertically"
      (rem-defview view ()
        (rem-join 'column 'middle "foo" "abcdefg" "bar"))
      (expect (view) :to-equal "  foo  \nabcdefg\n  bar  "))

    (it "should join blocks horizontally"
      (rem-defview view ()
        (rem-join 'row 'end "foo\nbar" "xyz" "a\nb\nc"))
      (expect (view) :to-equal "      a\nfoo   b\nbarxyzc"))

    (it "should accept arrays of blocks"
      (rem-defview view ()
        (rem-join 'row 'start "foo" '("bar" "xyz")))
      (expect (view) :to-equal "foobarxyz"))

    (it "should return nil if no blocks are provided"
      (rem-defview view ()
        (rem-join 'column 'start nil))
      (expect (view) :to-be nil))))

(describe "update helper"
  (it "should redraw the buffer after each action"
    (setq str "foo")
    (defun action (arg) (setq str arg))
    (defun view () str)
    (rem-bind "*my-buffer*" 'view '(action))
    (action "bar")
    (expect (with-current-buffer "*my-buffer*" (buffer-string))
            :to-equal "bar"))

  (it "should restore the point to the specified position"
    (defun action ())
    (defun view () "foo bar")
    (rem-bind "*my-buffer*" 'view '(action) (lambda () 3))
    (action)
    (expect (with-current-buffer "*my-buffer*" (point))
            :to-equal 3))

  (it "should support a lambda as a pointer position"
    (defun action ())
    (defun view () "foo bar")
    (rem-bind "*my-buffer*" 'view '(action) (lambda () (lambda () 3)))
    (action)
    (expect (with-current-buffer "*my-buffer*" (point))
            :to-equal 3)))
