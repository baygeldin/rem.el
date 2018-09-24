;; -*- lexical-binding: t -*-

(require 'rem)

(describe "view"
  (it "should memoize results for components from the previous render")
  (it "should not memoize results for components forever")
  (it "should memoize results for components that were re-used indirectly")
  (it "should not interfere with memoization of other views")
  (it "should not consider equal strings with different properties equal"))

(describe "component"
  (describe "block"
    (it "should return a single line unchanged")
    (it "should make all lines the same length")
    (it "should support all stated parameters"))
  (describe "join"
    (it "should join blocks vertically")
    (it "should join blocks horizontally")))

(describe "update helper"
  (it "should redraw the buffer after each action")
  (it "should restore the point to the specified position")
  (it "should support a lambda as a pointer position"))
