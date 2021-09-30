;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

;; String Rotation

;; Assume you have a method isSubstring which checks if one word is
;; a substring of another. Given two strings, s1 and s2, write code to check
;; if s2 is a rotation of s1 using only one call to isSubstring (e.g.,
;; "waterbottle" is a rotation of"erbottlewat").

;;; Code

;; So, I'm not sure if I've been extremely lucky with this one, but I've
;; solved it in 8'.  I couldn't believe that I'd done so, even after reading
;; the hints.  All things considered, I think it was easier than the earlier
;; stuff.

(defun ctci/string-rotation (str-1 str-2)
  (when (and (s-contains? str-2 (concat str-1 str-1))
             (= (length str-2)
                (length str-1)))
    t))

(ctci/string-rotation "bar" "arba")
(ctci/string-rotation "bar" "arb")
(ctci/string-rotation "bar" "")
