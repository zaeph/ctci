;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

;; URLify

;; Write a method to replace all spaces in a string with '%20'. You may assume
;; that the string has sufficient space at the end to hold the additional
;; characters, and that you are given the "true" length of the string. (Note:
;; If implementing in Java, please use a character array so that you can
;; perform this operation in place.)

;;; Code

(require 'cl-lib)

(defun ctci/urlify-simple (str)
  (cl-loop for char across str
           with out = ""
           for add = ""
           if (= char ? )
           do (setq add "%20")
           else
           do (setq add (char-to-string char))
           end
           and do (setq out (concat out add))
           finally return out))

;; This solution is decent, but it's not taking into account some of the
;; elements which are provided in the description of the exercise.  For
;; instance, I've completely eluded the fact the reference to 'sufficient
;; space' refers to the fact that the array won't have to be expanded, which
;; prevents me from having to think about expanding the array.

;; (defun ctci/urlify-with-length (str length))

;;; Tests

(defun ctci/urlify-make-test (&optional variant)
  (let* ((fun-base-name "ctci/urlify")
         (fun-name (if variant
                       (format "%s-%s" fun-base-name variant)
                     fun-base-name))
         (fun (intern fun-name))
         (test (intern (format "%s-%s" fun-name "test")))
         (comparison #'string=)
         (data '(("foo bar" "foo%20bar")
                 ("foo" "foo")
                 ("" ""))))
    `(ert-deftest ,test ()
       ,@(cl-loop for (input output) in data
                  collect `(should (,comparison (funcall #',fun ,input)
                                                ,output))))))

(defun ctci/urlify-with-length-make-test (&optional variant)
  (let* ((fun-base-name "ctci/urlify-with-length")
         (fun-name (if variant
                       (format "%s-%s" fun-base-name variant)
                     fun-base-name))
         (fun (intern fun-name))
         (test (intern (format "%s-%s" fun-name "test")))
         (comparison #'string=)
         (data '((("foo bar" 9) "foo%20bar")
                 (("foo" 3) "foo")
                 (("" 0) ""))))
    `(ert-deftest ,test ()
       ,@(cl-loop for (input output) in data
                  collect `(should (,comparison (funcall #',fun ,input)
                                            ,output))))))

(ctci/urlify-make-test "simple")
