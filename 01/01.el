;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

(require 'cl-lib)
(require 'zp-timer-macs)

;;; Is Unique
;; Implement an algorithm to determine if a string has all unique
;; characters.  What if you cannot use additional data structures?

;; Using a hash-table
;; There might be some delay incurred by initialising a hash-table
(defun ctci/all-unique-char-hash (str)
  "Return t if all chars in STR is unique."
  (let ((chars (make-hash-table)))
    (not (catch 'found
           (cl-loop for char across str
                    if (gethash char chars)
                    do (throw 'found t)
                    else
                    do (puthash char t chars))))))

;; Using a list
(defun ctci/all-unique-char-list (str)
  "Return t if all chars in STR is unique."
  (let (chars)
    (not (catch 'found
           (cl-loop for char across str
                    if (member char chars)
                    do (throw 'found t)
                    else
                    do (push char chars))))))

;; It doesn't really make sense to try it with an array, since they're not
;; extensible in Elisp and not very lispy anyway.  I could re-invent the wheel
;; and make them extensible, but here's a POC with `max-char'.
;; (defun ctci/all-unique-char-array (str)
;;   "Return t if all chars in STR is unique."
;;   (let ((chars (make-bool-vector (max-char) nil)))
;;     (not (catch 'found
;;            (cl-loop for char across str
;;                     if (and chars       ;Avoid checking array if nil
;;                             (aref chars char))
;;                     do (throw 'found t)
;;                     else
;;                     do (aset chars char t))))))

;; Limiting ourselves to existing data structure
(defun ctci/all-unique-char-no-extra-struct (str)
  (not (catch 'found
         (cl-loop for char1 across str
                  for i = 0
                  do (cl-loop for char2 across str
                              if (eq char1 char2)
                              do (setq i (+ i 1))
                              and if (= i 2)
                              do (throw 'found t))))))
;; Complexity:
;; n is the number of chars in STR
;; Time: O(n²)
;; Space: O(n)

(ctci/all-unique-char-no-extra-struct "#11")
(ctci/all-unique-char-no-extra-struct "#12")

;; Do not check the same index twice to avoid the (= i 2) nonsense
(defun ctci/all-unique-char-no-extra-struct-2 (str)
  (not (catch 'found
         (cl-loop for char1 across str
                  for i from 0
                  do (cl-loop for char2 across str
                              for j from 0
                              when (and (not (= i j)) ;Excluding same indexes
                                        (eq char1 char2))
                              do (throw 'found t))))))

(ctci/all-unique-char-no-extra-struct-2 "#11")
(ctci/all-unique-char-no-extra-struct-2 "#12")



;;----------------------------------------------------------------------------
;; Tests
;;----------------------------------------------------------------------------
(ert-deftest ctci/all-unique-char-hash-test ()
  (should (ctci/all-unique-char-hash "#1"))
  (should (ctci/all-unique-char-hash "#132"))
  (should (not (ctci/all-unique-char-hash "#44")))
  (should (not (ctci/all-unique-char-hash "#117"))))

(ert-deftest ctci/all-unique-char-list-test ()
  (should (ctci/all-unique-char-list "#1"))
  (should (ctci/all-unique-char-list "#132"))
  (should (not (ctci/all-unique-char-list "#44")))
  (should (not (ctci/all-unique-char-list "#117"))))

(ert-run-tests-interactively "ctci/all-unique-char-*")

;;----------------------------------------------------------------------------
;; Optimisation
;;----------------------------------------------------------------------------
;; With a short string
(time-stats 5 10000
  (ctci/all-unique-char-hash "#117"))
;; "min: 0.375s, max: 0.407s, mean: 0.382s"
(time-stats 5 10000
  (ctci/all-unique-char-list "#117"))
;; "min: 0.360s, max: 0.384s, mean: 0.369s"

;; With a longer string
(time-stats 5 10000
  (ctci/all-unique-char-hash "'0123456789abcdefghijklmnopqrstuvwxyzABCDE\
FGHIJKLMNOPQRSTUVWXYZ!#$%&'()*+,-./:<=>?@[\\]^_`{|}~ \t\n\r\x0b\x0c'"))
;; "min: 0.692s, max: 0.708s, mean: 0.699s"
(time-stats 5 10000
  (ctci/all-unique-char-list "'0123456789abcdefghijklmnopqrstuvwxyzABCDE\
FGHIJKLMNOPQRSTUVWXYZ!#$%&'()*+,-./:<=>?@[\\]^_`{|}~ \t\n\r\x0b\x0c'"))
;; "min: 0.841s, max: 0.863s, mean: 0.851s"
