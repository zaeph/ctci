;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

;; Remove Dups

;; Write code to remove duplicates from an unsorted linked list.
;; FOLLOW UP
;; How would you solve this problem if a temporary buffer is not allowed?

;;; Code

(defun ctci/remove-dups (list)
  (cl-loop for elem in list
           with table = (make-hash-table)
           with out
           if (not (gethash elem table))
           do (progn (puthash elem t table)
                     (push elem out))
           finally return (nreverse out)))

(ctci/remove-dups '(1 1 2 3))

;; Initial solution was achieved quickly (5'); the 'FOLLOW UP' required me to
;; think of setf and nthcdr, and I did some experimentations in *scratch*

(let ((l (list 1 2 3)))
  ;; We want to remove 2
  (cl-loop for elem in l
           for i to (- (length l) 1)
           if (= elem 2)
           do (setf (nthcdr i l) (cdr (nthcdr i l)))
           finally return l))

;; Then, I solved it on paper; all in all, took 20'.

(defun ctci/remove-dups (list)
  (cl-loop for elem in list
           for i to (- (length list) 1)
           with offset = 0
           for index = (- i offset)
           with table = (make-hash-table)
           if (not (gethash elem table))
           do (puthash elem t table)
           else
           do (progn (setf (nthcdr index list) (nthcdr (+ index 1) list))
                     (cl-incf offset))
           finally return list))

(ctci/remove-dups '(1 1 2 3))
(ctci/remove-dups '(1 2 2 2 2))

;; Writing this solution was done pretty quickly by looking at the results of
;; eval.  A gotcha was that setf introduces an offset that needed to be kept
;; in mind, hence the addition of the vars offset and index.  I think I would
;; have been able to catch this one by running tests within the 45'.
