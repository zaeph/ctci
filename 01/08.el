;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

;; Zero Matrix

;; Write an algorithm such that if an element in an MxN matrix is 0, its
;; entire row and column are set to 0.

;;; Code

(require 'cl-lib)

(defun ctci/zero-matrix (matrix)
  (let (cols)
    (cl-loop for row across matrix
             for i to (- (length matrix) 1)
             do (catch 'back
                  (cl-loop for cell across row
                           for j to (- (length row) 1)
                           with l = (length row)
                           if (= cell 0)
                           do (progn (setf (aref matrix i) (make-vector l 0))
                                     (push j cols)
                                     (throw 'back nil))))
             finally return matrix)
    (cl-loop for i to (- (length matrix) 1)
             do (cl-loop for j in cols
                         do (setf (aref (aref matrix i) j)
                                  0)))
    matrix))

(ctci/zero-matrix [[0 1 2]
                   [3 4 5]
                   [6 7 0]])

;; Solved in 25'; not very efficient since we have a second loop to it, but
;; the `setf' is working fine for inplace modifications.

;; Time complexity:
;; O(M×(N+1)), with the extra M being for the 2nd loop for wiping cols.

;; Since we need to do a second loop to wipe the columns that have been tagged
;; in the first pass, I think that's pretty good.
