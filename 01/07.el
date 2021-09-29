;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

;; Rotate Matrix:

;; Given an image represented by an NxN matrix, where each pixel in the image
;; is 4 bytes, write a method to rotate the image by 90 degrees. Can you do
;; this in place?

;;; Code

(require 'cl-lib)
(require 'dash)

;; I was way out of my league on that one: I had trouble grasping what the
;; matrix was, how to log it, and after looking at the hints and not being
;; helped *at all*, I decided to look at the solution.  I'll try not to
;; implement the solution in Elisp to show that I've understood it.

;; Experimenting with what the matrix should look like first
(let ((matrix [[1 2 3]
               [4 5 6]
               [7 8 9]]))
  (length (aref matrix 1)))

(defun ctci/rotate-matrix (matrix)
  ;; Validate
  (when (or (= (length matrix) 0)
            ;; Testing not square; we should probably iterate over every
            ;; columns, but I think proper matrixes enforce equal col.length,
            ;; so I won't get too bothered by it and I'll assume well-formed
            ;; matrixes, even though Elisp doesn't have those types
            ;; apparently.
            (not (= (length matrix)
                    (length (aref matrix 0)))))
    (error "Not a valid matrix"))
  (cl-loop with n = (length matrix)
           for layer to (/ n 2)
           for first = layer
           for last = (- n 1 layer)
           do (cl-loop for i from first to (- last 1)
                       for offset = (- i first)

                       for top = (-> (aref matrix first)
                                     (aref i))
                       do (progn
                            ;; left -> top
                            (setf (-> (aref matrix first)
                                      (aref i))
                                  (-> (aref matrix (- last offset))
                                      (aref first)))

                            ;; bottom -> left
                            (setf (-> (aref matrix (- last offset))
                                      (aref first))
                                  (-> (aref matrix last)
                                      (aref (- last offset))))

                            ;; right -> bottom
                            (setf (-> (aref matrix last)
                                      (aref (- last offset)))
                                  (-> (aref matrix i)
                                      (aref last)))

                            ;; top -> right
                            (setf (-> (aref matrix i)
                                      (aref last))
                                  top)))
           finally return matrix))

(ctci/rotate-matrix [[1 2 3]
                     [4 5 6]
                     [7 8 9]])
;; [[7 4 1]
;;  [8 5 2]
;;  [9 6 3]]
