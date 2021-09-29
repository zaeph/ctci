;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

;; String compression

;; Implement a method to perform basic string compression using the counts of
;; repeated characters. For example, the string aabcccccaaa would become
;; a2blc5a3. If the "compressed" string would not become smaller than the
;; original string, your method should return the original string. You can
;; assume the string has only uppercase and lowercase letters (a - z).

;;; Code

(require 'cl-lib)

(defun clci/string-compression (str)
  (cl-loop for char across str
           with prev-char
           with counter = 0
           with output = ""
           do (progn
                (cond ((and prev-char
                            (not (= char prev-char)))
                       (setq output (concat output
                                            (char-to-string prev-char)
                                            (number-to-string counter)))
                       ;; Setting the counter to 1 because we've found the
                       ;; first one of a new character.
                       (setq counter 1))
                      (t
                       (cl-incf counter)))
                (setq prev-char char))
           finally return (concat output
                                  (char-to-string char)
                                  (number-to-string counter))))

(clci/string-compression "foo")

;; Got to the solutionon paper in 15'; writing the code and adjusting for
;; off-by-one errors with the indexes took me 10'; so, in total, 25' to get to
;; the solution.
;; I could probably optimise the legibility of the code by properly handling
;; the last character and reducing the duplication of the concat.

;; I've just noticed though that I forgot part of the instructions about the
;; lengths; took me 2' to fix it.

(defun clci/string-compression (str)
  (let ((str-new
         (cl-loop for char across str
                  with prev-char
                  with counter = 0
                  with output = ""
                  do (progn
                       (cond ((and prev-char
                                   (not (= char prev-char)))
                              (setq output (concat output
                                                   (char-to-string prev-char)
                                                   (number-to-string counter)))
                              ;; Setting the counter to 1 because we've found the
                              ;; first one of a new character.
                              (setq counter 1))
                             (t
                              (cl-incf counter)))
                       (setq prev-char char))
                  finally return (concat output
                                         (char-to-string char)
                                         (number-to-string counter)))))
    (if (>= (length str-new)
           (length str))
        str
      str-new)))

(clci/string-compression "foo")
(clci/string-compression "fooo")
(clci/string-compression "foooo")

;; One of the hints is warning against concat'ing string multiple times, which
;; I'm tempted to agree with.  So, let's try to use the remaining time to come
;; up with a solution that uses an array to do this.

;; I'm going to be too short on time to implement this; but given the Elisp
;; environment in which I'm working, I'd be tempted to use push the completed
;; blocks to a string, then `nreverse' and `mapconcat' the list.  I've checked
;; the C code, and I *think* `mapconcat' only runs `concat' once rather than
;; repeatedly, which would avoid the problem of concat strings multiple time.
;; Since I believe that the problem with concat'ing string all the time is
;; that it's constantly creating new arrays to store the new data, I think
;; this would solve the problem nicely.

;; After looking at the solution, the StringBuilder type that they describe
;; seems pretty similar to the `mapconcat' solution I've mentioned.  I was
;; using a list rather than an array to compensate for the non-expendible
;; arrays in Elisp, but other than that, I think I've had a good intuition.
