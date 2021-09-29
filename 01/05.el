;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

;; One away

;;; Code

(require 'cl-lib)

(defun ctci/good-length-p (str-1 str-2)
  (let ((diff (- (length str-1)
                 (length str-2))))
    (when (and (>= diff -1)
                 (<= diff 1))
      t)))

(ctci/good-length-p "foo" "fooo")

(defun ctci/one-away (str-1 str-2)
  (when (ctci/good-length-p str-1 str-2)
    (catch 'false
      (cl-loop for i to (max (length str-1)
                             (length str-2))
               for char-1 = (aref str-1 i)
               for char-2 = (aref str-2 i)
               with found-diff = 0
               with previous-char-1
               with previous-char-2
               do (progn
                    (when (not (or (= char-1 char-2)
                                   (and previous-char-2
                                        (= char-1 previous-char-2))
                                   (and previous-char-2
                                        (= char-2 previous-char-1))))
                      (cl-incf found-diff))
                    (when (> found-diff 1)
                      (throw 'false nil))
                    (setq previous-char-1 char-1
                          previous-char-2 char-2))
               finally return t))))

(ctci/one-away "foo" "oo")

;; Ugh.  Spent 55' to get there, and the cond is not even functional. *sigh*

;; 2nd attempt, without a time constraint

(defun ctci/get-delta (str-1 str-2)
  "Return delta between STR-1 and STR-2.
If there was an addition, return 1.
If there was a deletion, return -1.
Otherwise, return nil."
  (let ((delta (- (length str-2)
                  (length str-1))))
    (unless (or (> delta 1)
                (< delta -1))
      delta)))

(ctci/get-delta "foo" "foooo")
(ctci/get-delta "foo" "fooo")
(ctci/get-delta "fooo" "foo")

(defun ctci/one-away (str-1 str-2)
  (let ((delta (ctci/get-delta str-1 str-2)))
    (unless delta
      ;; We're only checking for deletions to make this easier, and we're
      ;; defining the polarity based on the delta
      (cl-loop for i in (length (pcase delta
                                  (1 str-2)
                                  (-1 str-1)))
               for char-1
               with previous-char-1
               with previous-char-2
               ...))))

;; I've read the hints now, and I think I've tried to go for the most
;; optimised solution right away.  The last hints tells us that you probably
;; don't need to go three times over the strings to check the conditions
;; separately, which is what I was trying to do right away.  I really need to
;; be able to come up with brute solutions right away, and I'll have to find
;; my perfectionism for this.

;; The next day

;; I'm giving myself 20 min at the start of the next prep day to write this
;; down.  I've read the hints, which validated all of my instincts for this
;; exercise, so now's the time to actually do it.

(defun ct/one-away (str-1 str-2)
  (let* ((get-polarity (lambda (str-1 str-2)
                           (- (length str-2)
                              (length str-1))))
         (delta (funcall get-polarity str-1 str-2)))
    ;; Fail early if the delta is too large
    (unless (or (> delta 1)
                (< delta -1))
      ;; Since deletion can be seen as an insertion if we reverse the
      ;; inputs, we'll do the reversing here
      (when (= delta -1)
        (setq str-1 str-2
              str-2 str-1
              delta 1))
      ;; At this point, it can either be an insertion, a replacement, or
      ;; nothing.  For now, we'll do it in two loops to keep the code simple.
      (pcase delta
        (0 (catch 'fail
             (cl-loop for char-1 across str-1
                      for char-2 across str-2
                      with differences = 0
                      do (when (not (= char-1 char-2))
                           (cl-incf differences)
                           (when (> differences 1)
                             (throw 'fail nil)))
                      finally return (if (= 0 differences)
                                         nil
                                       t))))
        (1 (catch 'fail
             (cl-loop for i to (- (length str-2) 1)
                      for char-1 = (aref str-1 (if (> (+ i 1)
                                                      (length str-1))
                                                   (- i 1)
                                                 i))
                      for char-2 = (aref str-2 (+ i differences))
                      with differences = 0
                      do (when (not (= char-1 char-2))
                           (cl-incf differences)
                           (when (> differences 1)
                             (throw 'fail nil)))
                      finally return t)))))))

(ct/one-away "foo" "foo")
(ct/one-away "foo" "fao")
(ct/one-away "f" "fo")
(ct/one-away "f" "foo")

;; After 35', I have a working solution that has redundancy, but which is
;; fairly optimised.  I did fall in many traps with the length vs. index, but
;; it's working pretty decently.

;; I could reduce redundancy quite easily, considering how similar the two
;; loops are.  As it stands, I've spent enough time on this problem, and would
;; rather move on to another one.
