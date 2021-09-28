;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

;; Palindrome permutation

;; Given a string, write a function to check if it is a permutation of
;; a palindrome.  A palindrome is a word or phrase that is the same forwards
;; and backwards.  A permutation is a rearrangement of letters.  The
;; palindrome does not need to be limited to just dictionary words.

;;; Code

(require 'cl-lib)

(defun ctci/palindrome-permutation (str)
  (let ((str (seq-sort #'< str)))
    (catch 'not-a-palindrome
      (cl-loop for char across str
               with has-odd
               with prev-char
               with prev-char-count
               with char-table = (make-vector 255 0)
               unless (= char ? )
               do (let ((char-count (cl-incf (aref char-table char))))
                    (when (and prev-char
                               (not (= char prev-char))
                               (cl-oddp prev-char-count))
                      (if has-odd
                          (throw 'not-a-palindrome nil)
                        (setq has-odd t)))
                    (setq prev-char char)
                    (setq prev-char-count char-count))
               finally return t))))

(ctci/palindrome-permutation "foo foo")
(ctci/palindrome-permutation " foooo")
(ctci/palindrome-permutation "foo bar")

;; This is what I had after 45 min; it's not even returning the permutations.
;; I grew incredibly frustrated at `cl-loop' for not allowing me to bind
;; variables below it with `for', which made me lose at least 10 min.  I think
;; I need to be more pragmatic about using proper Elisp conditions.  That
;; being said, this journey has made me discover CL's `iterate' (not ported to
;; Elisp yet) which seems to have a more flexible format than what `cl-loop'
;; is using.
;; https://common-lisp.net/project/iterate/doc/Don_0027t-Loop-Iterate.html

;; Looking at their first solution, I'm also not impressed by the fact that
;; they go multiple times over the list.  I wanted to go for an optimised
;; version right away, which in turn bogged me down into optimisation
;; thinking.

;; …Wait a minute.  I've just written the instructions for the exercise, and
;; the goal was *never* to return the list of permutations…?
;; …So, effectively, I've solved the exercise in the required time.  Yay!
;; Sure, it's not beautiful, but at least it has the merits of not going twice
;; over a list, and it's only using an array that is has long as the encoding
;; requires, which precludes the need for a hash-table.
