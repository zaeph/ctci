;; -*- mode: lisp-interaction; coding: utf-8; lexical-binding: t -*-

;;; Check Permutation
;; Given two strings, write a method to decide if one is a permutation of the
;; other.

(require 'cl-lib)
(require 'dash)
(require 'seq)

(defun ctci/check-permutation-reduce (str1 str2)
  (when (= (length str1)
           (length str2))
    (--> (cl-loop for str in (list str1 str2)
                  collect (seq-sort #'< str))
         (when (cl-reduce #'string= it)
           t))))

;; It's not great to use `reduce' here because if the fail happens early, the
;; rest of the strings will still go through the reduction.  I could implement
;; a throw/catch in the pred, but I think it's better to work with arrays
;; inside the loop.

(defun ctci/check-permutation-non-modularised-signature (str1 str2)
  (when (= (length str1)
           (length str2))
    (let ((sorted-strs (cl-loop for str in (list str1 str2)
                                collect (seq-sort #'< str))))
      (catch 'difference
        (cl-loop with str1 = (pop sorted-strs)
                 while sorted-strs
                 for str2 = (pop sorted-strs)
                 unless (string= str1 str2)
                 do (throw 'difference nil)
                 finally return t)))))

;; Now, we're not using `cl-reduce' anymore, which is good; but the modularity
;; needs to appear in the function signature as well.

;; Using `&rest' in the signature
(defun ctci/check-permutation (&rest strs)
  (when (->> (mapcar #'length strs)
             (cl-every #'=))
    (let ((sorted-strs (cl-loop for str in strs
                                collect (seq-sort #'< str))))
      (catch 'difference
        (cl-loop with str1 = (pop sorted-strs)
                 while sorted-strs
                 for str2 = (pop sorted-strs)
                 unless (string= str1 str2)
                 do (throw 'difference nil)
                 finally return t)))))

;;----------------------------------------------------------------------------
;; Alternatives
;;----------------------------------------------------------------------------

;; Cheating; there's a command that almost does what we want, but it returns
;; t for "foo" and "fooo"; we can just use our previous check
(defun ctci/check-permutation-native (str1 str2)
  (when (= (length str1)
           (length str2))
    (seq-set-equal-p str1 str2)))

;;----------------------------------------------------------------------------
;; Tests
;;----------------------------------------------------------------------------

;; Dumb tests
(ert-deftest ctci/check-permutation-test ()
  (should (ctci/check-permutation "foo" "oof"))
  (should (ctci/check-permutation "foo" "foo"))
  (should (not (ctci/check-permutation "foo" "")))
  (should (not (ctci/check-permutation "foo" "bar")))
  (should (not (ctci/check-permutation "foo" "bor")))
  (should (not (ctci/check-permutation "foo" "fooo"))))

;; Test-maker; vifon hates it, but at least I don't have to write boilerplate
;; for dumb tests like above.  This is just for using ERT's quick testing
;; facility, not for developing a package.
(defun ctci/check-permutation-deftest (&optional name)
  (let* ((base-name "ctci/check-permutation")
         (checks '((("foo" "foo") t)
                   (("foo" "oof") t)
                   (("foo" "bar") nil)
                   (("foo" "fooo") nil)
                   (("foo" "") nil)))
         (fun (-> (if name
                      (format "%s-%s" base-name name)
                    base-name)
                  (intern)))
         (comparison 'equal))
    (eval `(ert-deftest ,fun ()
             ,@(cl-loop for (input output) in checks
                        collect `(should (,comparison (apply #',fun ',input)
                                                      ,output)))))))

;; Running tests
(ctci/check-permutation-deftest "reduce")
(ctci/check-permutation-deftest "non-modularised-signature")
(ctci/check-permutation-deftest)
