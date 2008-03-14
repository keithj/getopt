;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: getopt-tests -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          getopt-tests.lisp
;;;; Purpose:       getopt tests file
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id$
;;;;
;;;; This file is Copyright (c) 2003 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package cl)
(defpackage getopt-tests
  (:use #:getopt #:cl #:ptester))
(in-package getopt-tests)

(defmacro test-mv (values form)
  `(test ,values ,form :multiple-values t :test #'equal))

(defun do-tests ()
  (with-tests (:name "GETOPT")
    (let ((*break-on-test-failures* nil))

      ;; match-unique-abbreviation
      (test nil (match-unique-abbreviation "abc" nil))
      (test nil (match-unique-abbreviation "abc" '("ab")))
      (test 0 (match-unique-abbreviation "ab" '("ab")))
      (test 0 (match-unique-abbreviation "a" '("ab")))
      (test nil (match-unique-abbreviation "b" '("ab")))
      (test nil (match-unique-abbreviation "ab" '("ab" "abc")))
      (test 1 (match-unique-abbreviation "ac" '("ab" "ac")))
      (test 1 (match-unique-abbreviation "ac" '("ab" "acb")))

      ;; getopt
      (test-mv '(("argv") nil nil) (getopt '("argv") nil))
      (test-mv '(("argv" "2") nil nil) (getopt '("argv" "2") nil))

      (test-mv '(("argv") (("c")) nil) (getopt '("argv" "-c") '(("c" :none))))

      (test-mv '(("argv") (("c" . "val")) nil)
               (getopt '("argv" "-c" "val") '(("c" :optional))))
      (test-mv '(("argv" "v1") (("c" . "val")) nil)
               (getopt '("argv" "-c" "val" "v1") '(("c" :optional))))
      (test-mv '(( "v1") (("colon" . "val")) nil)
               (getopt '("--colon" "val" "v1") '(("colon" :optional))))
      (test-mv '(("ab" "-c") (("colon" . "val")) nil)
               (getopt '("ab" "--colon" "val" "--" "-c")
                       '(("colon" :optional) ("-c" :none))))
      (test-mv '(("argv") (("c" . "cd")) nil)
               (getopt '("argv" "-c" "cd") '(("c" :required))))
      (test-mv '(("argv") nil ("c"))
               (getopt '("argv" "-c") '(("c" :required))))
      (test-mv '(("argv") (("c" . "10")) nil)
               (getopt '("argv" "-c=10") '(("c" :required))))
      (test-mv '(("argv") nil ("c"))
               (getopt '("argv" "-c=10") '(("c" :none))))
      (test-mv '(nil (("along" . "10")) nil)
               (getopt '("--along=10") '(("along" :optional))))
      (test-mv '(nil nil ("along"))
               (getopt '("--along=10") '(("along" :none))))
      (test-mv '(nil (("along" . "10")) nil)
               (getopt '("--a=10") '(("along" :optional))))
      (test-mv '(nil nil ("a"))
               (getopt '("--a=10") '(("along" :optional) ("aboot" :optional))))
      (test-mv '(("a") nil nil)
               (getopt '("a") '(("a" :none))))
      (test-mv '(("a") (("foo") ("bar")) nil)
               (getopt '("a" "--foo" "--bar") '(("foo" :none) ("bar" :none))))
      (test-mv '(("a") (("foo") ("bar")) nil)
               (getopt '("a" "--f" "--bar") '(("foo" :none) ("bar" :none))))
      ))
  t)
