;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for getopt package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id$
;;;;
;;;; *************************************************************************

(in-package cl-user)

(defpackage getopt
  (:use #:cl)
  (:export
   #:match-unique-abbreviation
   #:getopt
   ))
