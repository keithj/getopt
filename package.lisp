;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for getopt package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id: package.lisp 7819 2003-09-11 16:20:23Z kevin $
;;;;
;;;; *************************************************************************

(in-package cl-user)

(defpackage getopt
  (:use #:cl)
  (:export
   #:match-unique-abbreviation
   #:getopt
   ))
