;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: getopt-system -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          getopt.asd
;;;; Purpose:       ASDF system definition for getopt package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: kmrcl.asd 7817 2003-09-10 18:38:33Z kevin $
;;;;
;;;; *************************************************************************

(in-package cl-user)
(defpackage getopt-system (:use #:asdf #:cl))
(in-package getopt-system)


(defsystem getopt
    :name "getopt"
    :author "Kevin Rosenberg <kevin@rosenberg.net>"
    :version "1.0"
    :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
    :licence "BSD"

    :components 
    ((:file "package")
     (:file "main" :depends-on ("package"))))

(defmethod perform ((o test-op) (c (eql (find-system 'getopt))))
  (operate 'load-op 'getopt-tests)
  (operate 'test-op 'getopt-tests :force t))


(defsystem getopt-tests
    :depends-on (:ptester :getopt)
    :components
    ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system 'getopt-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:getopt-tests)))
      (error "test-op failed")))

