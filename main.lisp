;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: getopt -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          main.lisp
;;;; Purpose:       Command line option processing like GNU's getopt_long
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id$
;;;;
;;;; *************************************************************************

(in-package getopt)


(defun is-short-option (arg)
  (and (>= (length arg) 2)
       (char= #\- (schar arg 0))
       (char/= #\- (schar arg 1))))

(defun is-option-terminator (arg)
  (and (= 2 (length arg))
       (char= #\- (schar arg 0))
       (char= #\- (schar arg 1))))

(defun is-long-option (arg)
  (and (> (length arg) 2)
       (char= #\- (schar arg 0))
       (char= #\- (schar arg 1))
       (char/= #\- (schar arg 3))))

(defun decompose-arg (arg option-type)
  "Returns base-name,argument"
  (let ((start (ecase option-type
		 (:long 2)
		 (:short 1)))
	(name-end (position #\= arg)))

    (values (subseq arg start name-end)
	    (when name-end (subseq arg (1+ name-end))))))

(defun analyze-arg (arg)
  "Analyzes an argument. Returns option-type,base-name,argument"
  (let* ((option-type (cond ((is-short-option arg) :short)
			    ((is-long-option arg) :long)
			    (t :arg))))
    (if (or (eq option-type :short) (eq option-type :long))
	(multiple-value-bind (base arg) (decompose-arg arg option-type)
	  (values option-type base arg))
	(values :arg arg nil))))


(defun find-option (name options)
  "Find an option in option list. Handles using unique abbreviations"
  (let* ((option-names (mapcar #'car options))
	 (pos (match-unique-abbreviation name option-names)))
    (when pos
      (nth pos options))))

(defun match-option (arg options)
  "Matches an argument to an option. Returns option-list,option-type,base-name,argument"
  (multiple-value-bind (option-type base-name argument) (analyze-arg arg)
    (let ((match (find-option base-name options)))
      (values match option-type (when match (car match)) argument))))


;;; EXPORTED functions

(defun match-unique-abbreviation (abbr strings)
  "Returns position of ABBR in STRINGS. ABBR may be a unique abbreviation.
Returns NIL if no match found."
  (let ((len (length abbr))
	(matches nil))
    (dotimes (i (length strings))
      (let* ((s (nth i strings))
	     (l (length s)))
	(cond
	  ((= len l)
	   (when (string= abbr s)
	     (push (cons s i) matches)))
	  ((< len l)
	   (when (string= abbr (subseq s 0 len))
	     (push (cons s i) matches))))))
    (when (= 1 (length matches))
      (cdr (first matches)))))


(defun getopt (args options)
  "Processes a list of arguments and options. Returns filtered argument
list and alist of options.
opts is a list of option lists. The fields of the list are
 - NAME name of the long option
 - HAS-ARG with legal values of :NONE, :REQUIRED, :OPTIONAL
 - VAL value to return for a option with no arguments"
  (do ((pos args (cdr pos))
       (finished-options)
       (out-opts)
       (out-args)
       (errors))
      ((null pos) (values (nreverse out-args) (nreverse out-opts) errors))
    (cond
     (finished-options
      (push (car pos) out-args))
     ((is-option-terminator (car pos))
      (setq finished-options t))
     (t
      (let ((arg (car pos)))
	(multiple-value-bind (option-list option-type base-name argument)
	    (match-option (car pos) options)
	  (cond
	    ((and option-list (not (eq option-type :arg)))
	     (cond
	       (argument
		(case (second option-list)
		  (:none
		   (push base-name errors))
		  (t
		   (push (cons base-name argument) out-opts))))
	       ((null argument)
		(if (and (eq :required (second option-list)) (null (cdr pos)))
		    (push base-name errors)
		    (if (or (is-short-option (second pos))
			    (is-long-option (second pos)))
			(if (eq :required (second option-list))
			    (push base-name errors)
			    (push (cons base-name (third option-list)) out-args))
			(progn
			  (push (cons base-name (second pos)) out-opts)
			  (setq pos (cdr pos))))))))
	    (t
	     (if (or (eq :long option-type)
		     (eq :short option-type))
		 (push (nth-value 0 (decompose-arg arg option-type)) errors)
	       (push arg out-args))))))))))

