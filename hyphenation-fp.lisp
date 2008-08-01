;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; $Id: hyphen.lisp,v 1.1.1.1 2003/04/30 09:16:04 fabrice.popineau Exp $

;;; New implementation of TeX algorithms in Common Lisp by Fabrice Popineau

;;; See file LICENSE.TXT for the license terms.

;;; email: fabrice.popineau@supelec.fr
;;; snail: Fabrice Popineau
;;;        Supelec
;;;        2 rue E. Belin
;;;        F-57070 Metz France
;;; www:   http://www.metz.supelec.fr/~popineau/

(defpackage "CL-TYPESETTING-HYPHEN"
  (:use common-lisp)
  (:export
   ))

(in-package "CL-TYPESETTING-HYPHEN")

(defvar *cl-typesetting-base-directory*
   (make-pathname :name nil :type nil :version nil
     :defaults #.(or #-gcl *compile-file-truename* *load-truename*))
   "The base directory for cl-typesetting source and auxiliary data")

(defvar *hyphen-patterns-directory*
  (merge-pathnames (make-pathname :name nil :type nil :version nil
                                  :directory '(:relative "hyphen-patterns"))
                   *cl-typesetting-base-directory*))

(defvar *language-hyphen-file-list*
  '((:british        . "gbhyph")
    (:german-new     . "dehyphn")
    (:german-std     . "dehypht")
    (:american       . "ushyph")
    (:united-kingdom . "ukhyph")
    (:french         . "frhyph")
    (:foo            . "foo")
    )
  )

;; An hyphenation object is able to return the list
;; of hyphenation points for any word according to
;; the language it has been built for
(defclass hyphen-trie ()
  ((language :initform :american :accessor language :initarg :language)
#+nil
   (fn-find-hyphen-points :initform () :accessor fn-find-hyphen-points :initarg :fn-find-hyphen-points)
   (pattern-trie :initform () :accessor pattern-trie :initarg :pattern-trie)
   (exception-trie :initform () :accessor exception-trie :initarg :exception-trie)
   )
  )

(defmethod (setf pattern-trie) (value hyphen-trie)
  (declare (ignore hyphen-trie))
  value)

(defmethod (setf exception-trie) (value hyphen-trie)
  (declare (ignore hyphen-trie))
  value)

(defvar *left-hyphen-minimum* 2
  "Minimum number of characters that must precede a hyphen.")

(defvar *right-hyphen-minimum* 3
  "Minimum number of characters that must follow a hyphen.")

;; This will hold the lambda function which will execute the trie
(defmethod hyphen-find-hyphen-points (hyphen-trie word)
#+nil
  (funcall (symbol-function (fn-find-hyphen-points hyphen-trie)) word)
  (let* ((word-seq (coerce word 'list))
	 (word-length (length word))
	 (result (hyphen-trie-find-exception word-seq (exception-trie hyphen-trie))))
    (unless result
      (setq result (hyphen-trie-find `(#\. ,@word-seq #\.) (pattern-trie hyphen-trie))))
    (mapcar #'first (remove-if 
                     #'(lambda (x)
                         (let ((idx (first x)))
                           (or (eq idx :end)
                               (< idx *left-hyphen-minimum*)
                               (< (- word-length idx) *right-hyphen-minimum*))))
                     result))))

;; Format of the language file:
;; the first line has 'pattern'
;; one pattern per line
;; TeX notation ^^xy allowed
;; exceptions introduced by a line with 'exception'

;; Parse a pattern from the language file
(defun hyphen-parse-pattern-line (line)
  (let ((char-list (coerce line 'list))
	key value (position 0))
    (loop for c = (pop char-list)
      while c
      do
      (cond ((and (eq c #\^) (eq (pop char-list) #\^)) ;; FIXME against syntax errors
	     (let* ((c1 (digit-char-p (pop char-list) 16))
		    (c2 (digit-char-p (pop char-list) 16)))
	       (push (code-char (+ (* c1 16) c2))
		     key))
	     (incf position))
	    ((digit-char-p c) (push (cons position (digit-char-p c)) value)) 
	    (t (push c key) (incf position))))
    (append (reverse key) value)))

;; Parse an exception from the language file
(defun hyphen-parse-exception-line (line)
  (let ((char-list (coerce line 'list))
	key value (position 0))
    (loop for c = (pop char-list)
      while c
      do
      (cond ((eq c #\-) (push (cons position 1) value))
	    (t (push c key) (incf position))))
    (append (reverse key) (or value (list (cons :end 1))))))

;; Build a trie out of a sorted list 
;; of pairs (word, hyph-points)
;;
(defun hyphen-make-trie (list-of-list depth)
  (when (first list-of-list)
      (let (result 
	    subresult
	    (prev_c (caar list-of-list)))
	(loop for l = (pop list-of-list)
	  while l
	  do
	  (cond 
	   ((eq (first l) prev_c) (push (rest l) subresult))
	   (t 
	    (cond ((characterp prev_c)
		   (push `(,prev_c ,@(hyphen-make-trie subresult (+ depth 1))) result))
		  (t
		   (push `(:pattern ,prev_c ,@(first subresult)) result)))
	    (setq prev_c (first l) 
		  subresult (list (rest l)))))
	  finally 
	  (progn 
	    (if (characterp prev_c)
		(push `(,prev_c ,@(hyphen-make-trie subresult (+ depth 1))) result)
	      (push `(:pattern ,prev_c ,@(first subresult)) result)))
	  )
	result)
      ))

;; Find a word in an hyphenation trie
;;
;;
(defun hyphen-trie-find-aux (word-seq trie)
  (when trie
    (append 
     (rest (assoc :pattern trie))
     (when word-seq
       (hyphen-trie-find-aux (rest word-seq) (cdr (assoc (first word-seq) trie)))))))

(defun hyphen-trie-find (word-seq trie)
  (let* ((pos -2))
    (remove-if #'evenp
	       (remove-duplicates
		(sort
		 (mapcon #'(lambda (x) 
			     (incf pos)
			     (mapcar #'(lambda (y) 
					 (cons (+ (first y) pos) (rest y)))
				     (hyphen-trie-find-aux x trie))) word-seq)
		 #'(lambda (x y) (or (< (first x) (first y))
				     (and (= (first x) (first y)) (< (rest x) (rest y)))))
		 )
		:key #'first)
     :key #'rest)))
    
;; Exceptions are a bit different
;; Either a word is an exception or not, but no patter to test
;;
(defun hyphen-trie-find-exception-aux (word-seq trie)
  (when trie 
    (if word-seq
	(hyphen-trie-find-exception-aux (rest word-seq) (cdr (assoc (first word-seq) trie)))
      (rest (assoc :pattern trie)))))

(defun hyphen-trie-find-exception (word-seq trie)
  (remove-duplicates
   (sort
    (hyphen-trie-find-exception-aux word-seq trie)
    #'(lambda (x y) (or (< (first x) (first y))
			(and (= (first x) (first y))
			     (< (rest x) (rest y)))))
    )	
   :key #'first))
    
;;; Annoying, but Lispworks is not able to compile the resulting lambda
;;; under x86 architectures. Moreover, it is not clear if it is the most
;;; efficient approach.
#+nil
(defun hyphen-compile-patterns (patterns level pos)
  (declare (optimize speed))
  (declare (fixnum level pos))
  (if (= level 0)
      `(lambda (word)
	(let (result
	      (word-seq (coerce word 'list))
	      )
	  (declare (fixnum pos))
	  (loop for pos from 0 to (- (length word-seq) 1) by 1
	    do
	    ,(compile-hyphen-patterns patterns (+ 1 level) pos)
	    (pop word-seq)
	    )
	  result))
    (let* ((pattern-contrib (remove-if #'(lambda (x) (characterp (first x))) patterns))
	   (trie-contrib (remove-if #'(lambda (x) (eq (first x) :pattern)) patterns))
	   (pattern-inst
	    (mapcar
	     #'(lambda (x) 
		 `(push
		   (cons (+ ,(caadr x) pos) ,(cdadr x))
		   result))
	     pattern-contrib))
	   (trie-inst
	    (cond ((null trie-contrib) nil)
		  ((null (rest trie-contrib))
		   `(when (char= 
			   (nth ,(- level 1) word-seq) 
			   ,(first (first trie-contrib)))
		     ,(hyphen-compile-patterns (rest (first trie-contrib)) (+ level 1) pos)))
		  (t
		   `(case  (nth ,(- level 1) word-seq) 
		     ,@(mapcar #'(lambda (l) 
				   (list (first l) 
					 (hyphen-compile-patterns (rest l) (+ level 1) pos))) trie-contrib)))))
	   (inst (if (null pattern-inst)
		     (if (null (rest trie-inst))
			 (first trie-inst)
		       trie-inst)
		   (if (null trie-inst)
		       (if (null (rest pattern-inst))
			   (first pattern-inst)
			 pattern-inst)
		     `(,@pattern-inst ,trie-inst)))))
      (if (and (atom (first inst)) (fboundp (first inst)))
	  inst
	`(progn  ,@inst)))))
	     

(defun hyphen-cmp-char-lists (l1 l2)
  (let (result done)
    (loop for c1 = (pop l1)
          for c2 = (pop l2)
          while (and (characterp c1) (characterp c2) (not done))
	  do
	  (if (char< c1 c2)
	      (setq result t done t)
	    (if (char> c1 c2)
		(setq done t)
	      ))
	  finally (if done result nil))))
	      
(defmethod read-hyphen-file (hyphen-trie)
  (let ((filename (make-pathname :name (cdr (assoc (language hyphen-trie)
                                                   *language-hyphen-file-list*))
                                 :type "txt" :version nil
                                 :defaults *hyphen-patterns-directory*))
        patterns exceptions count)
    (with-open-file (input filename :external-format pdf::+external-format+)
      (do ((line (read-line input nil nil))
	   mode)
	  ((null line))
	(cond ((search "pattern" line)
	       (setq mode :patterns count 0))
	      ((search "exception" line)
	       (setq mode :exception count 0))
	      ((eq mode :patterns)
	       (push  (hyphen-parse-pattern-line line) patterns)
	       (incf count))
	      ((eq mode :exception)
	       (push (hyphen-parse-exception-line line) exceptions)
	       (incf count))
	      (t ))
	(setf line (read-line input nil nil))
	)
      )
    (setq patterns (sort patterns #'hyphen-cmp-char-lists)
	  exceptions (sort exceptions #'hyphen-cmp-char-lists))
#+nil
    (setq patterns (compile-hyphen-patterns (hyphen-make-trie patterns 0) 0 0))
    ;; Lispworks x86 is not able to compile a lambda of arbitrary size
#+nil
    (progn
      (harlequin-common-lisp::toggle-source-debugging nil) 
      (setf (symbol-function (fn-find-hyphen-points hyphen-trie))
	    (compile nil patterns)))
    (setf (pattern-trie hyphen-trie) (hyphen-make-trie patterns 0))
    (setf (exception-trie hyphen-trie) (hyphen-make-trie exceptions 0))
    ))

(defparameter *american-hyphen-trie* (make-instance 'hyphen-trie :language :american))
(defparameter *french-hyphen-trie* (make-instance 'hyphen-trie :language :french))
(read-hyphen-file *american-hyphen-trie*)
(read-hyphen-file *french-hyphen-trie*)

;;(trace compile-hyphen-patterns)
(setq *print-level* nil *print-length* nil)
