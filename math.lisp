;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package #:typeset)

(defparameter *fraction-rule-thickness-ratio* 0.03)
(defparameter *fraction-spacing-ratio* 0.16)
(defparameter *fraction-baseline-offset* 0.185)

(defmacro display-formula ((&rest args) &body body)
  `(let ((*use-exact-char-boxes* t))
    (hbox (,@args :adjustable-p t)
     :hfill ,@body :hfill)))

(defmacro fraction ((&rest args) numerator denominator)
  (with-gensyms (fraction-box spacing thickness num-box)
  `(let* ((,thickness (* *font-size* *fraction-rule-thickness-ratio*))
	  (,spacing (* *font-size* *fraction-spacing-ratio*))
	  ,num-box
	  (,fraction-box
	   (vbox (,@args :align :center)
		 (setf ,num-box (hbox (:align :center) ,@numerator))
		 (vspace ,spacing)
		 (hrule :dy ,thickness)
		 (vspace ,spacing)
		 (hbox (:align :center) ,@denominator))))
    (hspace ,spacing)
    (setf (baseline ,fraction-box) (+ (dy ,num-box) ,spacing ,thickness)
          (offset ,fraction-box) (* *font-size* *fraction-baseline-offset*))
    ,fraction-box)))

(defmacro math-super-and-sub-script ((&key (offset+ '(* 0.45 *font-size*))
					   (offset- '(* -0.2 *font-size*))
					   (font-size '(* 0.5 *font-size*)))
				     super sub)
  (with-gensyms (up-box low-box full-box space-box)
    `(let* (,up-box ,low-box ,space-box
	    (,full-box (vbox ()
			     (with-style (:font-size ,font-size)
			       (setf ,up-box (hbox () (hspace (pdf::get-font-italic-correction
							       *font* ,offset+))
						   ,@super)
				     ,space-box (vspace 0)
				     ,low-box (hbox () (hspace (pdf::get-font-italic-correction
								*font* (- ,offset- *font-size*)))
						    ,@sub))))))
      (setf (baseline ,full-box)(+ ,offset+ (internal-baseline ,up-box)))
      (setf (dy ,space-box)(- (- (baseline ,full-box)(dy ,up-box))
			    (+ ,offset- (internal-baseline ,low-box))))
      (incf (dy ,full-box)(dy ,space-box))
      ,full-box)))

