;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package #:typeset)

(defun compute-boxes-size (boxes size-fn)
  (loop for box in boxes
	sum (funcall size-fn box)
	sum (delta-size box)))

(defun compute-boxes-natural-size (boxes size-fn)
  (reduce #'+ boxes :key size-fn))

(defun compute-boxes-elasticity (boxes size-fn)
  (loop for box in boxes
	sum (funcall size-fn box) into size
	if (locked box)
	   sum (delta-size box) into size
	else
   	   sum (max-expansion box) into max-expansion
	   and sum (expansibility box) into expansibility
	   and sum (max-compression box) into max-compression
	   and sum (compressibility box) into compressibility
	   finally (return (values size max-expansion expansibility
				   max-compression compressibility))))

(defun compute-parallel-size (boxes size-fn)
  (loop for box in boxes
	for baseline = (+ (baseline box)(offset box))
	for bottom = (- (funcall size-fn box) baseline)
	maximize baseline into max-baseline
	maximize bottom into max-bottom
;	do (print (list (baseline box)(offset box)(funcall size-fn box)))
	finally (return (values (+ max-baseline max-bottom) max-baseline))))

(defmethod compute-natural-box-size (box)
  (declare (ignore box))
  )

(defmethod compute-natural-box-size ((box hbox))
  (when (boxes box)
    (setf (dx box) (compute-boxes-natural-size (boxes box) #'dx))
    (multiple-value-bind (size baseline)
	(compute-parallel-size (boxes box) #'dy)
      (setf (dy box) size (internal-baseline box) baseline))))

(defmethod (setf boxes) :after (value (box container-box))
  (declare (ignore value))
  (compute-natural-box-size box))

(defmethod initialize-instance :after
    ((box container-box) &key fixed-size &allow-other-keys)
  (unless fixed-size
    (compute-natural-box-size box)))

(defmethod compute-natural-box-size ((box vbox))
  (multiple-value-bind (size baseline)
      (compute-parallel-size (boxes box) #'dx)
    (setf (dx box) size (internal-baseline box) baseline))
  (setf (dy box) (compute-boxes-natural-size (boxes box) #'dy)))

(defmethod align-baseline (box alignment)
  (declare (ignore box alignment))
  )

(defmethod align-baseline ((box hbox) alignment)
  (setf (baseline box) (case alignment
			  (:left 0)
			  (:center (* 0.5 (dx box)))
			  (:right (dx box)))))

(defmethod align-baseline ((box vbox) alignment)
  (setf (baseline box) (case alignment
			  (:top 0)
			  (:center (* 0.5 (dy box)))
			  (:bottom (dy box)))))

(defmethod map-boxes (box x y fn)
  (funcall fn box x y))

(defmethod map-boxes ((hbox hbox) x y fn)
  (decf x (baseline hbox))
  (decf x (offset hbox))
  (funcall fn hbox x y)
  (decf y (internal-baseline hbox))
  (dolist (box (boxes hbox))
    (map-boxes box x y fn)
    (incf x (+ (dx box)(delta-size box)))))

(defmethod map-boxes ((vbox vbox) x y fn)
  (incf y (baseline vbox))
  (incf y (offset vbox))
  (funcall fn vbox x y)
  (incf x (internal-baseline vbox))
  (dolist (box (boxes vbox))
    (map-boxes box x y fn)
    (decf y (+ (dy box)(delta-size box)))))

