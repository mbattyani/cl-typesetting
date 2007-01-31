;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

(defclass box ()
  ((dx :accessor dx :initform 0 :initarg :dx)
   (dy :accessor dy :initform 0 :initarg :dy)
   (baseline :accessor baseline :initform 0 :initarg :baseline)
   (offset :accessor offset :initform *offset* :initarg :offset)
   ))

(defmethod dx (box)
  (declare (ignore box))
  0)

(defmethod (setf dx) (value box)
  (declare (ignore box))
  value)

(defmethod dy (box)
  (declare (ignore box))
  0)

(defmethod (setf dy) (value box)
  (declare (ignore box))
  value)

(defmethod baseline (box)
  (declare (ignore box))
  0)

(defmethod (setf baseline) (value box)
  (declare (ignore box))
  value)

(defmethod offset (box)
  (declare (ignore box))
  0)

(defmethod (setf offset) (value box)
  (declare (ignore box))
  value)

(defclass h-mode-mixin ()
  ())

(defclass v-mode-mixin ()
  ())

(defmethod delta-size (obj)
  (declare (ignore obj))
  0)

(defmethod max-expansion (obj)
  (declare (ignore obj))
  0)

(defmethod expansibility (obj)
  (declare (ignore obj))
  0)

(defmethod max-compression (obj)
  (declare (ignore obj))
  0)

(defmethod compressibility (obj)
  (declare (ignore obj))
  0)

(defclass soft-box (box)
  ((delta-size :accessor delta-size :initform 0)
   (max-expansion :accessor max-expansion :initform 0 :initarg :max-expansion)
   (expansibility :accessor expansibility :initform 0 :initarg :expansibility)
   (max-compression :accessor max-compression :initform 0 :initarg :max-compression)
   (compressibility :accessor compressibility :initform 0 :initarg :compressibility)
   (locked :accessor locked :initform nil :initarg :locked)))

(defmethod locked (box)
  (declare (ignore box))
  t)

(defmethod (setf locked) (value box)
  (declare (ignore box))
  value)

(defclass container-box (soft-box)
  ((boxes :accessor boxes :initform nil :initarg :boxes)
   (adjustable-p :accessor adjustable-p :initform nil :initarg :adjustable-p)
   (internal-baseline :accessor internal-baseline :initform 0)))

(defclass vbox (container-box h-mode-mixin)
  ())

(defclass hbox (container-box v-mode-mixin)
  ())

(defclass glue (soft-box)
  ())

(defclass hglue (glue h-mode-mixin)
  ())

(defclass vglue (glue v-mode-mixin)
  ())

(defclass spacing (soft-box) ;; non trimmable white space
  ())

(defclass h-spacing (spacing h-mode-mixin) 
  ())

(defclass v-spacing (spacing v-mode-mixin)
  ())

(defclass char-box (box h-mode-mixin)
  ((boxed-char :accessor boxed-char :initform nil :initarg :boxed-char)))

(defclass white-char-box (hglue)
  ((trimmable-p :accessor trimmable-p :initform nil :initarg :trimmable-p)))

(defmethod soft-box-p (box)
  (declare (ignore box))
  nil)

(defmethod soft-box-p ((box soft-box))
  t)

(defmethod char-box-p (box)
  (declare (ignore box))
  nil)

(defmethod char-box-p ((box char-box))
  t)

(defmethod white-char-box-p (box)
  (declare (ignore box))
  nil)

(defmethod white-char-box-p ((box white-char-box))
  t)

(defmethod trimmable-p (box)
  (declare (ignore box))
  nil)

(defmethod trimmable-p ((box glue))
  t)

(defmethod white-space-p (box)
  (declare (ignore box))
  nil)

(defmethod white-space-p ((box glue))
  t)

(defmethod white-space-p ((box spacing))
  t)

(defmethod hmode-p (box)
  (declare (ignore box))
  nil)

(defmethod hmode-p ((box h-mode-mixin))
  t)

(defmethod vmode-p (box)
  (declare (ignore box))
  nil)

(defmethod vmode-p ((box v-mode-mixin))
  t)

(defmethod adjust-box-dx (box dx baseline)
  (declare (ignore box dx baseline))
  nil)

(defmethod adjust-box-dx ((box hbox) dx baseline)
  (when (adjustable-p box)
    (setf (dx box) dx
	  (baseline box) baseline)))

(defmethod adjust-box-dy ((box vbox) dy baseline)
  (when (adjustable-p box)
    (setf (dy box) dy
	  (baseline box) baseline)))

(defmethod adjust-box-dy (box dy baseline)
  (declare (ignore box dy baseline))
  nil)

(defgeneric v-split (box dx dy)
 ;;; Split a v-mode box vertically into two parts
  ;; Args: dx - area width, dy - area height
  ;; Values: box-fitted, box-left, dy-left
 (:method ((box v-mode-mixin) dx dy)
  (declare (ignore dx))
  (if (> (dy box) dy)
      (values nil box dy)
      (values box nil (- dy (dy box))))))

(defgeneric boxes-left (content))

(defgeneric (setf boxes-left) (value content)
 (:method (value content)	; Do nothing if has already been adjusted by v-split.
  (declare (ignore content))
  value))

(defmethod print-object ((self char-box) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (boxed-char self) stream)))

