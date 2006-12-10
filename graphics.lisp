;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package #:typeset)

(defclass hrule (soft-box v-mode-mixin)
  ((color :accessor color :initarg :color :initform *color*)
   (stroke-fn :accessor stroke-fn :initarg :stroke-fn :initform nil)))

(defmethod adjust-box-dx ((box hrule) dx baseline)
  (setf (dx box) dx (baseline box) baseline))

(defmethod stroke ((box hrule) x y)
  (if (stroke-fn box)
      (funcall (stroke-fn box) box x y)
      (unless (zerop (dy box))
	(pdf:with-saved-state
	    (pdf:set-color-fill (color box))
	  (decf x (baseline box))
	  (pdf:basic-rect x y (dx box)(- (dy box)))
	  (pdf:fill-path)))))

(defun hrule (&rest args)
  (add-box (apply 'make-instance 'hrule args)))

(defclass jpeg-box (soft-box)
  ((file :accessor file :initform nil :initarg :file)
   (pdf-jpeg-obj :accessor pdf-jpeg-obj :initform nil :initarg :pdf-jpeg-obj)))

(defun image (&rest args &key inline &allow-other-keys)
  (if inline
      (add-box (apply 'make-instance 'jpeg-box :allow-other-keys t args))
      (let ((hbox (make-instance 'hbox :boxes (list (make-hfill-glue)
						    (apply 'make-instance 'jpeg-box :allow-other-keys t args)
						    (make-hfill-glue))
				 :adjustable-p t)))
	(compute-natural-box-size hbox)
	(add-box hbox))))

(defmethod stroke ((box jpeg-box) x y)
  (unless (pdf-jpeg-obj box)
    (setf (pdf-jpeg-obj box) (pdf:make-jpeg-image (pdf:read-jpeg-file (file box)))))
  (pdf:add-images-to-page (pdf-jpeg-obj box))
  (pdf:draw-image (pdf-jpeg-obj box) x (+ (- y (dy box))(offset box))(dx box)(dy box) 0 t))

(defclass background-jpeg-box (jpeg-box)
  ((x0 :accessor x0 :initarg :x0)
   (y0 :accessor y0 :initarg :y0)
   (fill-dx :accessor fill-dx :initform nil :initarg :fill-dx)
   (fill-dy :accessor fill-dy :initform nil :initarg :fill-dy)))

#+nil
(defmacro background-image (&rest args &key inline &allow-other-keys)
  `(add-box (make-instance 'background-jpeg-box ,@args :allow-other-keys t)))

(defclass user-drawn-box (soft-box)
  ((stroke-fn :accessor stroke-fn :initform nil :initarg :stroke-fn)))

(defun user-drawn-box (&rest args &key inline &allow-other-keys)
  (if inline
      (add-box (apply 'make-instance 'user-drawn-box :allow-other-keys t args))
      (let ((hbox (make-instance 'hbox :boxes
				 (list (make-hfill-glue)
				       (apply 'make-instance 'user-drawn-box  :allow-other-keys t args)
				       (make-hfill-glue))
				 :adjustable-p t)))
	(compute-natural-box-size hbox)
	(add-box hbox))))

(defmethod stroke ((box user-drawn-box) x y)
  (if (stroke-fn box)
      (funcall (stroke-fn box) box x y)
      (unless (zerop (dy box))
	(pdf:with-saved-state
	    (pdf:set-color-fill '(0.5 0.5 0.5))
	  (pdf:basic-rect x y (dx box)(- (dy box)))
	  (pdf:fill-path)))))

(defun stroke-colored-box (box x y color border-width border-color)
  (pdf:with-saved-state
      (pdf:set-color-fill color)
      (when border-width
	(pdf:set-color-stroke border-color)
	(pdf:set-line-width border-width))
      (pdf:basic-rect x (+ y (offset box)) (dx box)(- (dy box)))
      (pdf:fill-and-stroke)))

(defun colored-box (&rest args &key dy (offset dy) color border-width (border-color '(0 0 0))
			  &allow-other-keys)
  (add-box (apply 'make-instance 'user-drawn-box
		  :stroke-fn
		  #'(lambda(box x y)
		      (stroke-colored-box box x y color border-width border-color))
		  :allow-other-keys t :offset offset args)))

(defclass dotted-spacing (soft-box h-mode-mixin)
  ((char-pattern :accessor char-pattern :initarg :char-pattern :initform ".")
   (pattern-spacing :accessor pattern-spacing :initarg :pattern-spacing :initform 0.3)))

(defmethod stroke ((box dotted-spacing) x y)
  (let* ((pattern-width (pdf::text-width (char-pattern box) *font* *font-size*))
	 (spacing-width (* *font-size* (pattern-spacing box)))
	 (total-width (+ pattern-width spacing-width))
	 (last-x (- (+ x (dx box)(delta-size box)) pattern-width)))
    (incf y (offset box))
    (loop for x from (* total-width (ceiling x total-width)) by total-width
	  while (< x last-x)
	  do (pdf:in-text-mode
	      (pdf:move-text x y)
	      (pdf:set-font *font* *font-size*)
	      (pdf:show-text (char-pattern box))))))

(defun dotted-hfill (&rest args)
  (add-box (apply 'make-instance 'dotted-spacing
		  :dx 0 :max-expansion +huge-number+ :expansibility +huge-number+ args)))
