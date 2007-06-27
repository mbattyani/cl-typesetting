;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

(defmethod stroke (box x y)
  (declare (ignore box x y))
  )

(defmethod stroke :before ((box char-box) x y)
  (when (functionp *pre-decoration*) 
    (funcall *pre-decoration*
	     box
	     x (+ y (baseline box) (offset box))
	     (dx box) (- (dy box)))))

(defmethod stroke :after ((box char-box) x y)
  (when (functionp *post-decoration*)
    (funcall *post-decoration*
	     box
	     x (+ y (baseline box) (offset box))
	     (dx box) (- (dy box)))))


(defmethod stroke ((hbox hbox) x y)
  (decf x (baseline hbox))
  (decf x (offset hbox))
  (decf y (internal-baseline hbox))
  (dolist (box (boxes hbox))
    (stroke box x y)
    (incf x (+ (dx box)(delta-size box)))))

(defmethod stroke ((vbox vbox) x y)
  (incf y (baseline vbox))
  (incf y (offset vbox))
  (incf x (internal-baseline vbox))
  (dolist (box (boxes vbox))
    (stroke box x y)
    (decf y (+ (dy box)(delta-size box)))))

(defmethod stroke ((box char-box) x y)
  (pdf:in-text-mode
   (pdf:move-text x (+ y (offset box)))
   (pdf:set-font *font* *font-size*)
   (pdf:set-text-x-scale (* *text-x-scale* 100))
   (pdf:show-char (boxed-char box))))

(defmethod stroke ((line text-line) x y)
  (decf y (internal-baseline line))
  (let ((string ())
	(offset 0)
	(nb-spaces 0)
	text-x text-y
	(text-chunk ()))
    (labels ((end-string ()
	       (when string
		 (push (coerce (nreverse string) unicode-string-type) text-chunk)
		 (setf string nil)))
	     (end-text-chunk ()
	       (end-string)
	       (setf nb-spaces 0)
	       (when (some 'stringp text-chunk) 
		 (pdf:in-text-mode
		  (pdf:move-text text-x text-y)
		  (pdf:set-font *font* *font-size*)
		  (pdf:set-text-x-scale (* *text-x-scale* 100))
		  (pdf:draw-spaced-strings (nreverse text-chunk)))
		 (setf text-chunk nil)))
	     (add-char (char-box)
	       (when (/= offset (offset char-box))
		 (end-text-chunk)
		 (setf offset (offset char-box)
		       text-y (+ offset y)))
	       (unless (or string text-chunk)
		 (setf text-x x text-y (+ offset y)))
               (push (boxed-char char-box) string))
	     (add-spacing (space)
	       (setf space (round (/ (* -1000 space) *text-x-scale*) *font-size*))
	       (unless (zerop space)
		 (end-string)
		 (incf nb-spaces)(when (> nb-spaces 10)(end-text-chunk))
		 (when (or string text-chunk)
		   (push space text-chunk)))))
      (loop for box in (boxes line)
	    for size = (+ (dx box)(delta-size box))
	    do
	    (cond
	      ((or (functionp *pre-decoration*)
		   (functionp *post-decoration*))
	       (end-text-chunk)
	       (stroke box x y))
	      ((char-box-p box)(add-char box))
	      ((white-space-p box) (add-spacing size))
	      (t (end-text-chunk)(stroke box x y)))
	    (incf x size))
      (end-text-chunk))))

(defmethod stroke ((style text-style) x y)
  (declare (ignore x y))
  (when (font style)
    (setf *font* (font style)))
  (when (font-size style)
    (setf *font-size* (font-size style)))
  (when (text-x-scale style)
    (setf *text-x-scale* (text-x-scale style)))
  (when (color style)
    (setf *color* (color style))
    (pdf::set-color-fill *color*))
  (when (pre-decoration style)
    (setf *pre-decoration* (pre-decoration style)))
  (when (post-decoration style)
    (setf *post-decoration* (post-decoration style))))
