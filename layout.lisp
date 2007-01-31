;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

(defclass hyphen-box ()
  ((cost :accessor cost :initform 0 :initarg :cost)
   (char-box :accessor char-box :initform nil :initarg :char-box)))

(defmethod convert-hyphen-to-char-box ((box hyphen-box))
  (char-box box))

(defmethod convert-hyphen-to-char-box (box)
  box)

(defmethod cut-point-p (box)
  (declare (ignore box))
  nil)

(defmethod cut-point-p ((box (eql :eol)))
  t)

(defmethod cut-point-p ((box hyphen-box))
  t)

(defmethod cut-point-p ((box white-char-box))
  t)

(defmethod cut-point-p ((box char-box))
  (and (pdf:hyphen-char *font*)(char= (pdf:hyphen-char *font*)(boxed-char box))))

(defun non-empty-style-p (style)
  (and (style-p style)
       (or (font style)
           (font-size style)
           (text-x-scale style)
           (color style)
           (background-color style)
           (h-align style)
           (left-margin style)
           (right-margin style)
	   (pre-decoration style)
	   (post-decoration style))))

;;This would need a complete rewrite...
(defmethod fit-lines ((content text-content) dx dy &optional (v-align :top)(advance t))
  (let* ((boxes (boxes content))
         (boxes-left boxes)
	 (text-lines ())
	 (line-boxes ())
	 (last-cut-point nil)
	 (last-cut-boxes-left ())
	 (trimming t)
	 (last-style (make-instance 'text-style))
	 (x 0)
	 (next-x 0)
	 (last-x dx))
    (when (member v-align '(:center :bottom))
      (push (make-vfill-glue) text-lines))
    (labels ((return-lines (&optional (dy-left dy))
	       ;; Optimize boxes-left slightly by ignoring empty style
	       (when (and boxes-left (non-empty-style-p last-style))
		 (push last-style boxes-left))
	       ;; Detect pathology where text-lines is a singleton spurious text-line
	       ;; consisting only of instances of
	       ;;  (1) text-style, or
	       ;;  (2) h-spacing (including explicit or made by make-hfill-glue).
	       (cond ((or (null text-lines)
			  (and (null (rest text-lines))
			       (every (lambda (box) (or (style-p box)
							(typep box 'h-spacing)))
				      (boxes (first text-lines)))))
		      (setq text-lines nil))
		     ((member v-align '(:center :top))
		      (setq text-lines (cons (make-vfill-glue) text-lines))))
	       (when advance 
		 (setf (boxes-left content) boxes-left))
	       (return-from fit-lines (values (nreverse text-lines) dy-left)))
	     (abort-line ()
	       (setf boxes-left boxes)
	       (return-lines))
	     (init-line ()
	       (save-style last-style)
	       (setf line-boxes nil last-cut-point nil last-cut-boxes-left nil
		     trimming t next-x 0 last-x (- dx *right-margin*)))
	     (start-line ()
	       (setf last-x (- dx *right-margin*) trimming nil)
	       (unless (zerop *left-margin*)
		 (push (make-instance 'h-spacing :dx *left-margin*) line-boxes)
		 (incf next-x *left-margin*))
	       (when (member *h-align* '(:center :right))
		 (push (make-hfill-glue) line-boxes)))
	     (next-line (line-boxes)
	       (if line-boxes
		 (let ((text-line (make-instance 'text-line :dx dx :adjustable-p t)))
		   (setf line-boxes (boxes-left-trim line-boxes))
		   (when (member *h-align* '(:center :left :left-not-last))
		     (push (make-hfill-glue) line-boxes))
		   (unless (zerop *right-margin*)
		     (push (make-instance 'h-spacing :dx *right-margin*) line-boxes))
		   (setf (boxes text-line)(nreverse line-boxes))
		   (decf dy (dy text-line))
		   (when (minusp dy) (abort-line))
		   (setf boxes boxes-left)
		   (when (and text-lines (not (zerop dy)))
		     (push (make-inter-line-glue (dy text-line)) text-lines))
		   (push text-line text-lines))
		 (setf boxes boxes-left))
	       (init-line)))
      (loop for box = (pop boxes-left)
	    for box-size = (dx box)
	    while box
	    do
	    (setf next-x (+ x box-size))
	    (when (and (cut-point-p box)(> x 0))
	      (setf last-cut-point box
		    last-cut-boxes-left boxes-left))
	    (when (and trimming (not (trimmable-p box))(> box-size 0))
	      (start-line))
	    (when (style-p box)
	      (use-style box))
	    (cond
	      ((vmode-p box)
	       (next-line line-boxes)
	       (decf dy (dy box))
	       (if (minusp dy)
		     ;; try to split object
		   (multiple-value-bind (box-fitted box-left dy-left) (v-split box dx (+ dy (dy box)))
		     (when box-left
		       (push box-left boxes-left))
		     (when box-fitted
		       (push box-fitted text-lines))
		       (return-lines dy-left))
		   (push box text-lines)))
	      ((and trimming (trimmable-p box)) nil)
	      ((eq box :eol)
	       (when (eq *h-align* :left-not-last)
		 ;; incredibly ugly hack - need to get rid of the
		 ;; superfluous box added on the last line.
		 (let ((fbox (find-if (lambda (box)
					    (and (typep box 'h-spacing)
						 (eql (expansibility box) +huge-number+)))
					  line-boxes)))
		   (if fbox (setq line-boxes (remove fbox line-boxes)))))
	       (when (eq *h-align* :justified)
		 (push (make-hfill-glue) line-boxes))
	       (next-line line-boxes))
	      ((eq box :fresh-page)
	       (unless (or (null text-lines)
			   (and (null (rest text-lines))
				(every (lambda (box) (or (style-p box)
							 (typep box 'h-spacing)
							 (typep box 'v-spacing)))
				       (boxes (first text-lines)))))
		 (next-line line-boxes)
		 (return-lines 0)))
	      ((eq box :eop)
	       (next-line line-boxes)
	       (return-lines 0))
	      ((<= next-x last-x) (push box line-boxes))
	      ((and last-cut-point (not (eq box last-cut-point)))
	       (setf boxes-left last-cut-boxes-left)
	       (next-line (cons (convert-hyphen-to-char-box last-cut-point)
				(cdr (member last-cut-point line-boxes)))))
	      (t (unless line-boxes (error "could not fit anything"))
	       (next-line line-boxes) (setf boxes-left (cons box boxes-left))))
	    (setf x next-x)
	    finally
	    (when line-boxes (next-line line-boxes)))
      (return-lines))))

(defmethod do-layout (box)
  (declare (ignore box))
  )

(defmethod do-layout ((hbox hbox))
  (loop with dy = (dy hbox) and baseline = (internal-baseline hbox)
	for box in (boxes hbox)
	do (adjust-box-dy box dy baseline))
  (spread-boxes (boxes hbox) (dx hbox) #'dx)
  (map nil 'do-layout (boxes hbox)))

(defmethod do-layout ((vbox vbox))
  (loop with dx = (dx vbox) and baseline = (internal-baseline vbox)
	for box in (boxes vbox)
	do (adjust-box-dx box dx baseline))
  (spread-boxes (boxes vbox) (dy vbox) #'dy)
  (map nil 'do-layout (boxes vbox)))

(defun spread-boxes (boxes final-size size-fn &optional (first-pass t))
  (when first-pass
    (dolist (box boxes)
      (when (soft-box-p box)
	(setf (locked box) nil
	      (delta-size box) 0))))
  (multiple-value-bind (size max-expansion expansibility max-compression compressibility)
      (compute-boxes-elasticity boxes size-fn)
    (let ((final-pass t))
      (cond
	((and (< size (- final-size +epsilon+)) (> max-expansion +epsilon+))
	 (let ((delta-size (- final-size size)))
	   (when (> delta-size max-expansion) (setf delta-size max-expansion))
	   (setf delta-size (/ delta-size expansibility))
	   (dolist (box boxes)
	     (unless (locked box)
	       (let* ((max-expansion (max-expansion box))
		      (expansibility (expansibility box))
		      (box-delta-size (* expansibility delta-size)))
		 (when (> box-delta-size max-expansion)
		   (setf (locked box) t
			 box-delta-size max-expansion
			 final-pass nil))
		 (setf (delta-size box) box-delta-size))))))
	((and (> size (+ final-size +epsilon+))(> max-compression +epsilon+))
	 (let ((compression (- size final-size)))
	   (when (> compression max-compression) (setf compression max-compression))
	   (setf compression (/ compression compressibility))
	   (dolist (box boxes)
	     (unless (locked box)
	       (let* ((max-compression (max-compression box))
		      (compressibility (compressibility box))
		      (box-compression (* compressibility compression)))
		 (when (> box-compression max-compression)
		   (setf (locked box) t
			 box-compression max-compression
			 final-pass nil))
		 (setf (delta-size box) (- box-compression))))))))
      (unless final-pass
	(spread-boxes boxes final-size size-fn nil)))))

(defun boxes-left-trim (boxes)
  (let ((start (position-if-not #'trimmable-p boxes)))
      (if (and start (> start 0))
	  (subseq boxes start)
	  boxes)))

(defun boxes-right-trim (boxes)
  (let ((stop  (position-if-not #'trimmable-p boxes :from-end t)))
    (when stop
      (incf stop)
      (if (< stop (length boxes))
	  (subseq boxes 0 stop)
	  boxes))))

(defun boxes-trim (boxes)
  (let ((start (position-if-not #'trimmable-p boxes))
	(stop  (position-if-not #'trimmable-p boxes :from-end t)))
    (when (and start stop)
      (incf stop)
      (if (or (> start 0)(< stop (length boxes)))
	  (subseq boxes start stop)
	  boxes))))

(defmethod boxes-left ((content text-content))
  (boxes content))

(defmethod (setf boxes-left) (value (content text-content))
 ;;; Args: value - can start form a text-style.
  (setf (boxes content) value))

(defun make-filled-vbox (content dx dy &optional (v-align :top) (advance t))
 ;;; Args: advance  If true, assign new boxes-left.
  (with-text-content (content)
    (multiple-value-bind (boxes) (fit-lines content dx dy v-align advance)
      (when boxes
	(let* ((vbox (make-instance 'vbox  :dx dx :dy dy  :boxes boxes  :fixed-size t)))
	  (do-layout vbox)
	  vbox)))))

#+nil
(defun make-filled-vbox (content dx dy &optional (v-align :top))
  (with-text-content (content)
    (multiple-value-bind (lines boxes-left) (split-lines (boxes content) dx dy v-align)
      (when lines
	(let* ((box (make-instance 'vbox :dx dx :dy dy :boxes lines :fixed-size t)))
	  (do-layout box)
	  (setf (boxes content) boxes-left)
	  box)))))

