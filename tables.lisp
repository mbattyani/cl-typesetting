;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package typeset)

(defvar *table* nil)

(defvar *table-row* nil)

(defclass table-cell ()
  ((content :accessor content :initarg :content)
   (box :accessor box)
   (width :accessor width :initform 0)
   (height :accessor height :initform 0)
   (background-color :accessor background-color :initform nil :initarg :background-color)
   (col-span :accessor col-span :initform 1 :initarg :col-span)
   (row-span :accessor row-span :initform 1 :initarg :row-span)))

(defclass table-row ()
  ((height :accessor height :initform nil :initarg :height)
   (splitable-p :accessor splitable-p :initform t :initarg :splitable-p)
   (background-color :accessor background-color :initform nil :initarg :background-color)
   (cells :accessor cells :initform () :initarg :cells)))

(defclass table (box v-mode-mixin)
  ((cols-widths :accessor col-widths :initform nil :initarg :col-widths)
   (border :accessor border :initform 1 :initarg :border)
   (border-color :accessor border-color :initform '(0 0 0) :initarg :border-color)
   (background-color :accessor background-color :initform nil :initarg :background-color)
   (padding :accessor padding :initform 1 :initarg :padding)
   (cell-padding :accessor cell-padding :initform 1 :initarg :cell-padding)
   (rows :accessor rows :initform ())))

(defun add-table-row (row &optional (table *table*))
  (if (rows table)
      (setf (cdr (last (rows table))) (list row))
      (setf (rows table) (list row))))

(defun add-table-cell (cell &optional (row *table-row*))
  (if (cells row)
      (setf (cdr (last (cells row))) (list cell))
      (setf (cells row) (list cell))))

(defmacro table ((&key (padding 5) (cell-padding 2) col-widths (border 1)(border-color ''(0 0 0)) inline
		       background-color)
		 &body body)
  (with-gensyms (vbox)
    `(let* ((*table* (make-instance 'table :col-widths ,col-widths :padding ,padding
				    :cell-padding ,cell-padding :background-color ,background-color
				    :border ,border :border-color ,border-color))
	    ,@(unless inline `((,vbox (make-instance 'hbox :boxes
				       (list (make-hfill-glue) *table* (make-hfill-glue))
				       :adjustable-p t)))))
      (add-box ,(if inline *table* vbox))
      ,@body
      (compute-table-size *table*)
      ,@(unless inline `((compute-natural-box-size ,vbox)))
      *table*)))

(defmacro row ((&rest args) &body body)
  `(let* ((*table-row* (make-instance 'table-row ,@args)))
    (add-table-row *table-row*)
    ,@body
    *table-row*))

(defmacro cell ((&rest args) &body body)
  `(add-table-cell (make-instance 'table-cell :content (compile-text () ,@body) ,@args)))

(defun compute-table-size (table)
  (loop	with cell-padding = (cell-padding table)
	with border = (border table)
	with cell-offset = (+ cell-padding border)
	with full-size-offset = (+ cell-offset cell-padding)
	for row in (rows table)
	for height = (or (height row) +huge-number+)
	do (loop with next-widths = (col-widths table)
	         for width = (pop next-widths)
		 for cell in (cells row)
		 for col-span = (1- (col-span cell))
		 do
		 (unless (zerop col-span)
		   (incf width (+ (* col-span full-size-offset)
				  (reduce #'+ next-widths :end col-span)))
		   (setf next-widths (subseq next-widths col-span)))
		 (setf (box cell) (make-filled-vbox (content cell) width height)
		       (width cell) width)
		 unless (height row)
		 maximize (compute-boxes-natural-size (boxes (box cell)) #'dy) into max-height
		 finally (setf height (+ (max (or (height row) 0) max-height) +epsilon+)))
	(setf (height row) height)
	(dolist (cell (cells row))
	  (setf (height cell) height
		(dy (box cell)) height)
	  (do-layout (box cell))))
  (let ((nb-rows (length (rows table)))
	(nb-cols (length (col-widths table))))
    (setf (dx table)(+ (* 2 (padding table))
		       (* 2 nb-cols (cell-padding table))
		       (* (1+ nb-cols) (border table))
		       (reduce #'+ (col-widths table))
		       +epsilon+)
	  (dy table)(+ (* 2 (padding table))
		       (* 2 nb-rows (cell-padding table))
		       (* (1+ nb-rows) (border table))
		       (reduce #'+ (rows table) :key 'height)
		       +epsilon+))))

(defmethod stroke ((table table) x y)
  (when (background-color table)
    (pdf:with-saved-state
      (pdf:set-color-fill (background-color table))
      (pdf:basic-rect x y (dx table) (- (dy table)))
      (pdf:fill-path)))
  (loop with cell-padding = (cell-padding table)
	with border = (border table)
	with cell-offset = (+ cell-padding border)
	with full-size-offset = (+ cell-offset cell-padding)
	for row in (rows table)
	for row-y = (- y (padding table) border) then (- row-y height full-size-offset)
	for height = (height row)
	do (loop for cell-x = (+ x (padding table) border) then (+ cell-x width full-size-offset)
		 for cell in (cells row)
		 for width = (width cell)
		 for height = (height cell)
		 do (pdf:with-saved-state
		      (pdf:translate cell-x row-y)
		      (pdf:with-saved-state
			(when (or (background-color cell)(background-color row))
			  (pdf:set-color-fill (or (background-color cell)(background-color row)))
			  (pdf:basic-rect 0 0 (+ width full-size-offset) (- (+ height full-size-offset)))
			  (pdf:fill-path))
			(unless (zerop (border table))
			  (pdf:set-line-width (border table))
			  (pdf:set-gray-stroke 0)
			  (pdf:basic-rect 0 0 (+ width full-size-offset)(- (+ height full-size-offset)))
			  (pdf:stroke)))
		      (stroke (box cell) cell-offset (- cell-offset))))))
