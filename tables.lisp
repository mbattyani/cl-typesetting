;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

;;; Thanks to Dmitri Ivanov for the splittable tables!

(in-package typeset)

(defvar *table* nil)

(defvar *table-row* nil)

(defclass table-cell ()
  ((content :accessor content :initarg :content)
   (box :accessor box)
   (width :accessor width :initform 0)
   (height :accessor height :initform 0)
   (v-align :accessor v-align :initform :top :initarg :v-align)
   (background-color :accessor background-color :initform nil :initarg :background-color)
   (col-span :accessor col-span :initform 1 :initarg :col-span)
   (row-span :accessor row-span :initform 1 :initarg :row-span)))

(defclass table-row ()
  ((height :accessor height :initform nil :initarg :height)
   (splittable-p :accessor splittable-p :initform t :initarg :splittable-p)
   (background-color :accessor background-color :initform nil :initarg :background-color)
   (cells :accessor cells :initform () :initarg :cells)))

(defclass table (box v-mode-mixin)
  ((cols-widths :accessor col-widths :initform nil :initarg :col-widths)
   (h-align :accessor h-align :initform :top :initarg :h-align)
   (splittable-p :accessor splittable-p :initform t :initarg :splittable-p)
   (border :accessor border :initform 1 :initarg :border)
   (border-color :accessor border-color :initform '(0 0 0) :initarg :border-color)
   (background-color :accessor background-color :initform nil :initarg :background-color)
   (padding :accessor padding :initform 1 :initarg :padding)
   (cell-padding :accessor cell-padding :initform 1 :initarg :cell-padding)
   (rows :accessor rows :initform ())
   (header :accessor header :initarg :header :initform nil) ; rows printed on each page
   (footer :accessor footer :initarg :footer :initform nil) ; rows printed on each page
   (header&footer-height :accessor header&footer-height :initform 0)
   ))

(defclass split-table (box v-mode-mixin)
  ((original-table :accessor original-table :initform nil :initarg :original-table)
   (rows :accessor rows :initform nil :initarg :rows)))

(defun add-table-row (row &optional (table *table*))
  (if (rows table)
      (setf (cdr (last (rows table))) (list row))
      (setf (rows table) (list row))))

(defun add-table-cell (cell &optional (row *table-row*))
  (if (cells row)
      (setf (cdr (last (cells row))) (list cell))
      (setf (cells row) (list cell))))

(defun first-or-self (arg)
 ;;; The first element of arg, if it is a list; else arg itself.
  ;; Code from Paradigms of AI Programming, Copyright (c) 1991 Peter Norvig
  (if (consp arg) (first arg) arg))

(defmacro cell-start-row-p (cell row)
  `(or (numberp (row-span ,cell))		; still untouched cell
       (eq (first-or-self (row-span ,cell)) ,row)))

(defmacro cell-end-row-p (cell row)
  `(and (consp (row-span ,cell))
        (eq (first (last (row-span ,cell))) ,row)))

(defun span-cell (rows cell col-number)
 ;;; Replace the cell's numeric row-span by the list of rows spanned
  ;; and add the cell into the cell list of each of these rows.
  ;; Args: rows    Table row sublist starting from the one where the cell was defined.
  ;;   col-number  Starts from zero.
  (setf (splittable-p (first rows)) nil)
  (loop for row in (rest rows)
        and i downfrom (1- (row-span cell)) above 0	;repeat (1- (row-span cell))
        collect row into rows-spanned
        when (> i 1)				; set all but last rows unsplittable
          do (setf (splittable-p row) nil)
        do
	(loop for j = 0 then (+ j (col-span c))
              for tail = (cells row) then (cdr tail);; hack for CLISP instead of for tail on (cells row)
              for c = (first tail)		; j is the column number of c
              while (and c (< j col-number))
              collect (first tail) into head
              finally				; insert cell between head and tail
              (setf (cells row) (nconc head (list cell) tail)))
        finally					; replace numeric row-span by the list
        (return (setf (row-span cell) (cons (first rows) rows-spanned)))))

(defun compute-row-size (table row &optional rows)
  (let ((full-size-offset (+ (border table) (* 2 (cell-padding table))))
        (height (or (height row) +huge-number+)))
    (loop with next-widths = (col-widths table)
          for cell in (cells row)
          and width = (pop next-widths)
          and col-number = 0 then (+ col-number col-span 1)
	  and cell-height = 0.0
          for col-span = (1- (col-span cell))
          and row-span = (row-span cell)
	  unless width do (error "Too many cells in this row")
	  ;; Adjust cell width for cells spanning multiple columns
          unless (zerop col-span)
            do (incf width (+ (* col-span full-size-offset)
                              (reduce #'+ next-widths :end col-span)))
               (setf next-widths (nthcdr col-span next-widths))

          ;; Fill cell with content if required
          when (cell-start-row-p cell row)
            do (setf (box cell) (make-filled-vbox (content cell) width height (v-align cell))
                     (width cell) width)
	    
          ;; A cell spanning several rows participates only in height calculation 
          ;; of the last row
          if (and (numberp row-span) (> row-span 1))
          do (span-cell rows cell col-number)
          else unless (height row)
            if (eql row-span 1)
            do (setq cell-height
		     (compute-boxes-natural-size (boxes (box cell)) #'dy))
            else if (cell-end-row-p cell row)
	    do (setq cell-height
		     (- (compute-boxes-natural-size (boxes (box cell)) #'dy)
			(reduce #'+ row-span
				:key #'height
			:end (1- (length row-span))
				:initial-value (* (1- (length row-span))
						  full-size-offset))))

	  maximize cell-height into max-height

          finally (setf height (+ (max (or (height row) 0.0) max-height) +epsilon+)))
    (setf (height row) height)
    (loop for cell in (cells row)
          for row-span = (row-span cell)
          if (eql row-span 1)
          do (setf (height cell) height
                   (dy (box cell)) height)
             (do-layout (box cell))
          else if (cell-end-row-p cell row)
          do (let ((height (reduce #'+ row-span :key #'height
                                   :initial-value (* (1- (length row-span))
                                                     full-size-offset))))
               (setf (height cell) height
                     (dy (box cell)) height)
               (do-layout (box cell))))
    height))

(defmethod compute-table-size (table)
  (loop for rows on (header table)
        do (compute-row-size table (first rows) rows))
  (loop for rows on (rows table)
        do (compute-row-size table (first rows) rows))
  (loop for rows on (footer table)
        do (compute-row-size table (first rows) rows))
  (let ((nb-h&f (+ (length (header table))(length (footer table))))
	(nb-rows (length (rows table)))
	(nb-cols (length (col-widths table))))
    (setf (header&footer-height table)
	  (+ (* 2 nb-h&f (cell-padding table))
	     (* nb-h&f (border table))
	   (reduce #'+ (header table) :key 'height)
	   (reduce #'+ (footer table) :key 'height)))
    (setf (dx table)(+ (* 2 (padding table))
		       (* 2 nb-cols (cell-padding table))
		       (* (1+ nb-cols) (border table))
		       (reduce #'+ (col-widths table))
		       +epsilon+)
	  (dy table)(+ (* 2 (padding table))
		       (* 2 nb-rows (cell-padding table))
		       (* (1+ nb-rows) (border table))
		       (reduce #'+ (rows table) :key 'height)
		       (header&footer-height table)
		       +epsilon+))))

(defmethod v-split ((table table) dx dy)
  "Factor out rows that fit and return a split table + the table."
  ;; Treat unsplittable rows as a single unit - for this purpose,
  ;; group the rows list into the following form:
  ;;
  ;;     ( (group1-height row1 row2 ...)
  ;;       (group2-height row7)
  ;;       (group3-height row8 row9 ...) )
  ;;
  (if (splittable-p table)
      (with-slots (header footer border padding cell-padding) table ;(print (rows table))
	(loop with fitted-rows = ()
	      with header&footer-height = (header&footer-height table)
	      with padding&border = (+ (* 2 cell-padding) border)
	      with current-height = (+ border padding)
	      with available-height = (- dy header&footer-height)
	      with row-groups = (loop with height = 0
				      and  rows = ()
				      for row in (rows table)
				      do
				      (incf height (+ (height row) padding&border))
				      (push row rows)
				      when (splittable-p row)
				      collect (cons height (nreverse rows))
				      and do (setf height 0 rows nil))
	      with rows-remaining = (rows table)
	      
	      for (group-height . rows) in row-groups
	      while (<= (+ current-height group-height) available-height)
	      
	      do (dolist (r rows)
		   (push r fitted-rows)
		   (pop rows-remaining))
	      (incf current-height group-height)
	      
	      finally
	      (when fitted-rows
		(setq boxes (append header  footer))
		;; reduce rows to output
		(setf (rows table) rows-remaining)
		;; reduce space required by table (don't subtract header/footer)
		(decf (dy table) current-height)
		(return (values (make-instance 'split-table :rows (nreverse fitted-rows)
					       :original-table table
					       :dx (dx table)
					       :dy (+ current-height header&footer-height))
				table
				(- dy current-height header&footer-height))))
	      (return (values nil table dy))))
      (values nil table dy)))

(defun stroke-table (table x y rows dy)
  (let* ((padding (padding table))
         (border (border table)))
    (when (background-color table)
      (pdf:with-saved-state
        (pdf:set-color-fill (background-color table))
        (if (or (zerop padding) (zerop border))
            (pdf:basic-rect x y (dx table) (- dy))
            ;; External colorful padding should not breach border
            (pdf:basic-rect (+ x padding border) (- y padding border)
                            (- (dx table) (* 2 (+ padding border)))
                            (- (* 2 (+ padding border)) dy)))
        (pdf:fill-path)))
    (loop with cell-padding = (cell-padding table)
          with cell-offset = (+ cell-padding border)
          with full-size-offset = (+ cell-offset cell-padding)
          for row in (append (header table) rows (footer table))
          for row-y = (- y padding border) then (- row-y height full-size-offset)
          and height = (height row)
          do (loop for cell-x = (+ x padding border) then (+ cell-x width full-size-offset)
                   for cell in (cells row)
                   for width = (width cell)
                   for height = (height cell)
                   when (cell-start-row-p cell row)
                   do (pdf:with-saved-state
                        (pdf:translate cell-x row-y)
                        (pdf:with-saved-state
                          (when (or (background-color cell)(background-color row))
                            (pdf:set-color-fill (or (background-color cell)
                                                    (background-color row)))
                            (pdf:basic-rect 0 0 (+ width full-size-offset)
                                            (- (+ height full-size-offset)))
                            (pdf:fill-path))
                          (unless (zerop border)
                            (pdf:set-line-width (border table))
                            (pdf:set-gray-stroke 0)
                            (pdf:basic-rect 0 0 (+ width full-size-offset)
                                            (- (+ height full-size-offset)))
                            (pdf:stroke)))
                        (stroke (box cell) cell-offset (- cell-offset)))))))

(defmethod stroke ((table split-table) x y)
  (stroke-table (original-table table) x y (rows table) (dy table)))

(defmethod stroke ((table table) x y)
  (stroke-table table x y (rows table) (dy table)))

;;; Convenience macros 

(defmacro table ((&key col-widths
                       (padding 5) (cell-padding 2)
                       (border 1) (border-color #x00000) 
		       background-color
                       header footer
                       (splittable-p (and (or header footer) t)))
		 &body body)
  (with-gensyms (hbox)
    `(let* ((*table* (make-instance 'table :splittable-p ,splittable-p
                                    ,@(when header `((:header ,header)))
                                    ,@(when footer `((:footer ,footer)))
                                    :col-widths ,col-widths
                                    :padding ,padding :cell-padding ,cell-padding
                                    :background-color ,background-color
				    :border ,border :border-color ,border-color)))
      (add-box *table*)
      ,@body
      (compute-table-size *table*)
      *table*)))

(defmacro header-row ((&rest args) &body body)
  `(let* ((*table-row* (make-instance 'table-row :splittable-p nil ,@args)))
    (setf (header *table*) (nconc (header *table*) (list *table-row*)))
    ,@body
    *table-row*))

(defmacro footer-row ((&rest args) &body body)
  `(let* ((*table-row* (make-instance 'table-row :splittable-p nil ,@args)))
    (setf (footer *table*) (nconc (footer *table*) (list *table-row*)))
    ,@body
    *table-row*))

(defmacro row ((&rest args) &body body)
  `(let ((*table-row* (make-instance 'table-row ,@args)))
     (add-table-row *table-row*)
     ,@body
     *table-row*))

(defmacro cell ((&rest args) &body body)
  `(add-table-cell (make-instance 'table-cell :content (compile-text () ,@body) ,@args)))

#|
;(let ((pdf:*page*(setq content
(defun make-test-table (&optional (inline t) (splittable-p nil) (border 1/2))
  (typeset:table (:col-widths '(20 40 60 80 120)
                  :background-color :yellow :border border
                  :inline inline :splittable-p splittable-p)
    (typeset::row (:background-color :green)
      (typeset:cell (:row-span 2 :background-color :blue)
                    "1,1 2,1  row-span 2")
      (typeset:cell () "1,2")
      (typeset:cell (:col-span 2 :row-span 3 :background-color :red)
                    "1,3 1,4 - 3,3 3,4  col-span 2 row-span 3")
      (typeset:cell () "1,5"))
    (typeset::row ()
      (typeset:cell () "2,2")
      (typeset:cell (:row-span 2 :background-color :blue) "2,5 3,5  row-span 2"))
    (typeset::row (:background-color :green)
      (typeset:cell (:col-span 2) "3,1 3,2  col-span 2"))
    (typeset::row ()
      (typeset:cell () "4,1")
      (typeset:cell () "4,2")
      (typeset:cell () "4,3")
      (typeset:cell () "4,4")
      (typeset:cell () "4,5"))
) )

(defun test-table (table
                   &optional (file (lw:current-pathname "../examples/test-table.pdf"))
                   &aux (margins '(72 72 72 50)))
  (with-document ()
    (let ((content (compile-text ()
                     (setq table (make-test-table t nil))
                     (make-test-table t nil 0)
                     (vspace 280)
                     ;(setq table (make-test-table t t 0))
                     ;(add-box (make-test-table t t))
                  )))
      (draw-pages content :margins margins)); :header header :footer footer) ;:break :after
    (draw-pages (setq table (make-test-table t t 1)) :margins margins)
    (when pdf:*page* (typeset::finalize-page pdf:*page*))
    (pdf:write-document file))
  table)

(setq table (test-table nil))
 |#