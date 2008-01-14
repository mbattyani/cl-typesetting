;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

;;; Thanks to Dmitri Ivanov for the splittable tables!

(in-package #:typeset)

(defvar *table* nil)

(defvar *table-row* nil)

(defclass table-cell ()
  ((content :accessor content :initarg :content)
   (box :accessor box)
   (width :accessor width :initform 0)
   (height :accessor height :initform 0)
   (v-align :accessor v-align :initform :top :initarg :v-align)
   (background-color :accessor background-color :initform nil :initarg :background-color)
   ;; col-span equal to 0 (zero) means that the cell spans all rows
   ;; from the current row to the last row of the table
   (col-span :accessor col-span :initform 1 :initarg :col-span)
   ;; row-span equal to 0 (zero) means that the cell spans all columns
   ;; from the current column to the last column of the table
   (row-span :accessor row-span :initform 1 :initarg :row-span)
   ;; Quad specifying the border to be drawn between table border and cell padding;
   ;; Borders of a cell count for its external height (unless it is limited by the height
   ;; of the containing row) and tighten the width of its content box.
   ;; Hence, they participate in the calculation of total row or table height,
   ;; but do not increase column width.
   ;; If is symbol, has special mening: 
   ;; - T (default) means "inherit" from table (or row?),
   ;; - NIL, displays no additional border and even supresses the table border.
   (border :accessor border  :initform t  :initarg :border)))

(defclass table-row ()
  ((height :accessor height :initform nil :initarg :height)
   (splittable-p :accessor splittable-p :initform t :initarg :splittable-p)
   (background-color :accessor background-color :initform nil :initarg :background-color)
   (cells :accessor cells :initform () :initarg :cells)))

(defclass table (box v-mode-mixin)
  ((cols-widths :accessor col-widths :initform nil :initarg :col-widths)
   (h-align :accessor h-align :initform :left :initarg :h-align)
   (splittable-p :accessor splittable-p :initform t :initarg :splittable-p)
   (border :accessor border :initform 1 :initarg :border)
   (border-color :accessor border-color :initform '(0 0 0) :initarg :border-color)
   (background-color :accessor background-color :initform nil :initarg :background-color)
   (padding :accessor padding :initform 1 :initarg :padding)
   (cell-padding :accessor cell-padding :initform 1 :initarg :cell-padding)
   (rows :accessor rows :initform ())
   (header :accessor header :initform nil) ; rows printed on each page
   (footer :accessor footer :initform nil) ; rows printed on each page
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

(defmacro cell-continue-row-p (cell row)
  `(and (consp (row-span ,cell))
        (not (eq (first (row-span ,cell)) ,row))))

(defmacro cell-end-row-p (cell row)
  `(and (consp (row-span ,cell))
        (eq (first (last (row-span ,cell))) ,row)))

(defun span-cell (rows cell col-number)
 ;;; Replace the cell's numeric row-span by the list of rows spanned
  ;; and add the cell into the cell list of each of these rows.
  ;; Args: rows        Table row sublist starting from the one where the cell was defined.
  ;;       col-number  Starts from zero.
  (setf (splittable-p (first rows)) nil)
  (do* ((rows-spanned (list (first rows)))
        (row-span (row-span cell))		; zero row-span means all the rows
        (i (if (= row-span 0) most-positive-fixnum (1- row-span)) (1- i))
        (tail (rest rows) rest)
        (rest (rest tail) (rest rest)))
       ((or (null tail) (= i 0))		; replace numeric row-span by the list
        (setf (row-span cell) (nreverse rows-spanned)))
    (let ((row (first tail)))
      (push row rows-spanned)
      (when (and (> i 1) rest)			; set all but last rows unsplittable
        (setf (splittable-p row) nil))
      (do* ((head () (cons c head))
            (j 0 (+ j (col-span c)))
            (tail (cells row) (rest tail))
            (c (first tail) (first tail)))	; j is the column number of c
           ((or (null c) (>= j col-number))	; insert cell between head and tail
            (setf (cells row) (nreconc head (cons cell tail))) )) )))
      
(defun reduce+nullable (rows &key (key #'height) (initial-value 0) end)
 ;;; Summarize height of the rows in subsequence (subseq rows 0 length).
  ;; Returns NIL as soon as some of the height values is null.
  (do ((i 0 (1+ i))
       (tail rows (rest tail)))
      ((or (null tail) (and end (>= i end)))
       (values initial-value i))
    (let* ((elt (first tail))
           (value (if key (funcall key elt) elt)))
      (if (numberp value)
          (incf initial-value value)
          (return nil)))))

(defun row-cell-col-number (cell cells)
 ;;; Value: column number of the cell with cells
  (do ((j 0 (+ j col-span))			; step by col-span
       (tail cells (rest tail))
       c
       col-span)
      ((null tail)
       nil)
    (cond ((eq cell (setq c (first tail)))
           (return j))
          ((= (setq col-span (col-span c)) 0)
           (return most-positive-fixnum)))))

(defun reorder-row-cells (row)
 ;;; Sort the row's cells correctly as there are more than one cells that
  ;; - are defined by the previous row or rows, and
  ;; - are spanning to this row.
  ;; Value: new list
  (flet ((predicate (cell) (cell-continue-row-p cell row)))
    (do* ((cells (cells row))
          (continue (sort (mapcar (lambda (cell)	; alist of cells defined above
                                    (cons cell (row-cell-col-number
                                                cell (cells (first (row-span cell))))))
                                  (remove-if-not #'predicate cells))
                          #'<
                          :key #'cdr))
          (start (remove-if #'predicate cells))		; list cells defined here
          cell
          (j 0 (+ j (col-span cell)))
          (acc ()))
         ((cond ((null continue)
                 (setq acc (nreconc acc start))
                 t)
                ((null start)
                 (setq acc (nreconc acc (mapcar #'car continue)))
                 t))
          acc)
      (if (>= j (cdar continue))
          (setq cell (caar continue)			; found in above
                continue (cdr continue))
          (setq cell (pop start)))			; take from current
      (push cell acc))))

(defun compute-row-size (table row rows)
  (let* ((full-size-offset (+ (border table) (* 2 (cell-padding table))))
         (height-expr (height row))
         (height (if (numberp height-expr)
                     height-expr
                     +huge-number+))
         (continued-count 0))				; nr of spanned cells defined above
    (loop with next-widths = (col-widths table)
          with col-count = (length next-widths)
          for cell in (cells row)
          and width = (pop next-widths)
          and col-number = 0 then (+ col-number actual-col-span 1)
	  and cell-height = 0.0
          for col-span = (col-span cell)
          and row-span = (row-span cell)
          and cell-border = (border cell)
          for actual-col-span = (1- (if (= col-span 0)	; allowed for the very last column
                                        (- col-count col-number)
                                        col-span))
          and cell-borders-width 
              = (cond ((symbolp cell-border) 0)
                      ((numberp cell-border) (+ cell-border cell-border))
                      ((with-quad (left-border nil right-border) cell-border
                         (+ left-border right-border))))
          and cell-borders-height
              = #1=(cond ((symbolp cell-border) 0)
                         ((numberp cell-border) (+ cell-border cell-border))
                         ((with-quad (left-border top-border nil bottom-border) cell-border
                            (+ top-border bottom-border))))
	  unless width do (error "Too many cells in this row")
	  ;; Adjust the cell width for cells spanning multiple columns
          unless (= actual-col-span 0)
            do (incf width (+ (* actual-col-span full-size-offset)
                              (reduce #'+ next-widths :end actual-col-span)))
               (setf next-widths (nthcdr actual-col-span next-widths))

          ;; Fill in the cell with content as required and when it spans several rows
          ;; of fixed height, summarize these height values for layout purpose.
          if (cell-start-row-p cell row)
          do (setf (box cell) (make-filled-vbox (content cell)
                                  (- width cell-borders-width)
                                  (cond ((eql row-span 1)
                                         height)
                                        ((numberp row-span)
                                         (multiple-value-bind (height row-span)
                                             (reduce+nullable rows
                                                              :end (if (= row-span 0)
                                                                       most-positive-fixnum
                                                                       row-span))
                                           (if height	
                                               (+ height (* (1- row-span) ; actual row-span
                                                            full-size-offset))
                                               +huge-number+)))
                                        ((reduce+nullable row-span	; list of rows
                                              :initial-value (* (1- (length row-span))
                                                                full-size-offset)))
                                        (+huge-number+))
                                  (v-align cell))
                     (width cell) width)
          else if (cell-continue-row-p cell row)
               do (incf continued-count)
          end
          ;; A cell spanning several rows participates only in height calculation 
          ;; of the last row
          if (and (numberp row-span) (/= row-span 1))	; 0 or >1
          do (span-cell rows cell col-number)
          else unless (numberp height-expr)
            if (eql row-span 1)
            do (setq cell-height (+ (compute-boxes-natural-size (boxes (box cell)) #'dy)
                                    cell-borders-height))
            else if (cell-end-row-p cell row)
	    do (setq cell-height
		     (- (+ (compute-boxes-natural-size (boxes (box cell)) #'dy)
                           cell-borders-height)
			(reduce #'+ row-span
				:key #'height
                                :end (1- (length row-span))
				:initial-value (* (1- (length row-span))
						  full-size-offset))))

	  maximize cell-height into max-height

          ;finally (setf height (+ (max (or (height row) 0.0) max-height) +epsilon+))
          finally (setf height (+ (cond ((numberp height-expr)
                                         (max height-expr max-height))
                                        ((consp height-expr)
                                         (apply (first height-expr)
                                                (subst max-height :text-height
                                                       (rest height-expr))))
                                        (t max-height))
                                  +epsilon+)))
    (setf (height row) height)
    (when (> continued-count 1)			  ; if two or more row-spanned cells are
      (setf (cells row) (reorder-row-cells row))) ; continued into this row, sort all cells
    (loop for cell in (cells row)
          for row-span = (row-span cell)
          and cell-border = (border cell)
          for cell-borders-height = #1#
          if (eql row-span 1)
          do (setf (height cell) height
                   (dy (box cell)) (- height cell-borders-height))
             (do-layout (box cell))
          else if (cell-end-row-p cell row)
          do (let ((height (reduce #'+ row-span :key #'height
                                   :initial-value (* (1- (length row-span))
                                                     full-size-offset))))
               (setf (height cell) height
                     (dy (box cell)) (- height cell-borders-height))
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
  (declare (ignore dx))
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
	      with full-size-offset = (+ (* 2 cell-padding) border)
	      with current-height = (+ border padding)
	      with available-height = (- dy header&footer-height)
	      with row-groups = (loop with height = 0
				      and  rows = ()
				      for row in (rows table)
				      do
				      (incf height (+ (height row) full-size-offset))
				      (push row rows)
				      when (splittable-p row)
				      collect (cons height (nreverse rows))
				      and do (setf height 0 rows nil))
	      with rows-remaining = (rows table)
	      
              ;; Do not count the bottom table padding in available-height.
	      for (group-height . rows) in row-groups
	      while (<= (+ current-height group-height) available-height)
	      do (dolist (r rows)
		   (push r fitted-rows)
		   (pop rows-remaining))
             (incf current-height group-height)
	      
           finally (return
                       (if fitted-rows
                           (let ((height (+ current-height header&footer-height)))
                             ;; for split-table height, take bottom padding into account
                             (incf height (min padding (- available-height height)))
                             ;; reduce rows to output
                             (setf (rows table) rows-remaining)
                             ;; reduce space required by the rest of the table subtracting
                             ;; neither top border nor bottom table padding
                             ;; nor header/footer
                             (decf (dy table) (- current-height border padding))
                             (values (make-instance 'split-table
                                                    :rows (nreverse fitted-rows)
                                                    :original-table table
                                                    :dx (dx table)  :dy height)
                                     table
                                     (- dy height)))
                           (values nil table dy)))))
      (values nil table dy)))

(defun multi-line (points widths)
 ;;; Helper akin to polyline, but skips zero-width lines
  ;; Args: points - list of dotted(!) pairs.
  (do ((prev-x (caar points))
       (prev-y (cdar points))
       (other-points (rest points) (rest other-points)))
      ((null (and other-points widths)))
    (let ((width (pop widths))
          (x (caar other-points))
          (y (cdar other-points)))
      (if (zerop width)
          (setq prev-x x prev-y y)
          (progn 
            (pdf:set-line-width width)
            (when prev-x (pdf:move-to prev-x prev-y))
            (pdf:line-to x y)
            (setq prev-x nil prev-y nil))))))

(defun stroke-table (table x y rows dy)
  (let* ((padding (padding table))
         (border (border table))
         (half-border (/ border 2))			; for more careful drawing
         (*table* table))
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
          with full-size-offset = (+ cell-padding cell-padding border)
          for row in (append (header table) rows (footer table))
          for row-y = (- y padding border) then (- row-y height full-size-offset)
          and height = (height row)
          and *table-row* = row
          do (loop for cell-x = (+ x padding border) then (+ cell-x width full-size-offset)
                   for cell in (cells row)
                   for width = (width cell)
                   for height = (height cell)
                   and cell-border = (border cell)
                   and cell-offset-x = cell-padding
                   and cell-offset-y = cell-padding
                   when (cell-start-row-p cell row)
                   do (pdf:with-saved-state
	                ;; Set translation while counting both table border and padding,
                        ;; but neither cell-border nor cell-padding.
                        (pdf:translate cell-x row-y)
		        ;; First, fill background
                        (pdf:with-saved-state
                          (let ((background-color (or (background-color cell)
                                                      (background-color row))))
                            (when background-color
                              (pdf:set-color-fill background-color)
                              (pdf:basic-rect (- half-border) half-border ;0 0
                                              (+ width full-size-offset)
                                              (- (+ height full-size-offset)))
                              (pdf:fill-path)))
 		          ;; Next, draw table border if not suppressed
			  (unless (or (zerop border) (null cell-border))
                            (pdf:set-line-width border)
                            (pdf:set-gray-stroke 0)
                            (pdf:basic-rect (- half-border) half-border ;0 0
                                            (+ width full-size-offset)
                                            (- (+ height full-size-offset)))
                            (pdf:stroke))
		          ;; Last, draw additional cell border
                          (unless (symbolp cell-border)
                            (pdf:set-gray-stroke 0)
                            (with-quad (left-border top-border right-border bottom-border)
                                cell-border
                              (if (or (numberp cell-border)
                                      (= left-border top-border right-border bottom-border))
                                  (unless (zerop left-border)
                                    (pdf:set-line-width left-border)
                                    (let ((half-border (/ left-border 2)))
                                      (pdf:basic-rect half-border (- half-border)
                                            (- (+ width full-size-offset) left-border)
                                            (- left-border height full-size-offset))
                                      (incf cell-offset-x left-border)
                                      (incf cell-offset-y left-border)))
                                  (let* ((left (/ left-border 2))
                                         (top (- (/ top-border 2)))
                                         (right (- (+ width full-size-offset)
                                                   left (/ right-border 2)))
                                         (bottom (- (/ bottom-border 2) top
                                                    height full-size-offset)))
                                    (multi-line `((,left  . ,bottom)
                                                  (,left  . ,top)
                                                  (,right . ,top)
                                                  (,right . ,bottom)
                                                  (,left  . ,bottom))
                                                `(,left-border ,top-border
                                                  ,right-border ,bottom-border))
                                    (incf cell-offset-x left-border)
                                    (incf cell-offset-y top-border))))
                            (pdf:stroke)))
                        (stroke cell cell-offset-x (- cell-offset-y)))))))

(defmethod stroke ((table split-table) x y)
  (stroke-table (original-table table) x y (rows table) (dy table)))

(defmethod stroke ((table table) x y)
  (stroke-table table x y (rows table) (dy table)))

(defmethod stroke ((cell table-cell) x y)
  (stroke (box cell) x y))

;;; Convenience macros 

(defmacro table ((&key col-widths
                       (padding 5) (cell-padding 2)
                       (border 1) (border-color #x00000) 
		       background-color
                       splittable-p)
		 &body body)
  `(let* ((*table* (make-instance 'table :splittable-p ,splittable-p
				  :col-widths ,col-widths
				  :padding ,padding :cell-padding ,cell-padding
				  :background-color ,background-color
				  :border ,border :border-color ,border-color)))
    (add-box *table*)
    ,@body
    (compute-table-size *table*)
    *table*))

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
