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
          and cell-border = (border cell)
          for cell-borders-width 
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
	  ;; Adjust cell width for cells spanning multiple columns
          unless (zerop col-span)
            do (incf width (+ (* col-span full-size-offset)
                              (reduce #'+ next-widths :end col-span)))
               (setf next-widths (nthcdr col-span next-widths))

          ;; Fill cell with content if required
          when (cell-start-row-p cell row)
            do (setf (box cell) (make-filled-vbox (content cell)
                                                  (- width cell-borders-width)
                                                  height (v-align cell))
                     (width cell) width)
	    
          ;; A cell spanning several rows participates only in height calculation 
          ;; of the last row
          if (and (numberp row-span) (> row-span 1))
          do (span-cell rows cell col-number)
          else unless (height row)
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

          finally (setf height (+ (max (or (height row) 0.0) max-height) +epsilon+)))
    (setf (height row) height)
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
                          (unless (or (zerop border) (null cell-border)) ; next, draw table border
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
                        (stroke (box cell) cell-offset-x (- cell-offset-y)))))))

(defmethod stroke ((table split-table) x y)
  (stroke-table (original-table table) x y (rows table) (dy table)))

(defmethod stroke ((table table) x y)
  (stroke-table table x y (rows table) (dy table)))

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

#|
(defun test-table (&optional (file (lw:current-pathname "test-table.pdf"))
                   &aux content table (margins '(72 72 72 50)))
  (let* ((tt:*default-font* (pdf:get-font "Helvetica"))
         (tt:*default-font-size* 10))
   (with-document ()
    (setq content (compile-text (:font tt:*default-font* :font-size tt:*default-font-size*)
     (tt:paragraph () "Test table spans and borders")
           ;; Various spans
     (tt:table (:col-widths '(20 40 60 80 120) :background-color :yellow :border 1)
       (tt:header-row ()
         (tt:cell (:col-span 5)
           (tt:paragraph (:h-align :center :font-size 12)
             "Table with cells spanning more then one row or column")))
       (tt:row (:background-color :green)
         (tt:cell (:row-span 2 :background-color :blue)
           "1,1 2,1  row-span 2")
         (tt:cell () "1,2")
         (tt:cell (:col-span 2 :row-span 3 :background-color :red)
           "1,3 1,4 - 3,3 3,4  col-span 2 row-span 3")
         (tt:cell () "1,5"))
       (tt::row ()
         (tt:cell () "2,2")
         (tt:cell (:row-span 2 :background-color :blue) "2,5 3,5  row-span 2"))
       (tt:row (:background-color :green)
         (tt:cell (:col-span 2) "3,1 3,2  col-span 2"))
       (tt::row ()
         (tt:cell () "4,1")
         (tt:cell () "4,2")
         (tt:cell () "4,3")
         (tt:cell () "4,4")
         (tt:cell () "4,5")))
    (setq table
     (tt:table (:col-widths '(50 40 60 80 120) :border 0)
       (tt:header-row ()
         (tt:cell (:col-span 5 :border 1)
           (tt:paragraph (:h-align :centered :font-size 12)
             "Cells with borders")))
       (tt:row (:background-color :green)
         (tt:cell (:row-span 2 :background-color :blue)
           "1,1 2,1  row-span 2")
         (tt:cell () "1,2")
         (tt:cell (:col-span 2 :row-span 3 :background-color :red)
           "1,3 1,4 - 3,3 3,4  col-span 2 row-span 3")
         (tt:cell () "1,5"))
       (tt:row ()
         (tt:cell (:border 2) "2,2")
         (tt:cell (:row-span 2 :background-color :blue) "2,5 3,5  row-span 2"))
       (tt:row (:background-color :green)
         (tt:cell (:col-span 2 :border 2) "3,1 3,2  col-span 2"))
       (tt:row ()
         (tt:cell (:border #(3 0 0 0)) "4,1 left-border 3")
         (tt:cell (:border #(0 3 0 0)) "4,2 top-border 3")
         (tt:cell (:border #(2 2 2 2)) "4,3 border #(2 2 2 2)")
         (tt:cell (:border #(0 0 3 0)) "4,4 right-border 3")
         (tt:cell (:border #(0 0 0 3)) "4,5 bottom-border 3"))
       (tt:row ()
         (tt:cell (:col-span 5 :border '(0 1/4))
           (tt:paragraph (:h-align :justified :font-size 12)
             "bottom" :hfill "cell" :hfill "spanning" :hfill "several" :hfill "rows"))))
    )))
    (draw-pages content :margins margins :break :after)
    (pdf:write-document file)))
 table)

(pdf:load-fonts)
(setq table (test-table (lw:current-pathname "examples/test-table.pdf")))
 |#
