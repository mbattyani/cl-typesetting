;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package typeset)

(defparameter *boxes* nil) ;for debugging...

(defun draw-block (content x y dx dy rotation &key (v-align :top) special-fn)
  (pdf:with-saved-state
    (pdf:translate x y)
    (pdf:rotate rotation)
    (pdf:set-gray-fill 1)
    (pdf:basic-rect -5 0 (+ dx 10) (- dy))
    (pdf:fill-and-stroke)
    (pdf:set-gray-fill 0)
    (let ((box (make-filled-vbox content dx dy v-align)))
      (push box *boxes*)
      (when special-fn
	(funcall special-fn box 0 0))
      (stroke box 0 0))))

; a very simple hello world
(defun hello (&optional (file #P"/tmp/hello.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(pdf:set-line-width 0.1)
	(let ((content
	       (compile-text ()
			     (vspace 100)
		 (paragraph (:h-align :centered :font "Helvetica-Bold" :font-size 50 :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 26)
			      "The cool Common Lisp typesetting system")
			    (vspace 50)
			    (with-style (:font "Helvetica-Oblique" :font-size 100)
			      "Hello World!")))))
	  (draw-block content 20 800 545 700 0))))
    (pdf:write-document file)))

; a multipage simple hello world
(defun muli-page-hello (&optional (file #P"/tmp/hello.pdf"))
  (pdf:with-document ()
    (let ((content
	   (compile-text ()
		(vspace 100)
		(paragraph (:h-align :centered :font "Helvetica-Bold" :font-size 50 :color '(0.0 0 0.8))
			   "cl-typesetting" :eol
			   (vspace 2)
			   (hrule :dy 1)
			   (with-style (:font "Times-Italic" :font-size 26)
			     "The cool Common Lisp typesetting system")
			   (vspace 50)
			     (dotimes (i 100)
			       (with-style (:font "Helvetica-Oblique" :font-size 36 :color '(0.0 0 0.8))
				 "Hello World!" :eol))))))
      (loop while (boxes content) do
	    (pdf:with-page ()
	      (pdf:set-line-width 0.1)
	      (draw-block content 20 800 545 700 0))))
    (pdf:write-document file)))

;; The Fancy Example!

(defparameter *par1*
  "Lisp is a family of languages with a long history. Early key ideas in Lisp were developed by John McCarthy during the 1956 Dartmouth Summer Research Project on Artificial Intelligence. McCarthy's motivation was to develop an algebraic list processing language for artificial intelligence work. Implementation efforts for early dialects of Lisp were undertaken on the IBM 704, the IBM 7090, the Digital Equipment Corporation (DEC) PDP-1, the DEC PDP-6, and the PDP-10. The primary dialect of Lisp between 1960 and 1965 was Lisp 1.5. By the early 1970's there were two predominant dialects of Lisp, both arising from these early efforts: MacLisp and Interlisp. For further information about very early Lisp dialects, see The Anatomy of Lisp or Lisp 1.5 Programmer's Manual.")

(defparameter *par2*
  "MacLisp improved on the Lisp 1.5 notion of special variables and error handling. MacLisp also introduced the concept of functions that could take a variable number of arguments, macros, arrays, non-local dynamic exits, fast arithmetic, the first good Lisp compiler, and an emphasis on execution speed. By the end of the 1970's, MacLisp was in use at over 50 sites. For further information about Maclisp, see Maclisp Reference Manual, Revision 0 or The Revised Maclisp Manual.")

;; example of extension

(defclass rotated-char-box (soft-box h-mode-mixin)
  ((boxed-char :accessor boxed-char :initarg :boxed-char)
   (rotation :accessor rotation :initarg :rotation)))

(defun put-rotated-char-string (string)
  (loop for char across string
	do (add-box (make-instance 'rotated-char-box :dx *font-size*
				   :dy *font-size* :boxed-char char :baseline (* *font-size* 0.8)
				   :rotation (- (random 120) 60)))))

(defmethod stroke ((box rotated-char-box) x y)
  (let ((dx (dx box))(dy (dy box))
	(width (pdf:get-char-width (boxed-char box) *font* *font-size*)))
    (pdf:with-saved-state
      (pdf:translate (+ x (* dx 0.5)) (+ y (* dy 0.3)))
      (pdf:set-line-width 0.5)
      (pdf:set-gray-fill 0.8)
      (pdf:circle 0 0 (* dx 0.45))
      (pdf:fill-and-stroke)
      (pdf:set-gray-fill 0)
      (pdf:rotate (rotation box))
      (pdf:in-text-mode
       (pdf:move-text (* -0.5 width)(* -0.18 *font-size*))
       (pdf:set-font *font* (* *font-size* 0.8))
       (pdf:show-text (make-string 1 :initial-element (boxed-char box)))))))

;; a draw function for the functional rule...

(defun draw-wavelet-rule (box x0 y0)
  (let ((dx/2 (* (dx box) 0.5))
	(dy/2 (* (dy box) 0.5)))
    (pdf:with-saved-state
      (pdf:translate (+ x0 dx/2) (- y0 dy/2))
      (pdf:set-line-width 1)
      (pdf:set-color-stroke (color box))
      (pdf:move-to (- dx/2) 0)
      (loop for x from (- dx/2) by 0.2
	    for y = (* dy/2 (cos (* x 0.8)) (exp (* x x -0.006)))
	    while (< x dx/2)
	    do (pdf:line-to x y))
      (pdf:stroke))))

;; user-drawn box

(defun user-drawn-demo (box x y)
  (draw-block (compile-text ()
			    (paragraph (:h-align :justified :top-margin 5 :first-line-indent 10
						 :font "Times-Italic" :font-size 6.5)
				       *par1*))
	      x (- y (dy box)) (- (dy box) 10) (dx box) 90))

;; a chart (I will have to change this in cl-pdf: it's a real mess!)

(defun draw-pie (box x y)
  (pdf:draw-object (make-instance
		    'pdf:pie-chart :x (+ x 30) :y (- y 100) :width 90 :height 90
		    :serie '(12 23 65 33)
		    :labels&colors
		    '(("Winter" (1.0 0.0 0.0))
		      ("Spring" (0.0 1.0 0.0))
		      ("Summer" (0.0 0.0 1.0))
		      ("Autumn" (0.0 1.0 1.0))))))

;; a stupid trick
;; brute force!!!
(defun link-all-a (box x y)
  (pdf:with-saved-state
      (let ((all-a ()))
	(map-boxes box 0 0 #'(lambda (box x y)
			       (when (and (char-box-p box) (char= (boxed-char box)#\a))
				 (push (list (+ x (* 0.5 (dx box)))(+ y (* 0.2 (dy box))) box) all-a))))
	(pdf:set-line-width 1)
	(pdf:set-rgb-stroke 1.0 0.8 0.4)
	(loop for (x y box) in all-a
	      for sorted-a = (sort (copy-seq all-a)
				   #'(lambda (item1 item2)
				       (let ((dx1 (- (first item1) x))
					     (dy1 (- (second item1) y))
					     (dx2 (- (first item2) x))
					     (dy2 (- (second item2) y)))
					 (<= (sqrt (+ (* dx1 dx1)(* dy1 dy1)))(sqrt (+ (* dx2 dx2)(* dy2 dy2)))))))
	      do (loop repeat 4
		   for (x2 y2 box) in sorted-a
		   do
	         (pdf:set-gray-fill 0.8)
		   (pdf::move-to x y)
		   (pdf::line-to x2 y2)
		   (pdf::stroke)))
	(pdf:set-gray-fill 0.8)
	(pdf:set-rgb-stroke 0.5 0.7 1.0)
	(loop for (x y box) in all-a
	      do
	      (pdf:circle x y (* (dx box) 0.7))
	      (pdf:fill-and-stroke)))))

;;; rivers detection (still brute force not to be used in real life...)
(defun link-all-spaces (box x y)
  (pdf:with-saved-state
      (let ((all-spaces ()))
	(map-boxes box 0 0 #'(lambda (box x y)
			       (when (white-char-box-p box)
				 (push (list (+ x (* 0.5 (dx box)))(+ y (* 0.5 (dx box))) box) all-spaces))))
	(pdf:set-line-width 1)
	(pdf:set-rgb-stroke 1.0 0.4 0)
	(loop for (x y box) in all-spaces
	      for sorted-spaces = (sort (copy-seq all-spaces)
					#'(lambda (item1 item2)
					    (let ((dx1 (- (first item1) x))
						  (dx2 (- (first item2) x)))
					      (<= (abs dx1)(abs dx2)))))
	      do (loop repeat 5
		   for (x2 y2 box) in sorted-spaces
		   for dy = (abs (- y2 y))
		   for dx = (abs (- x2 x))
		   do
		   (when (and (< dx (* 1.5 (+ (dx box)(delta-size box)))) (< 0 dy (* 5 (dx box))))
		     (pdf:set-gray-fill 0.8)
		     (pdf::move-to x y)
		     (pdf::line-to x2 y2)
		     (pdf::stroke)
		     (pdf:circle x y (* (dx box) 0.7))
		     (pdf:circle x2 y2 (* (dx box) 0.7))
		     (pdf:fill-and-stroke)))))))

(defun link-all-a-and-spaces (box x y)
  (link-all-a box x y)
  (link-all-spaces box x y))
  
;;example document

(defun ex (&optional (file #P"/tmp/ex.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(pdf:set-line-width 0.1)
	(let ((content
	       (compile-text ()
		 (paragraph (:h-align :centered :font "Helvetica-Bold" :font-size 30 :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 13)
			      "The cool Common Lisp typesetting system"))
		 (paragraph (:h-align :justified :top-margin 10 :first-line-indent 10
				      :font "Times-Italic" :font-size 10)
			      "This typesetting system's goal is to be an alternative to the TeX typesetting system. It is written in Common Lisp and uses cl-pdf as its backend. This will enable it to be powerful, extensible and fast. Though it is not considered very difficult, it is already better than Word...")
		 (paragraph (:h-align :centered :font "Helvetica-BoldOblique" :font-size 20 :color '(1.0 0 0))
			    "Now in Color!")
		 (paragraph (:h-align :centered :font "Times-Italic" :font-size 12 :color '(0.0 0.6 0.3))
			    "With user defined "
			    (put-rotated-char-string "extensions") :eol
			    (with-style (:font "Times-Italic" :font-size 11)
			      "Support for images and functional rules" :eol
			      (image :file #P"/tmp/banner.jpg" :dx 100 :dy 20)))
		 (hrule :dy 15 :stroke-fn 'draw-wavelet-rule)
		 (vspace 3)
		 (table (:col-widths '(60 80 80) :border 0.5 :background-color '(1 1 0.8)
				     :cell-padding 1 :padding 2)
			(row ()
			     (cell (:background-color '(0.8 1 0.8) :col-span 3)
				   (paragraph (:h-align :centered :font "Times-Italic" :font-size 12)
						"Title with a col-span of 3")))
			(row ()
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"Left aligned"))
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :centered :font "Times-Roman" :font-size 9)
						"Centered cell content"))
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :right :font "Times-Bold" :font-size 9)
						"Right cell content")))
			(row ()
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"This cell content should take three lines."))
			     (cell (:background-color '(1 1 1))
				   (paragraph (:h-align :centered :font "Times-Italic" :font-size 9)
						"A jpeg "
						(image :file #P"/tmp/fractal.jpg" :dx 15 :dy 15 :inline t
						       :offset 9)
						" in the text"))
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 11)
						(put-rotated-char-string "common lisp is cool"))))
			(row ()
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"An example of table inside a cell"))
			     (cell (:background-color '(1 1 1))
				   (table (:col-widths '(14 14 21) :border 0.2
							 :background-color '(0.4 0.4 0.8))
					    (row () (cell () "12")(cell () "34")(cell () "567"))
					    (row () (cell () "ab")(cell () "cd")(cell () "efg"))))
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"You can nest as many tables as you want, like you do in HTML."))))
		 (paragraph (:h-align :justified :top-margin 5 :first-line-indent 10 :color '(0 0 0)
				      :font "Times-Roman" :font-size 10 :text-x-scale 0.7)
			    (with-style (:color '(0 0.6 0.4))
			      "This paragraph has been horizontally strechted by a 0.7 ratio. ")
			    *par1*)
		 (user-drawn-box :dx 210 :dy 100 :stroke-fn 'user-drawn-demo) :eol
		 (paragraph (:h-align :justified :top-margin 9 :first-line-indent 10
				      :left-margin 20 :right-margin 20 :font "Helvetica" :font-size 9)
			    *par2*)
		 (paragraph (:h-align :justified :top-margin 9 :font "Helvetica-Oblique"
				      :font-size 9 :first-line-indent 20)
			   *par1*)
		 (user-drawn-box :dx 240 :dy 100 :stroke-fn 'draw-pie) :eol
		 (paragraph (:h-align :centered :font "Times-Italic" :font-size 8)
			    "An example of cl-pdf pie chart inserted.")
		 (paragraph (:h-align :justified :top-margin 9 :font "Helvetica-Oblique" :font-size 9
				      :left-margin 40 :right-margin 40)
			    *par2*)
		 (paragraph (:h-align :centered :top-margin 20 :font "Times-Bold" :font-size 20)
			    "Kerning test" :eol
			    (with-style (:font "Helvetica" :font-size 40 :left-margin 20 :right-margin 20)
			      "Yes, AWAY"))
		 (paragraph (:h-align :centered :top-margin 10 :font "CMTI10"
				      :font-size 16 :color '(0 0 0))
			    (with-style (:font "Times-Bold" :font-size 20)
			      "Basic Math Mode Test" :eol)
			    (vspace 5)
			    (display-formula ()
			      (with-style (:font (pdf:get-font "CMEX10" nil) :font-size 30)
				(with-offset (23) "H"))
			     "E"(math-super-and-sub-script () ("n+1") ("k,m"))"="
			     (fraction ()
				       ("x"(with-superscript () "2")"+x-1")
				       ("F(x)+b-3"))
			     "-e"(with-superscript () "-x"(with-superscript () "2")))
			    (vspace 5)
			    (with-style (:font "Times-Roman" :font-size 10)
			      "This test now uses a TeX font (cmti10). Note the italic" :eol "correction for the super/subscript of the E."))
		 (paragraph (:h-align :centered :top-margin 20 :font "Helvetica"
				      :font-size 40 :color '(0.8 0 0))
			    "Warning!" :eol
			    (with-style (:font "Times-Italic" :font-size 14)
			      "This test pdf file has been made with" :eol "cl-typesetting 0.6." :eol
			      (vspace 10)
			      "Marc Battyani"))
		 :vfill
		 (hrule :dy 20 :stroke-fn 'draw-wavelet-rule :color '(0.8 0 0))
		 :vfill
		 (paragraph (:h-align :centered :font "Helvetica-Oblique" :font-size 8)
			    "This project needs contributors. So if you are interested contact "
			    (with-style (:font "Times-Italic" :font-size 9)
			      "marc.battyani@fractalconcept.com") "."
			    ))))
	  (pdf::draw-bar-code128 "CODE128BARCODE" 10 35 :height 25 :width 150 :start-stop-factor 0.25
				 :font-size 7 :show-string t)
	  (draw-block content 40 800 250 380 5)
	  (draw-block content 50 425 250 380 -5)
	  (draw-block content 330 800 250 380 -2 :special-fn 'link-all-a-and-spaces)
	  (draw-block content 310 400 250 380 0 :v-align :justified))))
    (pdf:write-document file)))

