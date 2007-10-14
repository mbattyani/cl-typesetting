;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

;;; Low level tests

(defparameter *boxes* nil) ;for debugging...

; a very simple hello world
(defun hello (&optional (file #P"/tmp/hello.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(pdf:set-line-width 0.1)
	(let ((content
	       (compile-text ()
			     (vspace 100)
		 (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 50 :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 26)
			      "The cool Common Lisp typesetting system")
			    (vspace 50)
			    (with-style (:font "Helvetica-Oblique" :font-size 100)
			      "Hello World!")
			    (vspace 50)
			    (with-style (:font "Helvetica" :font-size 12)
			      "hello" (dotted-hfill) "4.2" :eol
			      "hello world" (dotted-hfill) "4.2.4.2"))
		 (paragraph (:h-align :justified :font "Helvetica" :font-size 12 :color '(0.0 0 0.0))
			    "hello" (dotted-hfill) "4.2" :eol
			    "hello world" (dotted-hfill) "4.2.4.2")
			    (vspace 50)
		 (paragraph (:h-align :justified :font "Helvetica" :font-size 12 :color '(0.0 0 0.0))
			    "hello" (dotted-hfill :pattern-spacing 0.6) "4.2" :eol
			    "hello world" (dotted-hfill :pattern-spacing 0.6) "4.2.4.2")
			    (vspace 50)
		 (paragraph (:h-align :justified :font "Helvetica" :font-size 12 :color '(0.0 0 0.0))
			    "hello" (dotted-hfill :char-pattern ".+" :pattern-spacing 0) "4.2" :eol
			    "hello world" (dotted-hfill :char-pattern ".+" :pattern-spacing 0) "4.2.4.2"))))
	  (draw-block content 20 800 545 700))))
    (pdf:write-document file)))

; a multipage simple hello world
(defun multi-page-hello (&optional (file #P"/tmp/hello.pdf"))
  (pdf:with-document ()
    (let ((content
	   (compile-text ()
		(vspace 100)
		(paragraph (:h-align :center :font "Helvetica-Bold" :font-size 50 :color '(0.0 0 0.8))
			   "cl-typesetting" :eol
			   (vspace 2)
			   (hrule :dy 1)
			   (with-style (:font "Times-Italic" :font-size 26)
			     "The cool Common Lisp typesetting system")
			   (vspace 50)
			   (with-style (:font "Times-Italic" :font-size 36 :color '(0.0 0 0.8))
			     (dotimes (i 100)
			       (put-string "Hello World!")(new-line)))))))
      (loop while (boxes content) do
	    (pdf:with-page ()
	      (pdf:set-line-width 0.1)
	      (draw-block content 20 800 545 700))))
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
      (loop with a = (/ -0.2 (dx box)) and w = (/ 200.0 (dx box))
	    for x from (- dx/2) by 0.2
	    for y = (* dy/2 (cos (* x w)) (exp (* x x a)))
	    while (< x dx/2)
	    do (pdf:line-to x y))
      (pdf:stroke))))

;; user-drawn box

(defun user-drawn-demo (box x y)
  (draw-block (compile-text ()
			    (paragraph (:h-align :justified :top-margin 5 :first-line-indent 10
						 :font "Times-Italic" :font-size 6.5)
				       *par1*))
	      x (- y (dy box)) (- (dy box) 10) (dx box) :rotation 90 :border 0.1))

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

(defun gen-box-graph ()
  (let* ((g1 (make-instance 'graph :dot-attributes '(#+nil("rankdir" "LR")("nodesep" "0.3")("ranksep" "0.8"))
			    :max-dx 500 :max-dy 300 :border-width nil))
	 (n1 (make-instance 'graph-node :data "box" :graph g1))
	 (n2 (make-instance 'graph-node :data "h-mode-mixin" :graph g1))
	 (n3 (make-instance 'graph-node :data "v-mode-mixin" :graph g1))
	 (n5 (make-instance 'graph-node :data "soft-box" :graph g1))
	 (n6 (make-instance 'graph-node :data "container-box" :graph g1))
	 (n7 (make-instance 'graph-node :data "vbox" :graph g1))
	 (n8 (make-instance 'graph-node :data "hbox" :graph g1))
	 (n9 (make-instance 'graph-node :data "glue" :graph g1))
	 (n10 (make-instance 'graph-node :data "hglue" :graph g1))
	 (n11 (make-instance 'graph-node :data "vglue" :graph g1))
	 (n12 (make-instance 'graph-node :data "spacing" :graph g1))
	 (n13 (make-instance 'graph-node :data "h-spacing" :graph g1))
	 (n14 (make-instance 'graph-node :data "v-spacing" :graph g1))
	 (n15 (make-instance 'graph-node :data "char-box" :graph g1))
	 (n16 (make-instance 'graph-node :data "white-char-box" :graph g1)))
    (add-rank-constraint g1 "same" (list n1 n5 n15))
    (make-instance 'graph-edge :head n1 :tail n5 :label "subclass" :graph g1)
    (make-instance 'graph-edge :head n5 :tail n6 :graph g1)
    (make-instance 'graph-edge :head n6 :tail n7 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n7 :graph g1)
    (make-instance 'graph-edge :head n6 :tail n8 :graph g1)
    (make-instance 'graph-edge :head n3 :tail n8 :graph g1)
    (make-instance 'graph-edge :head n5 :tail n9 :graph g1)
    (make-instance 'graph-edge :head n9 :tail n10 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n10 :graph g1)
    (make-instance 'graph-edge :head n9 :tail n11 :graph g1)
    (make-instance 'graph-edge :head n3 :tail n11 :graph g1)
    (make-instance 'graph-edge :head n5 :tail n12 :graph g1)
    (make-instance 'graph-edge :head n12 :tail n13 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n13 :graph g1)
    (make-instance 'graph-edge :head n12 :tail n14 :graph g1)
    (make-instance 'graph-edge :head n3 :tail n14 :graph g1)
    (make-instance 'graph-edge :head n1 :tail n15 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n15 :graph g1)
    (make-instance 'graph-edge :head n10 :tail n16 :graph g1)
    (compute-graph-layout g1)
    g1))

(defun make-color-node-box (graph name color description)
  (make-instance 'graph-node :graph graph :dx 70 :data
		 (make-filled-vbox
		  (compile-text ()
 		     (paragraph (:h-align :center :font "Helvetica-Oblique"
					  :font-size 14 :color '(0 0 0))
				(put-string name) " "
				(colored-box :dx 9.0 :dy 9.0 :color color :border-width 0.5)
				:eol
				(with-style (:font "Times-Italic" :font-size 10)
				  (put-string description))))
		  70 +huge-number+)))

(defun gen-color-graph ()
  (let* ((g1 (make-instance 'graph :dot-attributes '(#+nil("rankdir" "LR")("nodesep" "0.3")("ranksep" "0.8"))
			    :max-dx 500 :max-dy 300 :border-width nil))
	 (red (make-color-node-box g1 "red" '(1.0 0 0) "(primary color)"))
	 (green (make-color-node-box g1 "green" '(0 1.0 0) "(primary color)"))
	 (blue (make-color-node-box g1 "blue" '(0 0 1.0) "(primary color)"))
	 (cyan (make-color-node-box g1 "cyan" '(0 1.0 1.0) "(green + blue)"))
	 (magenta (make-color-node-box g1 "magenta" '(1.0 0 1.0) "(red + blue)"))
	 (yellow (make-color-node-box g1 "yellow" '(1.0 1.0 0) "(red + green)")))
    (make-instance 'graph-edge :head blue :tail cyan :graph g1)
    (make-instance 'graph-edge :head green :tail cyan :graph g1)
    (make-instance 'graph-edge :head red :tail magenta :graph g1)
    (make-instance 'graph-edge :head blue :tail magenta :graph g1)
    (make-instance 'graph-edge :head red :tail yellow :graph g1)
    (make-instance 'graph-edge :head green :tail yellow :graph g1)
    (compute-graph-layout g1)
    g1))

;;; Example document
; Copy the files from the directory "files-for-example/" (included in
; the cl-typesetting distribution) to the /tmp directory (or somewhere else).
;
; Then you need to load the fonts with something like this:
;   (pdf:load-t1-font "/tmp/cmex10.afm" "/tmp/cmex10.pfb")
;   (pdf:load-t1-font "/tmp/cmti10.afm" "/tmp/cmti10.pfb")

(defun full-example (&key (file #P"/tmp/ex.pdf")
                     (banner-jpg #P"/tmp/banner.jpg")
                     (fractal-jpg #P"/tmp/fractal.jpg")
                     display-graphs)
  (with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Table of Content" (pdf:register-page-reference))
	(let ((content
	       (compile-text ()
			     (vspace 100)
			     (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 60 :color '(0.0 0 0.8))
					"cl-typesetting" :eol
					(vspace 2)
					(hrule :dy 1)
					(with-style (:font "Times-Italic" :font-size 28)
					  "The Cool Common Lisp Typesetting System"))
			     (vspace 100)
			     (hrule :dy 30 :stroke-fn 'draw-wavelet-rule :color '(0.0 0 0.6))
			     (vspace 250)
			     (hrule :dy 0.5 :color '(0.0 0 0.6))
			     (paragraph (:h-align :center :font "Helvetica" :font-size 16 :color '(0.0 0 0.6))
					"Table of content")
			     (hrule :dy 0.1 :color '(0.0 0 0.6))
			     (paragraph (:h-align :fill :font "Helvetica" :font-size 14 :color '(0.0 0 0.4)
						  :left-margin 25 :right-margin 25)
					"Hello world" (dotted-hfill)
					(format-string "~d" (find-ref-point-page-number "hello")) :eol
					"Full demo" (dotted-hfill)
					(format-string "~d" (find-ref-point-page-number "demo")) :eol
					"cl-typegraph" (dotted-hfill)
					(format-string "~d" (find-ref-point-page-number "graph")) :eol)
			     (vspace 2)
			     (hrule :dy 0.5 :color '(0.0 0 0.6)))))
	  (draw-block content 20 800 545 700))))
    (pdf:with-page ()
      (pdf:with-outline-level ("Hello world" (pdf:register-page-reference))
	(let ((content
	       (compile-text ()
	          (mark-ref-point "hello")
		  (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 30 :color '(0.0 0 0.8))
			     "cl-typesetting" :eol
			     (vspace 2)
			     (hrule :dy 1)
			     (with-style (:font "Times-Italic" :font-size 24)
			       "The Cool Common Lisp Typesetting System")
;			     (vspace 50)
;			     (with-style (:font "Helvetica-Oblique" :font-size 100)
;			       "Hello World!")
			     :vfill
			     (with-style (:font "Times-Italic" :font-size 100)
			       "Hello World!")
			     :vfill))))
	  (draw-block content 20 800 545 700))))
   (pdf:with-page ()
      (pdf:with-outline-level ("Full demo" (pdf:register-page-reference))
	(let ((content
	       (compile-text ()
	         (mark-ref-point "demo")
		 (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 30 :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 13)
			      "The Cool Common Lisp Typesetting System"))
		 (paragraph (:h-align :justified :top-margin 10 :first-line-indent 10
				      :font "Times-Italic" :font-size 9)
			      "This typesetting system's goal is to be an alternative to the TeX typesetting system. It is written in Common Lisp and uses cl-pdf as its backend. This enables it to be powerful, extensible, programmable  and fast. Though it is not considered very difficult, it is already much better than Word...")
		 (paragraph (:h-align :center :font "Helvetica-BoldOblique" :font-size 20 :color '(1.0 0 0))
			    "Now in Color! "
			    (colored-box :dx 15.0 :dy 15.0 :color "#FFC0C0" :border-width 0.5) " "
			    (colored-box :dx 15.0 :dy 15.0 :color "#C0FFC0" :border-width 0.5) " "
			    (colored-box :dx 15.0 :dy 15.0 :color "#C0C0FF" :border-width 0.5))
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 12 :color '(0.0 0.6 0.3))
			    "With user defined "
			    (put-rotated-char-string "extensions") :eol
			    (with-style (:font "Times-Italic" :font-size 11)
			      "Support for images and functional rules" :eol
			      (image :file banner-jpg :dx 100 :dy 20)))
		 (hrule :dy 15 :stroke-fn 'draw-wavelet-rule)
		 (vspace 3)
		 (table (:col-widths '(60 80 80) :border 0.5 :background-color '(1 1 0.8)
				     :cell-padding 1 :padding 2)
			(row ()
			     (cell (:background-color '(0.8 1 0.8) :col-span 3)
				   (paragraph (:h-align :center :font "Times-Italic" :font-size 12)
						"Title with a col-span of 3")))
			(row ()
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"Left aligned"))
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :center :font "Times-Roman" :font-size 9)
						"Centered cell content"))
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :right :font "Times-Bold" :font-size 9)
						"Right cell content")))
			(row ()
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"This cell content should take three lines."))
			     (cell (:background-color '(1 1 1))
				   (paragraph (:h-align :center :font "Times-Italic" :font-size 9)
						"A jpeg "
						(image :file fractal-jpg :dx 15 :dy 15 :inline t
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
			     (cell (:v-align :bottom)(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"You can nest as many tables as you want, like you do in HTML."))))
		 (paragraph (:h-align :justified :top-margin 5 :first-line-indent 10 :color '(0 0 0)
				      :left-margin 5 :right-margin 5 
				      :font "Times-Roman" :font-size 10 :text-x-scale 0.7)
			    (with-style (:color '(0 0.6 0.4))
			      "This paragraph has been horizontally strechted by a 0.7 ratio. ")
			    *par1*)
		 (vspace 10)
		 (user-drawn-box :dx 210 :dy 100 :stroke-fn 'user-drawn-demo) :eol
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 8 :top-margin 5)
			    "An example of using cl-typesetting in an user-drawn box.")
		 (paragraph (:h-align :left :top-margin 15
				      :left-margin 5 :right-margin 5 :font "courier" :font-size 8)
			    (verbatim
"(defmethod stroke ((box char-box) x y)
  (pdf:in-text-mode
   (pdf:move-text x (+ y (offset box)))
   (pdf:set-font *font* *font-size*)
   (pdf:set-text-x-scale (* *text-x-scale* 100))
   (pdf:show-char (boxed-char box))))"))
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 8 :top-margin 3)
			    "An example of verbatim code.")
		 (paragraph (:h-align :justified :top-margin 9 :font "Helvetica-Oblique"
				      :left-margin 5 :right-margin 5 
				      :font-size 9 :first-line-indent 20)
			   *par1*)
		 (user-drawn-box :dx 240 :dy 100 :stroke-fn 'draw-pie) :eol
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 8)
			    "An example of cl-pdf pie chart inserted.")
		 (paragraph (:h-align :justified :top-margin 9 :font "helvetica" :font-size 9
				      :left-margin 40 :right-margin 40)
			    *par2*)
		 (vspace 10)
		 (paragraph (:h-align :center :top-margin 20 :font "Times-Bold" :font-size 20)
			    "Kerning test" :eol
			    (with-style (:font "Helvetica" :font-size 40 :left-margin 20 :right-margin 20)
			      "Yes, AWAY"))
		 (paragraph (:h-align :center :top-margin 10 :font "CMTI10"
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
		 (paragraph (:h-align :center :top-margin 20 :font "Times-Italic" :font-size 18 :color '(0.8 0 0))
			      "This test pdf file has been typeset " :eol "with cl-typesetting 0.80" :eol
			      (vspace 10)
			      (with-style (:font "Times-Italic" :font-size 14)
				"Marc Battyani"))
		 :vfill
		 (hrule :dy 20 :stroke-fn 'draw-wavelet-rule :color '(0.8 0 0))
		 :vfill
		 (paragraph (:h-align :center :font "Helvetica-Oblique" :font-size 8)
			    "This project needs contributors. So if you are interested contact "
			    (with-style (:font "Times-Italic" :font-size 9)
			      "marc.battyani@fractalconcept.com") "."
			    ))))
	  (pdf::draw-bar-code128 "CODE128BARCODE" 10 35 :height 25 :width 150 :start-stop-factor 0.25
				 :font-size 7 :show-string t)
	  (draw-block content 40 800 250 380 :rotation 5 :border 0.1)
	  (draw-block content 50 425 250 380 :rotation -5 :border 0.1)
	  (draw-block content 330 800 250 380 :rotation -2  :border 0.1 :special-fn 'link-all-a-and-spaces)
	  (draw-block content 310 400 250 380 :v-align :justified :border 0.1))))
    (pdf:with-page ()
      (pdf:with-outline-level ("cl-typegraph" (pdf:register-page-reference))
	(let ((content
	       (compile-text ()
	          (mark-ref-point "graph")
		  (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 30 :color '(0.0 0 0.8))
			     "cl-typegraph" :eol
			     (vspace 2)
			     (hrule :dy 1)
			     (with-style (:font "Times-Italic" :font-size 13)
			       "The Cool Common Lisp Graph Typesetting System"))
		  (vspace 20)
		  (paragraph (:h-align :justified :top-margin 10 :first-line-indent 10
				       :font "helvetica" :font-size 12)
			     "cl-typegraph is a cl-typesetting extension to typeset graphs. It uses GraphViz for the graph layout and then draws it with cl-pdf and cl-typesetting. The nodes can contain strings or a full cl-typesetting layout." :eol
			     (vspace 20)
			     "In the first graph example below, the nodes contain only strings.")
		  (vspace 20)
		  :hfill (if display-graphs 
                             (graph-box (gen-box-graph))
                             (with-style (:color '(0.0 0 0.8)) "display-graphs is nil")) :hfill
		  (paragraph (:h-align :center :font "Times-Italic" :font-size 11)
			     "The class hierarchy for the boxes in cl-typesetting.")
		  (vspace 20)
		  (paragraph (:h-align :justified :font "helvetica" :font-size 12)
			     "In the next graph, each node contains a full cl-typesetting layout. All the cl-typesetting features can be used in a node, even another graph.")
		  (vspace 20)
		  :hfill (if display-graphs 
                             (graph-box (gen-color-graph))
                             (with-style (:color '(0.0 0 0.8)) "display-graphs is nil")) :hfill
		  (paragraph (:h-align :center :font "Times-Italic" :font-size 11)
			     "The primary colors"))))
	  (draw-block content 30 810 530 780))))
    (pdf:write-document file)))

;;; A higher level example

(defun multi-page-example (&optional (file #P"/tmp/multi-page.pdf")
				     &aux content table (margins '(72 72 72 50)))
  (with-document ()
    ;(pdf:set-line-width 0.1)
   (let* ((print-stamp (multiple-value-bind (second minute hour date month year)
                           (get-decoded-time)
                         (format nil "Printed on ~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
                                 year month date hour minute)))
          (header (compile-text ()
                    (paragraph (:h-align :center
                                         :font "Helvetica-BoldOblique" :font-size 12)
                      "Multi-page example document")
                    ;(vspace 1) ;:vfill
                    (hrule :dy 1/2)))
          (footer (lambda (pdf:*page*)
                    (compile-text (:font "Helvetica" :font-size 10)
                      (hrule :dy 1/2)
                      (hbox (:align :center :adjustable-p t)
                        (verbatim print-stamp)
                        ;(hspace 100)
                        :hfill
                        (verbatim
                         (format nil "Page ~d" pdf:*page-number*)))
                 ))))
    (setq content
          (compile-text ()
            (paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
              "1. First paragraph group")
            (hrule :dy 2)
            (dotimes (i 40)
              (paragraph (:font "Helvetica" :font-size (+ 6 (random 10)))
                (verbatim (format nil "1.~d. " (1+ i)))
                (dotimes (j (1+ (random 5)))
                  (put-string "The quick brown fox jumps over the lazy dog. "))))
            :eop))
    (draw-pages content :margins margins :header header :footer footer)

    (setq content
          (compile-text ()
            ;(vbox ()
              (paragraph (:font "Times-Bold" :font-size 16 :top-margin 20)
                "2. Second paragraph group"
              (hrule :dy 2))
            (dotimes (i 40)
              (paragraph (:font "Times-Roman" :font-size (+ 6 (random 10)))
                (verbatim (format nil "2.~d. " (1+ i)))
                (dotimes (j (1+ (random 5)))
                  (put-string "The quick brown fox jumps over the lazy dog. "))))
            
            (table (:col-widths '(20 100 100 100) :splittable-p nil
                            :border 0 :background-color '(1 1 0.8))
              (row ()
                (cell (:col-span 4)
                  (paragraph (:h-align :center
                                      :font "Times-Italic" :font-size 12)
                    "Non-splittable table - row with a col-span of 4")))
              (loop for (name tel age) = (list "Dmitri Dmitriev" "555-1234" (+ 25 (random 25)))
                    and row-number from 1 upto 4
                    do
                    (row (:height (when (evenp row-number) 20)
                                  :background-color (if (zerop (mod row-number 3))
                                                        :red nil))
                      (cell () (verbatim
                                        (format nil "~d" row-number)))
                      (cell (:background-color (when (>= age 40) :blue))
                        name)
                      (cell () tel)
                      (cell () (paragraph (:h-align :right) age)))))
          ))
    (draw-pages content :margins margins :header header :footer footer)

    (setq content
          (compile-text ()
          (table (:col-widths '(20 100 100 100) :splittable-p t
                          :border 1/2 :background-color '(1 1 0.8))
            (header-row (:background-color :gray)
              (cell (:row-span 2) (verbatim "Row #"))
              (cell (:row-span 2) "Name")
              (cell () "Telephone")
                ;(table (:col-widths '(100) :splittable-p nil
                ;                :padding 0 :border 1/2); :background-color '(1 1 0.8))
                ;  (row () (cell () "Telephone"))
                ;  (row () (cell () "Fax"))))
              (cell (:row-span 2) (paragraph (:h-align :right) "Age")))
            (header-row (:background-color :gray)
               (cell () "Fax"))
            (footer-row (:background-color :gray)
              (cell (:col-span 4)
                (paragraph (:h-align :center
                                    :font "Times-Italic" :font-size 12)
                  "Table footer with a col-span of 4")))
            (loop for (name tel age) = (list "Ivan Ivanov" "555-1234" (+ 25 (random 25)))
                  and row-number from 1 upto 40
                  do
                  (row (:height (when (evenp row-number) 20)
                                :background-color (if (zerop (mod row-number 3))
                                                      :red nil))
                    (cell () (verbatim
                                      (format nil "~d" row-number)))
                    (cell (:background-color (when (>= age 40) :blue))
                                   name)
                    (cell () tel)
                    (cell () (paragraph (:h-align :right) age)))))))
    (setq table content)
    (draw-pages content :margins margins :header header :footer footer) ;:break :after
    
    (setq content	;; Various spans
          (compile-text ()
          (table (:col-widths '(20 40 60 80 120)
                          :background-color :yellow :border 1
                          :splittable-p t)
            (header-row ()
              (cell (:col-span 5)
		    (paragraph (:h-align :center :font "Times-Italic" :font-size 12)
                  "Table with cells spanning more then one row")))
            (row (:background-color :green)
              (cell (:row-span 2 :background-color :blue)
                "1,1 2,1  row-span 2")
              (cell () "1,2")
              (cell (:col-span 2 :row-span 3 :background-color :red)
                "1,3 1,4 - 3,3 3,4  col-span 2 row-span 3")
              (cell () "1,5"))
            (row ()
              (cell () "2,2")
              (cell (:row-span 2 :background-color :blue) "2,5 3,5  row-span 2"))
            (row (:background-color :green)
              (cell (:col-span 2) "3,1 3,2  col-span 2"))
            (row ()
              (cell () "4,1")
              (cell () "4,2")
              (cell () "4,3")
              (cell () "4,4")
              (cell () "4,5")))))
    (draw-pages content :margins margins :header header :footer footer) ;:break :after

    (draw-block (compile-text () "Test block - line1" :eol "Line 2")
                                 300 300 150 150 :border 1))
   (when pdf:*page* (finalize-page pdf:*page*))
    ;(pdf:with-page ()
     ;(draw-page content :margins 72))
   (pdf:write-document file))
  table)

#+nil
(defun multi-page-hello (&optional (file #P"/tmp/hello.pdf"))
   (pdf:with-document ()
     (let ((content
   (compile-text ()
     (vspace 100)
     (table (:col-widths '(100 200) :splittable-p t)  ;;; start Erik changes
	    (header-row ()
			(cell ()
                         (paragraph () "Header")))
	    (footer-row ()
			(cell ()
                         (paragraph () "Footer")))
            (dotimes (time 50)
              (row ()
                   (cell ()
                         (paragraph () (put-string (format nil "test ~d" time)))))))  ;;; end Erik changes
     (vspace 10)
     :eol 
     (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 50 
                          :color '(0.0 0 0.8))
                "cl-typesetting" :eol
                (vspace 2)
                (hrule :dy 1)
                (with-style (:font "Times-Italic" :font-size 26)
                  "The cool Common Lisp typesetting system")
                (vspace 50)
                (with-style (:font "Times-Italic" :font-size 36 :color '(0.0 0 
                                                                         0.8))
                  (dotimes (i 100)
                    (put-string "Hello World!")(new-line)))))))
       (loop while (boxes content) do
             (pdf:with-page ()
               (pdf:set-line-width 0.1)
               (draw-block content 20 800 545 700))))
     (pdf:write-document file)))

;;; Unicode test
#+nil
(pdf:load-ttu-font #P"/tmp/times.ufm"
                   #P"/tmp/times.ttf")

(defparameter *unicode-test-string*
  (map unicode-string-type 'code-char
    '(65 110 32 117 110 105 99 111 100 101 32 115 116 114 105 110 103 58 32
      8252 8319 8359 8592 8593 8594 8595 8596 8597 8616 915 920 934 945 948 949 963 964 966 32
      9554 9555 9556 9557 9558 9559 9560 9561 9562 9563 9564 9565 9566 9567 32 65 41 41 66
      9568 9650 9658 9660 9668 9675 9688 9689 8364 1027 8218 402 8222 8230 8224 8225 32 66
      372 373 374 375 383 506 507 508 509 510 511 903 913 914 916 917 918 919 921 922 923 32
      946 947 950 951 952 953 954 955 956 957 958 1101 1102 1103 1105 1106 1107 1108 32
      1475 1488 1489 1490 1491 1492 1493 64304 64305 64306 64307 64308 64309 32
      7911 7912 7913 7914 7915 7916 7917 7918 1179 1180 1181 1186 1187 1198 1199 1200 32)))

;; Look at the unicode-readme.txt in cl-pdf to see how to load unicode fonts
(defun unicode-hello (&optional (file #P"/tmp/hello-u.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Unicode Example" (pdf:register-page-reference))
	(pdf:set-line-width 0.1)
	(let ((content
	       (compile-text ()
			     (vspace 100)
		 (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 50 :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 26)
			      "The cool Common Lisp typesetting system")
			    (vspace 50)
			    (with-style (:font "TimesNewRomanPSMT" :font-size 36)
			      (put-string *unicode-test-string*):eol
                              (put-string "A non unicode-string in unicode font"))))))
	  (draw-block content 20 800 545 700))))
    (pdf:write-document file)))
