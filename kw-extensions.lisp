;;; Klaus Weidner extensions
;;; This stuff will be dispatched into better locations later.

(in-package #:typeset)

;; user-configurable default settings

(defvar *paper-size* :letter
  "Paper format, supported values as in tt:top-level, i.e. :a4 or :letter")

(defvar *page-margins* '(72 72 72 50)
  "Print margins LEFT TOP RIGHT BOTTOM in 1/72 inch units")

(defvar *twosided* t
  "If true, use alternating page headers suitable for duplex printing.")

(defvar *toc-depth* 3
  "Number of heading levels to print in table of contents.")

(defvar *watermark-fn* nil
  ;; FIXME: currently draws on top of page instead of below new
  ;; content. Needs toplevel extension :new-page-fn
  "Run this function (with the current PAGE as argument )for each new
page before printing anything on it. Useful for watermarks or
corporate identity decorations.")

(defvar *add-chapter-numbers* t)

(defvar *verbose* nil
  "Print progress report while running.")

(defvar *font-normal* "Times-Roman")
(defvar *font-bold* "Times-Bold")
(defvar *font-italic* "Times-Italic")
(defvar *font-bold-italic* "Times-BoldItalic")
(defvar *font-monospace* "Courier")

(defvar *default-text-style*
  (list :font *font-normal* :font-size 10
	:top-margin 3 :bottom-margin 4))

(defvar *chapter-styles*
  '((:font "Helvetica-Bold" :font-size 20
     :top-margin 14 :bottom-margin 10)
    (:font "Helvetica-BoldOblique"
     :font-size 18 :top-margin 10 :bottom-margin 8)
    (:font "Helvetica-Bold" :font-size 16
     :top-margin 10 :bottom-margin 8)
    (:font "Helvetica-BoldOblique" :font-size 14
     :top-margin 10 :bottom-margin 8)
    (:font "Helvetica-Bold" :font-size 12
     :top-margin 10 :bottom-margin 8)
    (:font "Helvetica-BoldOblique" :font-size 12
     :top-margin 10 :bottom-margin 8))
  "Paragraph styles used for various depths of section headings")

;; state for internal chapter handling

(defvar *chapters* nil
  "Ordered list of chapter information. For each chapter, contains
reference and title. Example:

 (((:chapter (1)) \"Intro\")
  ((:chapter (1 1)) \"More stuff\"))")

(defvar *chapter-nums* nil
  "List of chapter numbers of current section, i.e. (1 2 3) for 1.2.3")

(defvar *change-bar-start* nil)
(defvar *change-bar-end* nil)

;;; higher-level chapter number and table of contents handling

(defun chpnum-string (nums)
  (format nil "~{~S~^.~}" nums))

(defun new-chp-ref (level text)
  "Insert current chapter information into *chapters*, automatically
incrementing the elements of *chapter-nums*. Returns an ID suitable for a reference."
  (let ((higher (subseq *chapter-nums* 0 level))
	(current (nth level *chapter-nums*)))
    (setq *chapter-nums* (if current
			     (append higher (list (1+ current)))
			     (append higher (list 1)))))
  (let ((cs (cons :chapter *chapter-nums*)))
    (push (list cs text) *chapters*)
    cs))

(defun chp-ref (level text)
  ;; OBSOLETE, fixme, use new-chp-ref instead
  (list (new-chp-ref level text) :data text))

(defun make-toc ()
  "Generate table of contents from the information in *chapters*, to
maximum depth *toc-depth*."
  ;; FIXME: Indentation and font selection currently hardcoded
  (prog1
      (mapcar (lambda (chp)
		;; format table of contents entry
		(let* ((ref (first chp))
		       (cnum (cdr ref))
		       (depth (length cnum))
		       (title (second chp)))
		  (when (<= depth *toc-depth*)
		    `(paragraph (:h-align :left-but-last
				 :left-margin
				 ,(case depth
					(1 0) (2 10) (t 20))
				 :top-margin
				 ,(case depth
					(1 3) (t 0))
				 :bottom-margin
				 ,(case depth
					(1 2) (t 0))
				 :font-size
				 ,(case depth
					(1 12) (2 10) (t 9)))
		      (put-string ,(chpnum-string cnum))
		      (put-string " ")
		      (put-string ,title)
		      (dotted-hfill)
		      (with-style (:font-size 10) (put-ref-point-page-number ',ref))))))
	      (reverse *chapters*))
    (setf *chapter-nums* nil
	  *chapters* nil)))

(defun chapter-markup (level heading &optional content)
  (let* ((ref-id (new-chp-ref level heading))
	 (cprefix (if *add-chapter-numbers*
		      (concatenate 'string (chpnum-string (cdr ref-id)) ". ")
		      ""))
	 (numbered-heading (concatenate 'string cprefix heading)))
    `(pdf:with-outline-level (,numbered-heading
			      (pdf::register-named-reference
			       (vector (find-ref-point-page-content ',ref-id) "/Fit")
			       ,(pdf::gen-name "R")))
      ,(if (eql level 0) :fresh-page "")
      ,(if (eql level 0) `(set-contextual-variable :chapter ,heading) "")
      (paragraph ,(nth level *chapter-styles*)
       (mark-ref-point ',ref-id :data ,heading :page-content t)
       (put-string ,cprefix)
       ,@(if (null content)
	     (list heading)
	     content)))))

;; higher-level layout

(defun put-filled-string (string width &key (align :left))
  "place aligned string in fixed-width space"
  (let* ((string-width
	  (loop for char across string
		summing (pdf:get-char-width char *font* *font-size*)))
	 (blank (- width string-width)))
    (case align
      ((:left) (verbatim string) (hspace blank))
      ((:center)
       (hspace (* 0.5 blank)) (verbatim string) (hspace (* 0.5 blank)))
      ((:right) (hspace blank) (verbatim string)))))


(defun put-ref-point-page-number (ref)
  (put-string (format nil "~d" (find-ref-point-page-number ref))))

(defun put-ref-point-value (ref)
  (put-string (find-ref-point-page-data ref "*undefined*")))

(defun number-base-list (n base)
  "Return number N converted to base BASE, represented as list of
integers, lowest first. Example: (number-base-list 18 16) => (2 1)"
  (multiple-value-bind (remainder digit) (truncate n base)
    (if (> remainder 0)
	(cons digit (number-base-list remainder base))
	(list digit))))

(defun alpha-item (stream num &optional colon-p at-sign-p)
  "Prints input NUM to STREAM as sequence of letters corresponding to
a base-26 representation. Intended for use as custom FORMAT handler,
Use with colon modifier for uppercase."
  (declare (ignore at-sign-p))
  (princ (map 'string
	      (lambda (digit)
		(code-char (+ (char-code (if colon-p #\a #\A))
			      digit
			      -1)))
	      (nreverse (number-base-list num 26)))
	 stream))

 (defmacro item ((&rest style) &body body)
  "Render a list item. If BODY is a PARAGRAPH, use its body only."
  (if (and (consp (car body))
	   (eq 'paragraph (caar body)))
      `(with-style ,style ,@(nthcdr 2 (car body)) ,@(cdr body))
      `(with-style ,style ,@body)))

(defmacro itemize ((&key (indent 20)
			 (item-fmt "~D. ")
			 (start-from 1)
			 text-style
			 item-style)
		   &body body)
  "Render the BODY (which must contain of child ITEM elements) as an
itemized list. Usable both for ordered lists (formatted using
ITEM-FMT) and unordered list (using a constant string as ITEM-FMT).

Arguments:

item-fmt    Format string used to print the integer item number.
            Use a constant string for unordered (bullet) lists.
            Useful values include:
	      \"~D. \"                  Decimal:  1. 2. 3. 4.
	      \"~@R \"                  Roman:    I II III IV
	      \"~(~@R~) \"              lc roman: i ii iii iv
	      \"~/tt::alpha-item/. \"   Alpha:    A. B. C. ... AA. AB.
	      \"~:/tt::alpha-item/. \"  lc alpha: a. b. c. ... aa. ab.

start-from  Number of the first item, default 1

item-style  Style used for printing the item numbers.

text-style  Style used for printing the item body text."
  `(let ((%enumerate-indents% (cons ,indent %enumerate-indents%)))
    ,@(loop for item in body
            for i from start-from collect
            `(paragraph (:left-margin (reduce #'+ %enumerate-indents%)
                         :first-line-indent (- ,indent)
                         ,@text-style)
              (with-style ,item-style
		(put-filled-string ,(format nil item-fmt i)
		 ,indent :align :right))
              ,item))))

;; change bars

(defclass change-mark ()
  ((type :accessor mark-type :initform nil :initarg :type)))

(defmethod stroke ((mark change-mark) x y)
  (declare (ignore x))  
  ;; "stroking" change marks just records their positions for later
  ;; rendering in the postprocessing hook
  (cond ((eq :start-insert (mark-type mark))
	 (push (cons (+ y *font-size*)
		     :insert) *change-bar-start*))
	((eq :start-delete (mark-type mark))
	 (push (cons (+ y *font-size*)
		     :delete) *change-bar-start*))
	(t (push y *change-bar-end*))))

(defun change-start-insert ()
  (add-box (make-instance 'change-mark :type :start-insert)))

(defun change-start-delete ()
  (add-box (make-instance 'change-mark :type :start-delete)))

(defun change-end ()
  (add-box (make-instance 'change-mark :type :end)))

(defun draw-change-bars (page)
  ;; called when page is being finalized, draw the change bars based
  ;; on the recorded positions.
  (pdf:with-saved-state
      (pdf:set-line-width 2.0)
    (let ((xm (if (oddp pdf:*page-number*)
		  ;; this assumes 72pt margins
		  (- (aref (pdf::bounds page) 2) 48)
		  (+ 48 4)))
	  (cross-page nil))
      
      (when (> (length *change-bar-start*)
	       (length *change-bar-end*))
	;; close cross-page change bar(s)
	;; FIXME: need to handle two cross-page bars
	(setq cross-page
	      (list (cons (- (aref (pdf::bounds page) 3)
			     (nth 1 *page-margins*))
			  (cdar *change-bar-start*))))
	(push (nth 3 *page-margins*) *change-bar-end*))
      
      (loop for y0c in *change-bar-start*
	    for y1 in *change-bar-end*
	    do
	    (let* ((y0 (car y0c))
		   (type (cdr y0c))
		   (color (if (eq type :insert)
			      #x33aa33
			      #xaa3333))
		   (x (if (eq type :insert)
			  xm
			  (- xm 4))))
	      (pdf:set-color-stroke color)
	      (pdf:move-to x y0)
	      (pdf:line-to x y1)
	      (pdf:stroke)))
      
      (setq *change-bar-start* cross-page
	    *change-bar-end* nil))))

(defun draw-watermark (page)
  "Put the watermark on the page. FIXME: currently draws on top of
page instead of below new ;; content. Needs toplevel extension
:new-page-fn"
  (declare (ignorable page))
  (when (functionp *watermark-fn*)
    (pdf:with-saved-state
	(funcall *watermark-fn* page))))

(defun page-decorations (page)
  (draw-watermark page)
  (draw-change-bars page))

;; This is needed to allow style settings to survive across separate
;; draw-pages/compile-text invocations.
(defmacro set-contextual-style (style)
  `(progn
    (set-contextual-variable :style ',style)
    (set-style ,style)))

;; Note that the tree argument to render-document is a dead list of
;; symbols and strings. This is a prerequisite for being to handle
;; documents that are completely generated at runtime.

(defun render-document (trees &key
			(file #P"/tmp/output.pdf")
			(twosided *twosided*)
			(paper-size *paper-size*))
  "Render the document specified by the trees, which is a s-exp containing
a list of recursive typesetting commands. It gets eval'ed here to typeset it."
  (setq cl-typesetting-hyphen::*left-hyphen-minimum* 999
	cl-typesetting-hyphen::*right-hyphen-minimum* 999)
  (tt:with-document ()
    (let ((margins *page-margins*)
	  (header (lambda (pdf:*page*)
		    (if (get-contextual-variable :header-enabled)
			(let ((inside (get-contextual-variable :title "*Untitled Document*"))
			      (outside (get-contextual-variable :chapter "*No Chapter*")))
			  (if (and twosided (evenp pdf:*page-number*))
			      (compile-text ()
					    (with-style (:font-size 10
								    :pre-decoration :none
								    :post-decoration :none)
					      (hbox (:align :center :adjustable-p t)
						    (with-style (:font *font-normal*)
						      (put-string outside))
						    :hfill
						    (with-style (:font *font-italic*)
						      (put-string inside)))))
				
			      (compile-text ()
					    (with-style (:font-size 10
								    :pre-decoration :none
								    :post-decoration :none)
					      (hbox (:align :center :adjustable-p t)
						    (with-style (:font *font-italic*)
						      (put-string inside))
						    :hfill
						    (with-style (:font *font-normal*)
						      (put-string outside)))))))
			(compile-text () ""))))
	    
	  (footer (lambda (pdf:*page*)
		    (if (get-contextual-variable :footer-enabled)
			(let ((inside (get-contextual-variable :version ""))
			      (outside (format nil "Page ~d of ~d"
					       pdf:*page-number*
					       (find-ref-point-page-number "DocumentEnd"))))
			  (if (and twosided (evenp pdf:*page-number*))
			      (compile-text ()
					    (with-style (:font *font-normal*
							       :font-size 10
							       :pre-decoration :none
							       :post-decoration :none)
					      (hbox (:align :center :adjustable-p t)
						    (put-string outside)
						    :hfill
						    (put-string inside))))
			      (compile-text ()
					    (with-style (:font *font-normal*
							       :font-size 10
							       :pre-decoration :none
							       :post-decoration :none)
					      (hbox (:align :center :adjustable-p t)
						    (put-string inside)
						    :hfill
						    (put-string outside))))))
			(compile-text () "")))))
    
      (set-contextual-variable :header-enabled nil)
      (set-contextual-variable :footer-enabled nil)
      (set-contextual-style (:pre-decoration :none))
      
      (dolist (tree trees)
	(draw-pages (eval `(compile-text ()
			    (with-style ,*default-text-style* 
			      (set-style ,(get-contextual-variable :style ()))
			      ,tree)))
		    :margins margins
		    :header header
		    :footer footer
		    :size paper-size
		    :finalize-fn #'page-decorations))

      (when pdf:*page* (finalize-page pdf:*page*))

      (when (and (final-pass-p)
		 *undefined-references*)
	(format t "Undefined references:~%~S~%"
		*undefined-references*))
      
      (pdf:write-document file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; end of code, examples follow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun document-test-simple ()
  (render-document
   `((paragraph () "Table of Contents")
     ,@(make-toc)
     :fresh-page
     
     ,(chapter-markup 0 "Introduction")
     (paragraph () "test")
     ,(chapter-markup 1 "More stuff")
     (paragraph () "and more text")
     ,(chapter-markup 0 "New chapter")
     (paragraph () "and even more text")
     (mark-ref-point "DocumentEnd"))))

;;;

(defun watermark-draft (page)
  "Example for a page decoration"
  (declare (ignorable page))
  (pdf:with-saved-state
      (pdf:in-text-mode
	(pdf:set-text-rendering-mode 1)
	(pdf:set-color-stroke #xcccccc)
	(pdf:set-line-width 4)
	(pdf:set-font (pdf:get-font "Helvetica-Bold") 200.0)
	(pdf:translate 180 100)
	(pdf:rotate 55)
	(pdf:move-text 0 0)
	(pdf:draw-text "D r a f t"))))

(defun decoration-random-background (box x y dx dy)
  (declare (ignore box))
  (pdf:with-saved-state
    (pdf:set-rgb-fill (random 1.0) (random 1.0) (random 1.0))
    (pdf:basic-rect x y dx dy)
    (pdf:fill-path)))

(defun decoration-green-background (box x y dx dy)
  (declare (ignore box dy))
  (pdf:with-saved-state
    (pdf:set-rgb-fill 0.7 1.0 0.7)
    (pdf:basic-rect x (- y 2) dx (- 1 *font-size*))
    (pdf:fill-path)))

(defun decoration-circles (box x y dx dy)
  (declare (ignore box))
  (pdf:with-saved-state
    (pdf:set-color-stroke #xff33cc)
    (pdf:set-line-width 0.3)
    (pdf:circle (+ x (* 0.5 dx)) (+ y (* 0.60 dy)) (* *font-size* 0.4))
    (pdf:stroke)))

(defun decoration-gray-box (box x y dx dy)
  (declare (ignore box))
  (pdf:with-saved-state
    (pdf:set-gray-stroke 0.5)
    (pdf:set-line-width 0.5)
    (pdf:basic-rect x y dx dy)
    (pdf:stroke)))

(defun decoration-underline (box x y dx dy)
  (declare (ignore box))
  (pdf:with-saved-state
    (pdf:set-gray-stroke 0)
    (pdf:set-line-width (* 0.06 *font-size*))
    (pdf:move-to x (+ y (* 0.9 dy)))
    (pdf:line-to (+ x dx) (+ y (* 0.9 dy)))
    (pdf:stroke)))

(defun decoration-strikethrough (box x y dx dy)
  (declare (ignore box))
  (pdf:with-saved-state
    (pdf:set-color-stroke :red)
    (pdf:set-line-width (* 0.06 *font-size*))
    (pdf:move-to x (+ y (* 0.66 dy)))
    (pdf:line-to (+ x dx) (+ y (* 0.66 dy)))
    (pdf:stroke)))

(defun decoration-crosshatch (box x y dx dy)
  (declare (ignore box))
  (pdf:with-saved-state
    (pdf:set-color-stroke :black)
    (pdf:set-line-width 0.5)
    (pdf:move-to x (+ y (* 0.3 dy)))
    (pdf:line-to (+ x dx) (+ y (* 0.9 dy)))
    (pdf:stroke)))

(defun decoration-nil (box x y dx dy)
  (declare (ignore box x y dx dy))
  (print "Called nil decoration.")
  nil)

(defun document-test ()
  (render-document
   '((set-contextual-variable :title "Titled Document")
     (set-contextual-variable :version "Version 1.x")
		       
     (set-contextual-variable :header-enabled t)
     (set-contextual-variable :footer-enabled t)

     (mark-ref-point '(:chapter . '(0)) :data "Table of Contents")
     (with-style (:font *font-normal*)
       (paragraph (:h-align :left-but-last :top-margin 3 :bottom-margin 4)
	(put-ref-point-value '(:chapter . '(1)))
	(dotted-hfill)
	(put-ref-point-page-number '(:chapter . '(1))))
       (paragraph (:h-align :left-but-last :top-margin 3 :bottom-margin 4)
	(put-ref-point-value '(:chapter . '(2)))
	" This is a chapter with an insanely long title, to verify if the leader dots at the end of the line will be printed properly"
	(dotted-hfill)
	(put-ref-point-page-number '(:chapter . '(2)))))
     
     (mark-ref-point '(:chapter . '(1)) :data "Introduction")
     (paragraph (:h-align :left :top-margin 3 :bottom-margin 4)
      "Test with "
      (with-style (:font *font-bold*)
	"bold")
      " and "
      (with-style (:font *font-italic*)
	"italic")
      " text.")

     (paragraph (:top-margin 3 :bottom-margin 4)
      "This paragraph has an undefined reference (see page " 
      (put-ref-point-page-number "no-such-ref")
      "), and mentions KITTENS."
      (mark-ref-point "KITTENS"))

     (paragraph (:h-align :left :top-margin 3 :bottom-margin 4)
      "This paragraph has some "
      (change-start-insert)
      (with-style (:pre-decoration #'decoration-green-background)
	"inserted words")
      (change-end)
      " in it. Here's some filler to move to the next line. The now following line has both "
      (change-start-insert)
      (with-style (:pre-decoration #'decoration-green-background)
	"inserted words")
      (change-end)
      " and "
      (change-start-delete)
      (with-style (:post-decoration #'decoration-strikethrough)
	"deleted ones")
      "."
      (change-end)
      " Now here's even more filler text to again move to the next
line, to demonstrate having just the following word "
      (change-start-delete)
      (with-style (:post-decoration #'decoration-strikethrough)
	"deleted")
      (change-end)
      ".")

     (paragraph (:top-margin 3 :bottom-margin 4)
      "These are some "
      (change-start-insert)
      (set-contextual-style (:pre-decoration #'decoration-green-background))
      "random words. The changed area starts in this paragraph.")
     
     (paragraph (:top-margin 3 :bottom-margin 4)
      "The end-of-change marker is in this paragraph, in the middle of "
      (with-style (:font *font-italic*)
	"italic"
	(change-end)
	(set-contextual-style (:pre-decoration :none))
	" text.")

      "The change markers are handled in depth-first tree order and
are not required to be nested with the content. That makes automated change marking much easier.")

     (paragraph (:top-margin 3 :bottom-margin 4)
      "Just for fun, here are some more text decoration
experiments. This is just normal text. "
      (with-style (:pre-decoration #'decoration-random-background)
	"This should look different.")
      " Back to normal. There's more; "
      (with-style (:post-decoration #'decoration-underline)
	"multi word underline,")
      " and "
      (with-style (:pre-decoration #'decoration-gray-box)
	"visible boxes mode,")
      " and "
      (with-style (:pre-decoration #'decoration-circles)
	"circles,")
      " and "
      (with-style (:post-decoration #'decoration-crosshatch)
	"crosshatch."))

     (paragraph (:top-margin 3 :bottom-margin 4)
      "Inline alignment test: ["
      (put-filled-string "L" 30)
      "]["
      (put-filled-string "C" 30 :align :center)
      "]["
      (put-filled-string "R" 30 :align :right)
      "]")

     (itemize (:text-style (:h-align :left :top-margin 3 :bottom-margin 4))
      (item () "This is the first item, and it's rather
long-winded. wjr aireg iureahg iureahg iureahg iureahg lrea hlieahg
eliurhg eliurhg eliurhg liureahglueairhg liurea hliure hgliueahg
liureahg liurea hgliureahg liureahg liureahg liureag realih."
       (itemize (:text-style (:top-margin 3 :bottom-margin 4) :item-fmt "- ")
		(item () "a" "1")
		(item () "b" "2")
		(item () "c")
		(item () "d")))
       
      (item () "This is the second item, and it's rather long-winded. wjr
aireg iureahg iureahg iureahg iureahg lrea hlieahg eliurhg eliurhg
eliurhg liureahglueairhg liurea hliure hgliueahg liureahg liurea
hgliureahg liureahg liureahg liureag realih."))

     (paragraph ()
      "Start a new page:"
      :eop)
     
     (mark-ref-point '(:chapter . '(2)) :data "Interesting Stuff")
     (paragraph (:font "Courier" :top-margin 3 :bottom-margin 4)
      (mark-ref-point "stuff")
      "Some" :eol "more" :eol "Text." )
     (paragraph (:h-align :left :top-margin 3 :bottom-margin 4)
      "KITTENS are mentioned on page "
      (put-ref-point-page-number "KITTENS")
      ".")
     
     (mark-ref-point "DocumentEnd"))))


