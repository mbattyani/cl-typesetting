;;; Klaus Weinder extensions
;;; This stuff will be dispatched into better locations later.

(in-package typeset)

;; reference handling

(defvar *ref-table* (make-hash-table :test #'equal))
(defvar *ref-counter* 0)
(defvar *bad-reference* nil)

(defclass ref-mark ()
  ((id :accessor ref-id :initform nil :initarg :id)
   (value :accessor ref-mark-value :initform nil :initarg :value)
   (page :accessor ref-mark-page :initform nil)
   (x :accessor ref-x :initform nil)
   (y :accessor ref-y :initform nil)))

(defmethod stroke ((mark ref-mark) x y)
  (setf (ref-mark-page mark) (length (pages pdf:*document*))
	(ref-x mark) x
	(ref-y mark) y))

(defmacro ref-get (id)
  `(gethash ,id *ref-table*))

(defun make-ref-mark (id &optional value)
  (let ((mark (or (ref-get id)
		  (make-instance 'ref-mark
				 :id id))))
    (setf (ref-get id) mark)
    (setf (ref-mark-value mark) value)
    (add-box mark)))

(defun ref-page (id)
  (let* ((ref (ref-get id))
	 (page (if ref (ref-mark-page ref))))
    (cond (page page)
	  (t (push id *bad-reference*)
	     999))))

(defun put-ref-page (id)
  (put-string (format nil "~D" (ref-page id))))

(defgeneric ref-value (ref))

(defmethod ref-value ((ref ref-mark))
  (if ref (ref-mark-value ref)))

(defmethod ref-value ((id t))
  (let ((ref (ref-get id)))
    (if ref (ref-mark-value ref))))

(defun put-ref-value (id)
  (put-string (ref-value id)))

(defun this-page-number ()
  (length (pages pdf:*document*)))

(defun make-ref-page-mark (reftype value)
  (make-ref-mark (cons reftype (incf *ref-counter*)) value))

(defun get-latest-ref-to (reftype for-page)
  (let ((refs nil))
    ;; Find all references of a type, store unsorted
    ;; (ordinal page ref) lists in "refs".
    (maphash (lambda (key ref)
	       (if (and (consp key)
			(equal reftype (car key)))
		   (push (list (cdr key)
			       (or (ref-mark-page ref) most-positive-fixnum)
			       ref)
			 refs)))
	     *ref-table*)
    ;; Now walk through the reverse sorted references,
    ;; and get the last matching one on or before the
    ;; current page.
    (third (find-if (lambda (page)
		      (<= page for-page))
		    (sort refs #'> :key #'car)
		    :key #'cadr))))

(defun current-ref-value (reftype)
  (ref-value (get-latest-ref-to reftype (this-page-number))))

(defmacro itemize ((&key (indent 20)
			   text-style
			   (item-fmt "~D. ")
			   (start-from 1)
			   item-style)
                     &body body)
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

(defmacro item ((&rest style) &body body)
  `(with-style ,style ,@body))

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


;; higher-level layout

(defun safe-read (stream)
  (let ((*package* (find-package "TYPESET"))
	(*read-eval* nil))
    (read stream)))

;; change bars

(defvar *change-bar-start* nil)
(defvar *change-bar-end* nil)

(defclass change-mark ()
  ((type :accessor mark-type :initform nil :initarg :type)))

(defmethod stroke ((mark change-mark) x y)
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

(defun page-decorations (page)
  (pdf:with-saved-state
    (pdf:set-line-width 2.0)
    (let ((xm (if (oddp (this-page-number))
		  (* 0.95 (aref (pdf::bounds page) 2))
		  (* 0.05 (aref (pdf::bounds page) 2)))))
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
	      (pdf:stroke)))))
  (setq *change-bar-start* nil
	*change-bar-end* nil))

;; Note that the tree argument to render-document is a dead list of
;; symbols and strings. This is a prerequisite for being to handle
;; documents that are completely generated at runtime.

(defun render-document (tree &key
			(file #P"/tmp/stuff.pdf")
			(twosided t)
			(paper-size :letter))
  "Render the document specified by tree, which is a s-exp containing
recursive typesetting commands. It gets eval'ed here to typeset it."
  (do ((*ref-table* (make-hash-table :test #'equal))
       (*ref-counter* 0)
       (*bad-reference* nil)
       (pass 0 (1+ pass)))
      ((or (> pass 1)
	   (and (> pass 0)
		(not *bad-reference*)))
       *bad-reference*)
    (setq *bad-reference* nil)
    (format t "Pass ~d~%" pass)
    (with-document ()
      (let ((margins '(72 72 72 50))
	    (header (lambda (pdf:*page*)
		      (if (current-ref-value :header-enabled)
			  (let ((inside (or (current-ref-value :title) "Untitled Document"))
				(outside (current-ref-value :chapter)))
			    (if (and twosided (evenp (this-page-number)))
				(compile-text (:font "Times-Roman" :font-size 10)
				  (hbox (:align :center :adjustable-p t)
				    (put-string outside)
				    :hfill
				    (with-style (:font "Times-Italic")
				      (put-string inside))))
				(compile-text (:font "Times-Roman" :font-size 10)
				  (hbox (:align :center :adjustable-p t)
				    (with-style (:font "Times-Italic")
				      (put-string inside))
				    :hfill
				    (put-string outside))))))))
	    (footer (lambda (pdf:*page*)
		      (if (current-ref-value :footer-enabled)
			  (let ((inside (or (current-ref-value :version) ""))
				(outside (format nil "Page ~d of ~d"
						 (this-page-number)
						 (ref-page "DocumentEnd"))))
			    (if (and twosided (evenp (this-page-number)))
				(compile-text (:font "Times-Roman" :font-size 10)
				  (hbox (:align :center :adjustable-p t)
				    (put-string outside)
				    :hfill
				    (put-string inside)))
				(compile-text (:font "Times-Roman" :font-size 10)
				  (hbox (:align :center :adjustable-p t)
				    (put-string inside)
				    :hfill
				    (put-string outside)))))))))
	
	(draw-pages (eval (list 'compile-text () tree))
		    :margins margins :header header :footer footer
		    :size paper-size :finalize-fn #'page-decorations)
	(when pdf:*page* (finalize-page pdf:*page*))
	(pdf:write-document file)))))

;; Example follows.

(defun decoration-random-background (box x y dx dy)
  (pdf:with-saved-state
    (pdf:set-rgb-fill (random 1.0) (random 1.0) (random 1.0))
    (pdf:basic-rect x y dx dy)
    (pdf:fill-path)))

(defun decoration-green-background (box x y dx dy)
  (pdf:with-saved-state
    (pdf:set-rgb-fill 0.7 1.0 0.7)
    (pdf:basic-rect x y dx dy)
    (pdf:fill-path)))

(defun decoration-gray-box (box x y dx dy)
  (pdf:with-saved-state
    (pdf:set-gray-stroke 0.5)
    (pdf:set-line-width 0.5)
    (pdf:basic-rect x y dx dy)
    (pdf:stroke)))

(defun decoration-underline (box x y dx dy)
  (pdf:with-saved-state
    (pdf:set-gray-stroke 0)
    (pdf:set-line-width 0.5)
    (pdf:move-to x (+ y (* 0.9 dy)))
    (pdf:line-to (+ x dx) (+ y (* 0.9 dy)))
    (pdf:stroke)))

(defun decoration-strikethrough (box x y dx dy)
  (pdf:with-saved-state
    (pdf:set-color-stroke :red)
    (pdf:set-line-width 0.5)
    (pdf:move-to x (+ y (* 0.66 dy)))
    (pdf:line-to (+ x dx) (+ y (* 0.66 dy)))
    (pdf:stroke)))

(defun decoration-crosshatch (box x y dx dy)
  (pdf:with-saved-state
    (pdf:set-color-stroke :black)
    (pdf:set-line-width 0.5)
    (pdf:move-to x (+ y (* 0.3 dy)))
    (pdf:line-to (+ x dx) (+ y (* 0.9 dy)))
    (pdf:stroke)))

(defun decoration-nil (box x y dx dy)
  (print "Called nil decoration.")
  nil)

(defun document-test ()
  (render-document
   '(with-style (:font "Times-Roman" :font-size 12
		 :top-margin 3 :bottom-margin 4)
     (make-ref-page-mark :title "Titled Document")
     (make-ref-page-mark :version "Version 1.x")
     (make-ref-page-mark :header-enabled nil)
     (make-ref-page-mark :footer-enabled nil)

					#||
     :vfill
     (paragraph (:font "Helvetica-Bold" :font-size 24 :h-align :center :bottom-margin 20)
     "This is the Document Title")
     (paragraph (:font "Helvetica-Bold" :font-size 16 :h-align :center)
     "A. N. Author")
     :vfill
     :eop
     ||#
		       
     (make-ref-page-mark :header-enabled t)
     (make-ref-page-mark :footer-enabled t)
     (make-ref-mark '(:chapter . 0) "Table of Contents")
     (with-style (:font "Helvetica")
       (paragraph (:h-align :left-but-last :top-margin 3 :bottom-margin 4)
	 (put-ref-value '(:chapter . 1))
	 (dotted-hfill)
	 (put-ref-page '(:chapter . 1)))
       (paragraph (:h-align :left-but-last :top-margin 3 :bottom-margin 4)
	 (put-ref-value '(:chapter . 2))
	 "This is a chapter with an insanely long title, to verify if the leader dots at the end of the line will be printed properly"
	 (dotted-hfill)
	 (put-ref-page '(:chapter . 2))))
					#||
     :eop
     ||#
     
     (make-ref-mark '(:chapter . 1) "Introduction")
     (paragraph (:h-align :left :top-margin 3 :bottom-margin 4)
       "Test with "
       (with-style (:font "Times-Bold")
	 "bold")
       " and "
       (with-style (:font "Times-Italic")
	 "italic")
       " text.")

					#||
     (paragraph (:h-align :left :top-margin 3 :bottom-margin 4)
     (make-ref-mark "link-from")
     "See also stuff on page "
     (put-ref-page "stuff")
     ".")
     ||#

     (paragraph (:top-margin 3 :bottom-margin 4)
       "Inline alignment test: ["
       (put-filled-string "L" 30)
       "]["
       (put-filled-string "C" 30 :align :center)
       "]["
       (put-filled-string "R" 30 :align :right)
       "]")

     (paragraph (:top-margin 3 :bottom-margin 4)
       "This is just normal text. "
       (with-style (:pre-decoration #'decoration-random-background)
	 "This should look different.")
       " Back to normal. There's more; "
       (with-style (:post-decoration #'decoration-underline)
	 "multi word underline")
       " and "
       (with-style (:pre-decoration #'decoration-gray-box)
	 "visible boxes mode")
       " and "
       (with-style (:post-decoration #'decoration-crosshatch)
	 "crosshatch."))

     (paragraph (:top-margin 3 :bottom-margin 4)
       "This paragraph is not interesting.")


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
     

					#||
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

     :eop
     
     (make-ref-mark '(:chapter . 2) "Interesting Stuff")
     (paragraph (:font "Courier" :top-margin 3 :bottom-margin 4)
     (make-ref-mark "stuff")
     "Some" :eol "more" :eol "Text." )
     (paragraph (:h-align :left :top-margin 3 :bottom-margin 4)
     "This is linked to from page "
     (put-ref-page "link-from")
     ".")
     ||#
     
     (make-ref-mark "DocumentEnd"))))
