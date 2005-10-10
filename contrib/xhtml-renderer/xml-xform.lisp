(in-package typeset)

;; (asdf::use :cl-typesetting :xmls)
 
;; For prettier printing of XHTML output at the REPL, use with:
;; (setf (readtable-case *readtable*) :invert)

(defun char-invert-case (c)
  (if (upper-case-p c)
      (char-downcase c)
      (char-upcase c)))

(defun invert-if-single-case (s)
  (if (and (some #'upper-case-p s)
	   (some #'lower-case-p s))
      s
      (map 'string #'char-invert-case s)))

(defun xml-make-keyword (s)
  (if (symbolp s)
      s
      (intern (invert-if-single-case (substitute #\! #\: s))
	      "KEYWORD")))

;; XML trees must be of the form (elem attr . content), with attr
;; being a list of (key . value) conses, and content being a list of
;; strings and XML trees. There should be no consecutive strings or
;; empty strings in the content list.
;;
;; Example: <a href="http://example.com" title="Example">a <b>bold</b> link</a>
;;   ==>    
;; (:a ((:href . "http://example.com") (:title . "Example")) "a " (:b () "bold") " link")

;; Accessor functions 
(defun xml-elem (tree) (car tree))
(defun xml-attr (tree) (cadr tree))
(defun xml-clst (tree) (cddr tree))

(defun xml-attr-get (attr key)
  (cdr (assoc key attr)))

(defun xml-xform (elem-xform tree &optional parents)
  "Recursively transform XML tree depth-first by calling the supplied
elem-xform function on each node."
  (declare (function elem-xform))
  (let ((clst (mapcar (lambda (c)
			(if (consp c)
			    ;; recurse into content
			    (xml-xform elem-xform c
				       (cons c parents))
			    c))
		      (xml-clst tree))))
    (funcall elem-xform (nconc (list (xml-elem tree)
				     (xml-attr tree))
			       clst)
	     parents)))

(defun xml-collapse-sxml-namespace (node parents)
  "Remove namespace information from XML tree, and use keyword symbols
for elements and attributes. Example: (\"foo\" . \"http://namespace\")
=> :foo"
  (declare (ignorable parents))
  (let ((elem (xml-elem node))
	(attr (xml-attr node))
	(clst (xml-clst node)))
    (declare (ignorable elem attr clst))
    (nconc (list (if (consp elem)
		     (xml-make-keyword (car elem))
		     (xml-make-keyword elem))
		 (mapcar (lambda (a)
			   (cons (if (consp (car a))
				     (xml-make-keyword (cdar a))
				     (xml-make-keyword (car a)))
				 (cdr a)))
			 attr))
	   clst)))

(defun string-collapse-whitespace (string)
  (do* ((chars (map 'list #'identity (substitute #\Space #\NewLine string))
	       (cdr chars))
      (c (car chars) 
	 (car chars))
      (new nil))
    ((null chars) (map 'string #'identity (nreverse new)))
  (unless (and (eql c #\Space) (eql (car new) #\Space))
    (push c new))))

(defun verbatim-p (parents)
  (find-if (lambda (p)
	     (or (member (xml-elem p) '(:pre :ins :del))
		 ;; FIXME: assumes that if the attribute was
		 ;; specified, that it has the value "false"...
		 (assoc :white-space-collapse (xml-attr p))))
	   parents))

(defun remove-spaces (clst)
  (remove-if (lambda (c)
	       (or (null c)
		   (equal c " ")))
	     clst))

(defun remove-leading-space (clst)
  (if (equal " " (car clst))
      (cdr clst)
      clst))

(defun xml-collapse-whitespace (node parents)
  (declare (ignorable parents))
  (let ((elem (xml-elem node))
	(attr (xml-attr node))
	(clst (xml-clst node)))
    (declare (ignorable elem attr clst))
    (nconc (list elem attr)
	   (if (verbatim-p (cons node parents))
	       clst ;; keep unmodified for this element
	       (remove-leading-space
		(mapcar (lambda (c)
			  (if (stringp c)
			      (string-collapse-whitespace c)
			      c))
			clst))))))


(defun attr-list-to-assoc (node parents)
  "convert sxml (attr val) list to (attr . val) conses"
  (declare (ignorable parents))
  (let ((elem (xml-elem node))
	(attr (xml-attr node))
	(clst (xml-clst node)))
    (declare (ignorable elem attr clst))
    (nconc (list elem (mapcar (lambda (a)
				(cons (car a)
				      (cadr a)))
			      attr))
	   clst)))

(defun xml-extract-text (tree)
  "Extract text strings from XML file."
  (let ((clst (xml-clst tree))
	(strings nil))
    (dolist (c clst)
      (cond ((stringp c) 
	     (push c strings)) 
	    ((consp c)
	     (push (xml-extract-text c) strings))))
    (apply #'concatenate 'string (reverse strings))))

;; Note: load-xml-file can't handle non-XML files.
;;
;; The following command is useful to convert legacy HTML
;; to parseable XMTHL:
;;
;;   tidy -wrap 0 -asxhtml SLES-security-guide.html

(defun load-xml-file (file)
  ;;setq xmls::*entities*
  ;;(adjoin '("AElig;" #\?) xmls::*entities* :test #'equal))
  ;;setq xmls::*entities*
  ;;(adjoin '("sect;" #\#) xmls::*entities* :test #'equal))
  ;;setq xmls::*entities*
  ;;(adjoin '("nbsp;" #\Space) xmls::*entities* :test #'equal))
  (setq xmls::*entities*
	(concatenate 'vector
		     '(("AElig;" #\?)
		       ("sect;" #\#)
		       ("nbsp;" #\Space)
		       ("#8211;" #\-)
		       ("#8217;" #\')
		       ("#8219;" #\`)
		       ("#8220;" #\")
		       ("#8221;" #\")
		       ("#8230;" #\?))
		     xmls::*entities*))
  (with-open-file (s file)
   (let ((xml (xmls:parse s :compress-whitespace nil)))
     (xml-xform #'attr-list-to-assoc xml))))

(defun flatten-mostly (tree)
  "Similar to flatten, but keep the last level of list structure intact."
  (let ((acc nil))
    (labels ((rec (tree)
	       (cond ((null tree) nil)
		     ((or (atom tree)
			  (atom (car tree)))
		      (push tree acc))
		     (t (rec (car tree))
			(rec (cdr tree))))))
      (rec tree)
      (nreverse acc))))

(defun xml-subtrees (path tree)
  "Returns list of all subtrees matching path spec.
 Example: (xml-subtrees '(:body :h1) tree)"
  ;; FIXME: This should be simpler...
  (flatten-mostly
   (cond ((null tree) nil)
	 ((null path) tree)
	 (t (mapcar (lambda (c)
		      (xml-subtrees (cdr path) c))
		    (remove-if-not (lambda (c)
				     (and (consp c)
					  (eq (xml-elem c)
					      (car path))))
				   (xml-clst tree)))))))

(defun xml-subtree (path tree)
  "Returns first subtree matching path spec.
 Example: (xml-subtrees tree '(:body :h1))"
  (car (xml-subtrees path tree)))

(defun xhtml-get-body (tree)
  "Extract the body from an XHTML file."
  (xml-subtree tree '(:html :body)))

(defmacro appendq (var &rest lists)
  `(setf ,var (append ,var ,@lists)))

(defmacro append1 (var &rest elems)
  `(setf ,var (append ,var (list ,@elems))))

;; misc utilities

(defun remove-if-not-elems (elst clst)
  (remove-if-not (lambda (c)
		   (and (consp c)
			(member elst (xml-elem c))))
		 clst))

(defun remove-if-not-elem (elem clst)
  (remove-if-not (lambda (c)
		   (and (consp c)
			(eq elem (xml-elem c))))
		 clst))

;;(defun xmls::resolve-entity (ent)
;;  "Resolves the xml entity ENT to a character.  Numeric entities are
;;converted using CODE-CHAR, which only works in implementations that
;;internally encode strings in US-ASCII, ISO-8859-1 or UCS."
;;  (declare (type simple-base-string ent))
;;  (or (and (>= (length ent) 2)
;;           (char= (char ent 0) #\#)
;;           (code-char
;;            (min 255
;;		 (if (char= (char ent 1) #\x)
;;		     (parse-integer ent :start 2 :end (- (length ent) 1) :radix 16)
;;		     (parse-integer ent :start 1 :end (- (length ent) 1))))))
;;      (second (assoc ent xmls::*entities* :test #'string=))
;;      (warn "Unable to resolve entity ~S" ent)
;;      #\?))

(defun table-cell-p (c)
  (and (consp c)
       (eq (car c) 'cell)))

(defun column-count (rows)
  (iter (for row in rows)
	(maximize (count-if #'table-cell-p row))))


(defun calculate-column-widths (colspec rows)
  (print colspec)
  (mapcar (lambda (l)
	    (declare (ignorable l))
	    (/ 420
	       (column-count rows)))
	  rows))

;; The XHTML style sheet

(defun typeset-elem-xform (node parents)
  (let ((elem (xml-elem node))
	(attr (xml-attr node))
	(clst (xml-clst node)))

    ;; Deal with each element recursively.
    (case elem
      ((:html) (apply #'append (remove-if #'stringp clst)))
      
      ((:head) `((set-contextual-variable :title
		  ,(xml-extract-text (xml-subtree '(:title) node)))))
      
      ;; need to preserve :title for :head to work on, due to
      ;; depth-first search
      ((:title) node)

      ;; tricky elements that involve cross-reference handling
      
      ((:body)
       (if (> *toc-depth* 0)
	   (let ((toc (remove-if #'null (make-toc))))
	     `((set-contextual-variable :header-enabled t)
	       (set-contextual-variable :footer-enabled t)
	       (mark-ref-point '(:chapter 0) :data "Table of Contents")
	       ,@toc
	       :fresh-page
	       ,@clst
	       (mark-ref-point "DocumentEnd")))
	   `(,@clst
	     (mark-ref-point "DocumentEnd"))))
      
      ((:a)
       ;; FIXME: make links clickable
       (let ((name (xml-attr-get attr :name))
	     (href (xml-attr-get attr :href))
	     (out nil))
	 (if name (append1 out `(mark-ref-point ,name)))
	 (appendq out clst)
	 (if href
	     (append1 out
		      (if (eql #\# (aref href 0))
			  `(put-string (format nil " (page ~D)"
					(find-ref-point-page-number
					 ,(subseq href 1))))
			  `(with-style ()
			    " ("
			    (with-style (:color :blue)
			      (put-string ,href))
			    ")"))))
	 `(with-style () ,@out)))

      ((:h1) (chapter-markup 0 (xml-extract-text node) clst))
      ((:h2) (chapter-markup 1 (xml-extract-text node) clst))
      ((:h3) (chapter-markup 2 (xml-extract-text node) clst))
      ((:h4) (chapter-markup 3 (xml-extract-text node) clst))
      ((:h5) (chapter-markup 4 (xml-extract-text node) clst))
      ((:h6) (chapter-markup 5 (xml-extract-text node) clst))
      
      ((:p)
       `(paragraph (:font *font-normal* :font-size 10
		    :top-margin 3 :bottom-margin 4) ,@clst))

      ;; Table support is currently very limited
      ((:table) `(table (:col-widths
			 ',(calculate-column-widths (xml-attr-get attr :cols)
						    clst))
		  ,@clst))
      
      ((:tr) `(row () ,@clst))
      
      ((:td :th) (let* ((col-span (or (xml-attr-get attr :colspan) "1"))
			(row-span (or (xml-attr-get attr :rowspan) "1"))
			(align-s (xml-attr-get attr :align))
			(align (cond ((equal align-s "right") :right)
				     ((equal align-s "center") :center)
				     (t :left))))
		   `(cell (:col-span ,(parse-integer col-span)
			   :row-span ,(parse-integer row-span))
		     (paragraph (:h-align ,align) ,@clst))))
      
      ;; Ordered lists are a bit tricky, need to handle the item
      ;; numbering correctly. The following should support most
      ;; interesting parts of the XHTML spec.

      ((:ul)
       ;; FIXME: support different bullet styles
       `(itemize (:item-fmt "- "
		  :text-style (:top-margin 3 :bottom-margin 4))
	 ,@(remove-if-not-elem 'item clst)))
      
      ((:ol)
       (let* ((first (or (xml-attr-get attr :start) "1"))
	      (type (xml-attr-get attr :type))
	      (fmt (cond ((equal type "I") "~@R ")
			 ((equal type "i") "~(~@R~) ")
			 ((equal type "A") "~/tt::alpha-item/. ")
			 ((equal type "a") "~:/tt::alpha-item/. ")
			 (t "~D. "))))
	 `(itemize (:item-fmt ,fmt
		    :start-from ,(parse-integer first)
		    :text-style (:top-margin 3 :bottom-margin 4))
	   ,@(remove-if-not-elem 'item clst))))
      
      ((:li)
       `(item () ,@clst))
      
      ;; most elements are straightforward transformations
      
      ((:dl)
       `(with-style () ,@clst))
      
      ((:dt)
       `(paragraph (:font *font-bold* :bottom-margin 0)
	 ,@clst))
      
      ((:dd)
       `(paragraph (:top-margin 0 :left-margin 20 :bottom-margin 7)
	 ,@clst))
      
      ((:center)
       `(paragraph (:font *font-normal* :font-size 10
		    :top-margin 3 :bottom-margin 4
		    :h-align :center) ,@clst))
      
      ((:blockquote)
       `(paragraph (:font *font-normal* :font-size 10
		    :top-margin 3 :bottom-margin 4
		    :left-margin 20 :right-margin 20) ,@clst))
      
      ((:pre :code)
       `(with-style (:font *font-monospace* :font-size 9 :bottom-margin 0)
	 ,@(mapcar (lambda (c)
		     `(verbatim ,c))
		   clst)))
      
      ((:nobr)
       `(with-style () (hbox () ,@clst)))
      
      ((:br)
       :eol)
      
      ((:div :span)
       `(with-style () ,@clst))
      
      ((:i :em :var :address)
       ;; FIXME: can't handle bold-italic
       `(with-style (:font *font-italic*) ,@clst))
      
      ((:b :strong)
       ;; FIXME: can't handle bold-italic
       `(with-style (:font *font-bold*) ,@clst))
      
      ((:tt :kbd :samp)
       `(with-style (:font *font-monospace*) ,@clst))
      
      ((:big)
       `(with-style (:font-size (* *font-size* 1.2)) ,@clst))
      
      ((:small)
       `(with-style (:font-size (/ *font-size* 1.2)) ,@clst))
      
      ((:cite)
       `(with-style () ,@clst))
      
      ((:sub)
       `(with-subscript (:font-size (* 0.75 *font-size*)) ,@clst))
      
      ((:sup)
       `(with-superscript (:font-size (* 0.75 *font-size*)) ,@clst))
      
      ((:u)
       `(with-style (:post-decoration #'decoration-underline) ,@clst))
      
      ((:strike)
       `(with-style (:post-decoration #'decoration-strikethrough) ,@clst))
      
      ((:hr)
       `(hrule :dy 0.5))

      ;; change bar support
      
      ((:ins)
       `(with-style (:pre-decoration
		     #'decoration-green-background)
	 (change-start-insert)
	 ,@(if (verbatim-p parents)
	       (mapcar (lambda (c) `(verbatim ,c)) clst)
	       clst)
	 (change-end)))
      
      ((:del)
       `(with-style (:post-decoration
		     #'decoration-strikethrough)
	 (change-start-delete)
	 ,@(if (verbatim-p parents)
	       (mapcar (lambda (c) `(verbatim ,c)) clst)
	       clst)
	 (change-end)))

      ;; non-standard extension: unnested change start/stop markers.
      ;;
      ;; They need to be used pairwise (in tree depth-first order),
      ;; but do NOT need to be properly nested in relation to other
      ;; XHTML elements. This makes it much easier to generate diffs
      ;; with a non-XML-aware tool such as wdiff.
      ;;
      ;; Example:
      ;;   - This is <b>text</b>
      ;;   + This is some <b>bold text</b>
      ;; ->
      ;;     This is <ins-start />some <b>bold <ins-end />text</b>
      
      ((:ins-start)
       `(with-style ()
	 (set-contextual-style (:pre-decoration
			      #'decoration-green-background))
	 (change-start-insert)))
      
      ((:del-start)
       `(with-style ()
	 (set-contextual-style (:post-decoration
			       #'decoration-strikethrough))
	 (change-start-delete)))
      
      ((:ins-end :del-end)
       `(with-style ()
	 (set-contextual-style (:pre-decoration :none
			       :post-decoration :none))
	 (change-end)))

      ;; Unknown item: insert bright and ugly complaint
      
      (otherwise
       `(with-style (:color :red)
	 "[Unsupported: " ,(symbol-name elem) "]")))))

;;; high-level functions

(defun load-xml-file-xform (input)
  (xml-xform #'xml-collapse-whitespace
	     (xml-xform #'xml-collapse-sxml-namespace
			(load-xml-file input))))

(defun xhtml-to-typeset (input)
  "Read XML input file and transform to typesetting instructions"
  ;; First some cleanup on the input XML file
  (let ((tree (load-xml-file-xform input)))
    ;; Generate table of contents
    #-(and) (setq *chapters* (mapcar (lambda (h)
			       (xml-extract-text h))
			     (xml-subtrees '(:body :h1)
					   tree)))
    ;; The tree-to-tree transform
    (xml-xform #'typeset-elem-xform tree)))

(defun xhtml-to-pdf (input output)
  (typeset::render-document (xhtml-to-typeset input)
			    :file output
			    :twosided *twosided*))

;; following sections help in building a command line tool (based on
;; clisp) to convert HTML to PDF

#+clisp
(defun save-image ()
  (ext:gc)
  (ext:saveinitmem "clisp-xml-render.mem"
		   :init-function #'tt::run
		   :start-package (find-package :tt)))

;; gzip -9 clisp-xml-render.mem && mv clisp-xml-render.mem.gz ~/lisp/images/clisp/
;; ship with /usr/lib/clisp/full/lisp.run binary

#+clisp
(defun run ()
  (let ((args ext:*ARGS*))
    (when (equal "-x" (first args))
      (eval (read-from-string (second args)))
      (setq args (cddr args)))
    (if (eql 2 (length args))
	(apply #'xhtml-to-pdf args)
	(format *error-output* "~&Usage: html2pdf INPUT.html OUTPUT.pdf")))
  (ext:exit))

;; Test case:
;; (tt::xhtml-to-pdf "everything.html" "/tmp/output.pdf")