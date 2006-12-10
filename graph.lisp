;;; cl-typesetting/cl-typegraph copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

;(defparameter *dot-command* "dot -Tps ~s -o ~s")
(defparameter *dot-command* "dot")
(defparameter *dot-command-args* '("-Tplain-ext"))
(defparameter *graph-file-prefix* "/tmp/")
(defparameter *arrow-width* 2)
(defparameter *arrow-length* 6)
(defparameter *edge-label-font* (pdf:get-font "helvetica"))
(defparameter *edge-label-font-size* 9)
(defparameter *node-label-font* (pdf:get-font "helvetica"))
(defparameter *node-label-font-size* 12)

(defvar *graph-id-counter* 0)

(defun make-graph-node-id ()
  (format nil "N~d." (incf *graph-id-counter*)))

(defun make-graph-file-id ()
  (format nil "F~d" (incf *graph-id-counter*)))

(defclass graph-node ()
  ((id :accessor id :initform (make-graph-node-id))
   (data :accessor data :initarg :data :initform nil)
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (background-color :accessor background-color :initarg :background-color :initform '(1.0 1.0 1.0))
   (border-color :accessor border-color :initarg :border-color :initform '(0.0 0.0 0.0))
   (border-width :accessor border-width :initarg :border-width :initform 1)
   (shape :accessor shape :initarg :shape :initform :box)
   (x :accessor x :initform 0)
   (y :accessor y :initform 0)
   (dx :accessor dx :initarg :dx :initform 60)
   (dy :accessor dy :initarg :dy :initform 15)))

(defclass graph-edge ()
  ((label :accessor label :initarg :label :initform nil)
   (head :accessor head :initarg :head)
   (tail :accessor tail :initarg :tail)
   (direction :accessor direction :initarg :direction :initform :forward)
   (data :accessor data :initarg :data :initform nil)
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (color :accessor color :initarg :color :initform '(0.0 0.0 0.0))
   (width :accessor width :initarg :width :initform 1)
   (label-color :accessor label-color :initarg :color :initform '(0.0 0.0 0.0))
   (label-x :accessor label-x)
   (label-y :accessor label-y)
   (points :accessor points :initform ())))

;;; dot-attributes is a list of (attribute value) pairs ex: ("rankdir" "LR")

(defclass graph ()
  ((nodes :accessor nodes :initform (make-hash-table :test #'equal))
   (edges :accessor edges :initform (make-hash-table :test #'equal))
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (rank-constraints :accessor rank-constraints :initform nil)
   (background-color :accessor background-color :initarg :background-color :initform '(1.0 1.0 1.0))
   (border-color :accessor border-color :initarg :border-color :initform '(0.0 0.0 0.0))
   (border-width :accessor border-width :initarg :border-width :initform 1)
   (landscape-layout :accessor landscape-layout :initarg :landscape-layout :initform nil)
   (max-dx :accessor max-dx :initarg :max-dx :initform 400)
   (max-dy :accessor max-dy :initarg :max-dy :initform 400)
   (scale :accessor scale :initform 1)
   (dx :accessor dx)
   (dy :accessor dy)))

(defmethod initialize-instance :after ((node graph-node) &rest args
				       &key fixed-height fixed-width graph &allow-other-keys)
  (unless (and fixed-width fixed-height)
    (adjust-graph-node-size node (data node) fixed-width fixed-height))
  (when graph (add-node graph node)))

(defmethod initialize-instance :after ((edge graph-edge) &rest args &key graph &allow-other-keys)
  (when graph (add-edge graph edge)))

(defun add-node (graph node)
  (setf (gethash (id node) (nodes graph)) node))

(defun add-edge (graph edge)
  (setf (gethash (cons (head edge)(tail edge)) (edges graph)) edge))

(defun add-rank-constraint (graph constraint nodes)
  (push (cons constraint nodes) (rank-constraints graph)))

(defmethod adjust-graph-node-size ((node graph-node) data fixed-width fixed-height)
  (unless fixed-width
    (setf (dx node) (+ (pdf::text-width (format nil "~a" data) *node-label-font* *node-label-font-size*) 4)))
  (unless fixed-height
    (setf (dy node) (+ *node-label-font-size* 4))))

(defmethod adjust-graph-node-size ((node graph-node) (box box) fixed-width fixed-height)
  (unless fixed-height
    (setf (dy node) (+ (compute-boxes-natural-size (boxes box) #'dy) 4))))

(defun gen-dot-attributes (s attributes &optional comma)
  (loop for (attribute value) in attributes do
	(if comma
	    (write-string ", " s)
	    (setf comma t))
	(write-string attribute s)
	(write-char #\= s)
	(write-line value s)))

(defmethod gen-graph-dot-data ((graph graph) s)
  (format s "digraph G {
size=\"~a,~a\";
edge [fontname=~a,fontsize=~a];
"
	  (/ (max-dx graph) 72.0)(/ (max-dy graph) 72.0)
	  (pdf:name *edge-label-font*) *edge-label-font-size*)
  (loop for (rank-constraint . nodes) in (rank-constraints graph) do
	(format s "{rank = ~a; ~{~s;~^ ~}};~%" rank-constraint (mapcar 'id nodes)))
  (format s "graph [")
  (gen-dot-attributes s (dot-attributes graph))
  (format s "];")
  (iter (for (id node) in-hashtable (nodes graph))
	(gen-graph-dot-data node s))
  (iter (for (id edge) in-hashtable (edges graph))
	(gen-graph-dot-data edge s))
  (format s "}~%"))

(defmethod gen-graph-dot-data ((node graph-node) s)
  (format s "~s [shape=~a, fixedsize=true, width=~a, height=~a"
	  (id node)(shape node)(/ (dx node) 72.0)(/ (dy node) 72.0))
  (gen-dot-attributes s (dot-attributes node) t)
  (format s "];~%"))

(defmethod gen-graph-dot-data ((edge graph-edge) s)
  (format s "~s -> ~s [label=\"~a\", arrowhead=none"
	  (id (head edge)) (id (tail edge))
	  (if (label edge) (label edge) ""))
  (gen-dot-attributes s (dot-attributes edge) t)
  (format s "];~%"))

(defun read-graph-line-values (string)
  (when string
    (let ((*package* (find-package :keyword)))
      (iter (for position first 0 then end)
	    (for (values object end) = (read-from-string string nil nil :start position))
	    (while object)
	    (collect object)))))

(defun process-graph-line (graph values)
  (setf (scale graph)  (first values)
	(dx graph)(* (second values) (scale graph) 72.0)
	(dy graph)(* (third values) (scale graph) 72.0)))

(defun process-graph-node-line (graph values)
  (let ((node (gethash (pop values) (nodes graph))))
    (setf (x node) (* (pop values) 72.0)
	  (y node) (* (pop values) 72.0))))

(defun process-graph-edge-line (graph values)
  (let* ((head (gethash (pop values) (nodes graph)))
	 (tail (gethash (pop values) (nodes graph)))
	 (edge (gethash (cons head tail) (edges graph)))
	 (nb-points (pop values)))
    (setf (points edge) (iter (repeat nb-points)
			      (collect (* (pop values) 72.0))
			      (collect (* (pop values) 72.0))))
    (when (label edge)
      (pop values)
      (setf (label-x edge) (* (pop values) 72.0)
	    (label-y edge) (* (pop values) 72.0)))))

;;; this should be changed to use pipes instead of files and adapted to other Lisp implementations.
(defun compute-graph-layout (graph)
  (let* ((file-id (make-graph-file-id))
	 (dot-file (concatenate 'string *graph-file-prefix* file-id ".dot"))
	 (result-file  (concatenate 'string *graph-file-prefix* file-id ".txt")))
    (unwind-protect
	 (progn
	   (with-open-file (s dot-file :direction :output :if-exists :supersede)
	     (gen-graph-dot-data graph s))
#+lispworks (sys:call-system (format nil "~a~{ ~s~} ~s -o ~s" *dot-command* *dot-command-args* dot-file result-file) :wait t)
#+(or cmu sbcl) (ext:run-program *dot-command* `(,@*dot-command-args* ,dot-file "-o" ,result-file) :wait t)
	   (with-open-file (s result-file :direction :input)
	     (iter (for line = (read-line s nil))
		   (while line)
		   (for (line-type . values) = (read-graph-line-values line))
		   (case line-type
		     (:node (process-graph-node-line graph values))
		     (:edge (process-graph-edge-line graph values))
		     (:graph (process-graph-line graph values))
		     (:stop (finish))))))
      (progn (ignore-errors (delete-file dot-file))
	     (ignore-errors (delete-file result-file))))))

(defun graph-box (graph &rest args)
  (let ((dx (dx graph))
	(dy (dy graph)))
    (when (landscape-layout graph)
      (rotatef dx dy))
    (add-box (apply 'make-instance 'user-drawn-box
		    :stroke-fn #'(lambda(box x y)
				   (if (landscape-layout graph)
				       (pdf:with-saved-state
					   (pdf:translate x (- y dy))
					   (pdf:rotate 90)
					   (stroke graph 0 0))
				       (stroke graph x y)))
		    :inline t :dx dx :dy dy
		    :allow-other-keys t args))))

(defmethod stroke ((graph graph) x y)
  (pdf:with-saved-state
      (pdf:set-color-fill (background-color graph))
      (when (border-width graph)
	(pdf:set-color-stroke (border-color graph))
	(pdf:set-line-width (border-width graph))
	(pdf:basic-rect x y (dx graph)(- (dy graph)))
	(pdf:fill-and-stroke))
      (pdf:translate x (- y (dy graph)))
      (pdf:scale (scale graph)(scale graph))
      (iter (for (id edge) in-hashtable (edges graph))
	    (stroke-edge edge (data edge)))
      (iter (for (id node) in-hashtable (nodes graph))
	    (stroke-node node (data node)))))

(defmethod stroke-node ((node graph-node) data)
  (pdf:with-saved-state
      (pdf:set-color-fill (background-color node))
      (when (border-width node)
	(pdf:set-color-stroke (border-color node))
	(pdf:set-line-width (border-width node))
	(pdf:basic-rect (- (x node)(* (dx node) 0.5))(+ (y node)(* (dy node) 0.5))(dx node)(- (dy node)))
	(pdf:fill-and-stroke)))
  (stroke-node-content node data))

(defmethod stroke-node-content ((node graph-node) data)
  (when data
    (pdf:set-color-fill '(0.0 0.0 0.0))
    (pdf:draw-centered-text (x node)(- (y node) (* 0.3 *node-label-font-size*))
			    (format nil "~a" data)
			    *node-label-font* *node-label-font-size*)))

(defmethod stroke-node-content ((node graph-node) (box box))
  (stroke box (- (x node)(* (dx node) 0.5)) (+ (y node)(* (dy node) 0.5))))

(defmethod stroke-edge ((edge graph-edge) data)
  (pdf:with-saved-state
      (pdf:set-color-stroke (color edge))
      (pdf:set-color-fill (color edge))
      (pdf:set-line-width (width edge))
      (let ((points (points edge))
	    x1 y1 x2 y2 x3 y3 prev-x1 prev-y1)
	(pdf:move-to (pop points)(pop points))
	(iter (while points)
	      (setf prev-x1 x1 prev-y1 y1)
	      (setf x1 (pop points) y1 (pop points)
		    x2 (pop points) y2 (pop points)
		    x3 (pop points) y3 (pop points))
	      (pdf:bezier-to x1 y1 x2 y2 x3 y3))
	(pdf:stroke)
	(when (eq (direction edge) :forward)
	  (let* ((nx (- x1 x3))
		 (ny (- y1 y3))
		 (l (sqrt (+ (* nx nx)(* ny ny))))
		 x0 y0)
	    (when (zerop l)
	      (setf nx (- prev-x1 x3) ny (- prev-y1 y3))
	      (setf l (sqrt (+ (* nx nx)(* ny ny)))))
	    (setf nx (/ nx l) ny (/ ny l))
	    (pdf:move-to x3 y3)
	    (setf x0 (+ x3 (* nx *arrow-length*)) y0 (+ y3 (* ny *arrow-length*))
		  nx (* nx *arrow-width*) ny (* ny *arrow-width*))
	    (pdf:line-to (+ x0 ny)(- y0 nx))
	    (pdf:line-to (- x0 ny)(+ y0 nx))
	    (pdf:line-to x3 y3)
	    (pdf:fill-and-stroke)))
	(when (label edge)
	  (pdf:set-color-fill (label-color edge))
	  (pdf:draw-centered-text (label-x edge)(label-y edge)(label edge)
				  *edge-label-font* *edge-label-font-size*)))))
