;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package typeset)

(defvar *reference-table* nil)
(defvar *previous-reference-table* nil)
(defvar *undefined-references* nil)
(defvar *changed-references* nil)
(defvar *contextual-variables* nil)

(defclass ref-point ()
  ((id :accessor id :initform nil :initarg :id)
   (located-p :accessor located-p :initform nil)
   (data :accessor data :initform nil :initarg :data)
   (page-number :accessor page-number :initform 999)
   (x :accessor x :initform nil)
   (y :accessor y :initform nil)))

(defmethod located-p (obj)
  nil)

(defmethod stroke ((ref-point ref-point) x y)
  (let ((previous-ref (and *previous-reference-table*
			   (gethash (id ref-point) *previous-reference-table*)))
	(page-number pdf:*page-number*))
    (when (and previous-ref (/= page-number (page-number previous-ref)))
      (push (id ref-point) *changed-references*))
    (setf (located-p ref-point) t
	  (page-number ref-point) page-number
	  (x ref-point) x
	  (y ref-point) y)))

(defun mark-ref-point (id &rest args &key (type 'ref-point))
  (when (gethash id *reference-table*)
    (error "Reference ~s redefined" id))
  (remf args :type)
  (let* ((ref-point (apply #'make-instance type :id id args)))
    (setf (gethash id *reference-table*) ref-point)
    (add-box ref-point)))

(defun find-ref-point (id)
  (let ((ref-point (gethash id *reference-table*)))
    (unless (located-p ref-point)
      (setf ref-point (and *previous-reference-table* (gethash id *previous-reference-table*))))
    (unless (located-p ref-point)
      (pushnew id *undefined-references*))
    ref-point))

(defun find-ref-point-page-number (id)
  (let ((ref-point (find-ref-point id)))
    (if (located-p ref-point)
	(page-number ref-point)
	999)))

(defun find-ref-point-page-data (id &optional default)
  (let ((ref-point (find-ref-point id)))
    (if (located-p ref-point)
	(data ref-point)
	default)))

(defclass contextual-action ()
  ((action-fn :accessor action-fn :initform nil :initarg :action-fn)))

(defmethod stroke ((action contextual-action) x y)
  (when (action-fn action)
    (funcall (action-fn action))))

(defun add-contextual-action (action-fn)
  (add-box (make-instance 'contextual-action :action-fn action-fn)))

(defun set-contextual-variable (var-id value)
  (add-contextual-action
   #'(lambda ()
       (let ((previous (assoc var-id *contextual-variables*)))
	 (if previous
	     (setf (cdr previous) (list value))
	     (push (list var-id value) *contextual-variables*))))))

(defun get-contextual-variable (var-id &optional default)
  (let ((previous (assoc var-id *contextual-variables*)))
    (if (and previous (>= (length previous) 2))
	(second previous)
	default)))

(defun push-contextual-variable (var-id value)
  (add-contextual-action
   #'(lambda ()
       (let ((previous (assoc var-id *contextual-variables*)))
	 (if previous
	     (push value (cdr previous))
	     (push (list var-id value) *contextual-variables*))))))

(defun pop-contextual-variable (var-id &optional default)
  (let ((previous (assoc var-id *contextual-variables*)))
    (if (and previous (>= (length previous) 2))
	(pop (cdr previous))
	default)))

