;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

(defvar *reference-table* nil)
(defvar *undefined-references* nil)
(defvar *changed-references* nil)
(defvar *contextual-variables* nil)

(defclass ref-point ()
  ((id :accessor id :initform nil :initarg :id)
   (located-pass :accessor located-pass :initform nil)
   (data :accessor data :initform nil :initarg :data)
   (page-content :accessor page-content :initform nil :initarg :page-content)
   (page-number :accessor page-number :initform 999)
   (x :accessor x :initform nil)
   (y :accessor y :initform nil)))

(defmethod located-pass (obj)
  (declare (ignore obj))
  nil)

(defmethod stroke ((ref-point ref-point) x y)
  (when (and (located-pass ref-point)
	     (/= pdf:*page-number* (page-number ref-point)))
    (push (id ref-point) *changed-references*))
  (when (page-content ref-point)
    (setf (page-content ref-point) pdf:*page*))
  (setf (located-pass ref-point) *current-pass*
	(page-number ref-point) pdf:*page-number*
	(x ref-point) x
	(y ref-point) y))

(defun mark-ref-point (id &rest args
		       &key (type 'ref-point)
		       &allow-other-keys)
  (let* ((ref-point (gethash id *reference-table*)))
    (when (and ref-point (not (located-pass ref-point)))
      (error "Reference ~s is already defined " id))
    (unless ref-point
      (remf args :type)
      (setf ref-point (apply #'make-instance type :id id args))
      (setf (gethash id *reference-table*) ref-point))
    (add-box ref-point)))

(defun find-ref-point (id)
  (let ((ref-point (gethash id *reference-table*)))
    (unless (located-pass ref-point)
      (pushnew id *undefined-references*))
    ref-point))

(defun find-ref-point-page-number (id)
  (let ((ref-point (find-ref-point id)))
    (if (located-pass ref-point)
	(page-number ref-point)
	999)))

(defun find-ref-point-page-content (id)
  (let ((ref-point (find-ref-point id)))
    (if (located-pass ref-point)
	(page-content ref-point)
	nil)))

(defun find-ref-point-page-data (id &optional default)
  (let ((ref-point (find-ref-point id)))
    (if (located-pass ref-point)
	(data ref-point)
	default)))

(defclass contextual-action ()
  ((action-fn :accessor action-fn :initform nil :initarg :action-fn)))

(defmethod stroke ((action contextual-action) x y)
  (declare (ignorable x y))
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

