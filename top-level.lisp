;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

;;; Toplevel document and page layout, auto splitting
;;; Thanks to Dmitri Ivanov for this!

(in-package #:typeset)

(defconstant* +paper-sizes+	; In portrait orientation: (width . height) 
  '((:A3 . (841 . 1190))	; (841.89 . 1190.55)
    (:A4 . (595 . 841))		; (595.28 . 841.89)
    (:A5 . (420 . 595))		; (420.94 . 595.28)
    (:Letter . (612 . 792))
    (:Legal . (612 . 1008))))

(defvar *default-page-size* :A4)
(defvar *default-page-orientation* :portrait)		; :portrait or :landscape
(defvar *default-page-header-footer-margin* 30)

(defun compute-page-bounds (&optional (size *default-page-size*)
                                      (orientation *default-page-orientation*))
 ;;; Compute media box size
  ;; Args: size  Size identifier or (width . height)
  (let* ((pair (unless (consp size) (cdr (assoc size +paper-sizes+))))
         (width (cond ((consp size) (car size))
                      ((eq orientation :landscape) (or (cdr pair) 841))
                      ((or (car pair) 595))))
         (height (cond ((consp size) (cdr size))
                       ((eq orientation :landscape) (or (car pair) 595))
                       ((or (cdr pair) 841)))))
    (vector 0 0 width height)))

(defclass page (pdf::page)
 ((margins :accessor margins :initarg :margins :initform nil)	; :type quad
  (header :accessor header :initarg :header :initform nil)
  (footer :accessor footer :initarg :footer :initform nil)
  (header-top :initarg :header-top :initform nil)
  (footer-bottom :initarg :footer-bottom :initform nil)
  (finalize-fn :initarg :finalize-fn :initform nil)		; signature: page
  ;; dy left unallocated on this page
  (room-left :accessor room-left :initarg :room-left :initform 0)
))

(defun remove-properties (plist keys)
  (loop for (key value) on plist by #'cddr
        unless (member key keys) nconc (list key value)))

(defun draw-pages (content &rest args
                   &key (size *default-page-size*)
                        (orientation *default-page-orientation*)
                        bounds margins
                        (header-top *default-page-header-footer-margin*)
                        (footer-bottom *default-page-header-footer-margin*)
                        break
		        finalize-fn
                        &allow-other-keys)
 ;;; Args:
  ;;	content		Text content, multi-page-table, or other content.
  ;;	bounds  	Media box; overwrites size and orientation when specified.
  ;;	margins		Quad of distances between media edges and page body area.
  ;;			(independent from header and footer sizes for now).
  ;;	header, footer	Content or function of ftype (function (page) content)
  ;;	header-top	Distance between the top media edge and the header.
  ;;	footer-bottom	Distance between the bottom media edge and the footer.
  ;;	break   	Force new page ::= :before | :after | :always (both ends)
  (with-quad (left-margin top-margin right-margin bottom-margin) margins
   (let* ((bounds (or bounds (compute-page-bounds size orientation)))
          (height (aref bounds 3)))
    (flet ((add-page ()
             (setq pdf:*page* (apply #'make-instance 'page
                                     :bounds bounds
                                     :header-top header-top
                                     :footer-bottom footer-bottom
                                     ;; Move room-left into initialize-instance :after?
                                     :room-left (- height top-margin bottom-margin)
				     :finalize-fn finalize-fn
                                     (remove-properties args
                                       '(:size :orientation :bounds
                                         :header-top :footer-bottom :break))))))
      (when (and pdf:*page* (member break '(:before :always)))
        (finalize-page pdf:*page*)
        (setq pdf:*page* nil))
      (loop with width = (aref bounds 2)
            with dx = (- width left-margin right-margin)
            and x = left-margin
            with dy and y
            while (boxes-left content)
            unless pdf:*page* 
              do (add-page)
            do (setq dy (room-left pdf:*page*)
                     y  (+ dy bottom-margin))
            when (<= dy +epsilon+)
              do (finalize-page pdf:*page*)
                 (add-page)
                 (setq dy (room-left pdf:*page*)
                       y  (+ dy bottom-margin))
            do
            (handler-bind
                ((end-of-page
                  #'(lambda (c &aux restart)
                      (cond ((setq restart (find-restart 'continue-with-next-page c))
                             (finalize-page pdf:*page*)
                             (add-page)
                             (setq dy (room-left pdf:*page*)
                                   y  (+ dy bottom-margin))
                             (invoke-restart restart y))
                            ((loop-finish))))))
              (multiple-value-bind (boxes dy-left) (fit-lines content dx dy :top)
                (cond (boxes
                       (let ((vbox (make-instance 'vbox  :boxes boxes  :dx dx  :dy dy 
                                                  :fixed-size t)))
                         (do-layout vbox)
                         (setf (room-left pdf:*page*) dy-left)
                         (stroke vbox x y)))
                      ;; As no new lines can fit, check whether the page was just started
                      ((> (abs (- dy (- height top-margin bottom-margin))) +epsilon+)
                       (finalize-page pdf:*page*)
                       (setq pdf:*page* nil))
                      ;; Cannot fit even on a comletely fresh page
                      (t (error 'cannot-fit-on-page :box (first (boxes-left content))))
      )    ) ) )
      (when (and pdf:*page* (member break '(:after :always)))
        (finalize-page pdf:*page*)
        (setq pdf:*page* nil))
) )))

(defun finalize-page (pdf:*page* &optional (get-content t))
 ;;; Draw header and footer without advancing their content,
  ;; then obtain the entire page content stream.
  (with-slots (margins header header-top footer footer-bottom finalize-fn) pdf:*page*
    (with-quad (left-margin top-margin right-margin bottom-margin) margins
      (let* ((width (aref (pdf::bounds pdf:*page*) 2))
             (height (aref (pdf::bounds pdf:*page*) 3))
             (dx (- width left-margin right-margin)))
         (when header
           (let ((content (if (functionp header) (funcall header pdf:*page*) header)))
             (pdf:with-saved-state
              (stroke (cond ((typep content 'box)
                             content)
                            (content
                             (make-filled-vbox content dx (- top-margin header-top)
                                              :top nil)))
                       left-margin (- height header-top)))))
         (when footer
           (let ((content (if (functionp footer) (funcall footer pdf:*page*) footer)))
             (pdf:with-saved-state
              (stroke (cond ((typep content 'box)
                             content)
                            (content
                             (make-filled-vbox content dx (- bottom-margin footer-bottom)
                                              :bottom nil)))
                       left-margin bottom-margin))))))
    (when finalize-fn
      (funcall finalize-fn pdf:*page*))
    (when get-content
      (setf (pdf::content (pdf::content-stream pdf:*page*))
	    (get-output-stream-string pdf::*page-stream*))
      (pdf::compress-pdf-stream (pdf::content-stream pdf:*page*))))
  pdf:*page*)

(defmethod draw-block (content x y dx dy 
                       &key border (padding 5) rotation (v-align :top) special-fn)
 ;;; On the current *page*
  (pdf:with-saved-state
    (pdf:translate x y)
    (when rotation
      (pdf:rotate rotation))
    (when border
      (with-quad (left top right bottom) padding
        (pdf:set-line-width border)
        (pdf:set-gray-stroke 0)
        (pdf:set-gray-fill 1)
        (pdf:basic-rect (- left) top (+ dx left right) (- (+ dy top bottom)))
        (pdf:fill-and-stroke)
        (pdf:set-gray-fill 0)
    ) )
    (let ((vbox (make-filled-vbox content dx dy v-align)))
      ;(push vbox *boxes*)
      (when special-fn
        (funcall special-fn vbox 0 0))
      (stroke vbox 0 0))))

(defun final-pass-p ()
  (or (= *current-pass* *max-number-of-passes*)
      (not (or *undefined-references* *changed-references*))))

(defmacro with-document ((&rest args) &body body)
  `(let ((*reference-table* (make-hash-table :test #'equal))
	 (*undefined-references* t)
	 (*changed-references* nil)
	 (*contextual-variables* nil))
    (loop for *current-pass* from 1 to *max-number-of-passes*
          while (or *undefined-references* *changed-references*)
	  do
	  (setf *undefined-references* nil
		*changed-references* nil
		*contextual-variables* nil)
	  (pdf:with-document (,@args)
	    (let ((pdf::*page-stream* (make-string-output-stream)))
	      (declare (dynamic-extent pdf::*page-stream*))
		,@body)))))

(defun write-document (filename &optional (document pdf:*document*))
  (when (final-pass-p)
    (pdf:write-document filename document)))

