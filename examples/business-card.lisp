;;; cl-typesetting copyright 2003-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package typeset)

;;; Some business cards examples written during a mail exchange 
;;; with Peter Seibel on the cl-typesetting mailing list
;;; There is also a business card example with a pdf generated logo at the end

(defpackage :business-card (:use :cl :typeset))

(in-package :business-card)

(defconstant +points-per-inch+ 72)

(defun business-card1 (&optional (file #P"/tmp/card1.pdf"))
  (let ((width (* 3.5d0 +points-per-inch+))
        (height (* 2d0 +points-per-inch+)))
    (pdf:with-document ()
      (pdf:with-page ()
        (let ((center-block
               (compile-text ()
                 (paragraph (:h-align :center :font "Times-Italic" :font-size 8)
                    "You can hire a billion monkeys or you can hire me." :eol
                    :vfill
                    (with-style (:font "Helvetica" :font-size 12)
                      "John Smith" :eol)
                    (with-style (:font "Helvetica" :font-size 11)
                      "CL-Typesetting Consulting" :eol)
                    :vfill
                    (with-style (:font "Helvetica" :font-size 8)
                      (format-string "john@cl-typesetting.com ~c www.cl-typesetting.com" (code-char 127))) :eol
                      (format-string "16384 Lisp Avenue, Oakland, California, 94618 ~c 429.496.7296" 
                                     (code-char 127))))))
	  (pdf:translate 50 650)
          (typeset::draw-block center-block 0 height width height :v-align :fill :border 1)))
      (pdf:write-document file))))

(defun business-card2 (&optional (file #P"/tmp/card2.pdf"))
  (let ((width (* 3.5d0 +points-per-inch+))
        (height (* 2d0 +points-per-inch+)))
    (pdf:with-document ()
      (pdf:with-page ()
        (let ((center-block
               (compile-text ()
                 (paragraph (:h-align :center :font "Times-Italic" :font-size 8)
                   "You can hire a billion monkeys or you can hire me." :eol
		   :vfill
		   (with-style (:font "Helvetica" :font-size 12)
		     "John Smith" :eol)
		   (with-style (:font "Helvetica" :font-size 11)
		     "CL-Typesetting Consulting" :vfill :vfill))))
              (left-block
	       (compile-text ()
		     (paragraph (:h-align :left :font "Helvetica" :font-size 8) 
				"16384 Lisp Avenue" :eol
				"Okaland, California" :eol
				"94618")))
              (right-block
	       (compile-text ()
                  (paragraph (:h-align :right :font "Helvetica" :font-size 8)
			     "john@cl-typesetting.com" :eol
			     "www.cl-typesetting.com" :eol
			     "429.496.7296"))))
	  (pdf:translate 50 650)
          (typeset::draw-block left-block 0 height width height :v-align :bottom :border 1)
          (typeset::draw-block center-block 0 height width height :v-align :fill)
          (typeset::draw-block right-block 0 height width height :v-align :bottom)))
      (pdf:write-document file))))

(defun business-card3 (&optional (file #P"/tmp/card3.pdf"))
  (let ((width (* 3.5d0 +points-per-inch+))
        (height (* 2d0 +points-per-inch+)))
    (pdf:with-document ()
      (pdf:with-page ()
        (let ((center-block
               (compile-text ()
                 (paragraph (:h-align :center :font "Times-Italic" :font-size 8)
                   "You can hire a billion monkeys or you can hire me." :eol
		   :vfill
		   (with-style (:font "Helvetica" :font-size 12)
		     "John Smith" :eol)
		   (with-style (:font "Helvetica" :font-size 11)
		     "CL-Typesetting Consulting" :vfill))
		 (paragraph (:h-align :fill :font "Helvetica" :font-size 8)
			    "16384 Lisp Avenue" :hfill "john@cl-typesetting.com" :eol
			    "Okaland, California" :hfill "www.cl-typesetting.com" :eol
			    "94618" :hfill "429.496.7296"))))
	  (pdf:translate 50 650)
          (typeset::draw-block center-block 0 height width height :v-align :fill :border 1)))
      (pdf:write-document file))))



(defun draw-fc-logo (x y size)
  (pdf:with-saved-state
    (pdf:translate x y)
    (pdf:scale (* 0.01 size) (* 0.01 size))
    (pdf:set-rgb-stroke 1.0 1.0 1.0)
    (pdf:set-rgb-fill 0.0 0.3 0.58)
    (pdf:set-line-width 0.2)
    (loop for ((x0 y0)(x1 y1)(x2 y2)) in '(((20.025 55.573) (50 3.655) (79.975 55.573)) 
                                           ((28.2 58.72) (50 96.479) (71.8 58.72)) 
                                           ((56.63 9.162) (78.43 46.92) (100.23 9.162)) 
                                           ((-0.23 9.162) (21.57 46.92) (43.37 9.162)))
       do (pdf:move-to x0 y0)
          (pdf:line-to x1 y1)
          (pdf:line-to x2 y2)
          (pdf:fill-and-stroke))))

(defun draw-fc-card (x y width height)
   (pdf:with-saved-state
     (pdf:translate x y)
     (let ((center-block
            (compile-text ()
              (paragraph (:h-align :left :font "Helvetica" :font-size 25 :color '(0.0 0.3 0.58))
                         (typeset::vspace 12)
                         (with-style (:font "Helvetica")  
                           (typeset::hspace 60) "fractal concept" :eol
                           :vfill))
              (paragraph (:h-align :center :font "Helvetica" :font-size 12)
                         (with-style (:font "Helvetica" :font-size 13)
                           "Marc Battyani" :eol)
                         (with-style (:font "Helvetica-Oblique" :font-size 8)
                           "Chief Technology Officer" :eol
                           "Directeur Recherche & Développement" :vfill))
              (typeset::vspace 5)
              (paragraph (:h-align :fill :left-margin 3 :right-margin 3 :font "Helvetica" :font-size 7)
                         "marc.battyani@fractalconcept.com" :hfill "Tel: 01 60 39 53 40" :eol
                         "3, av St Marc - 77850 Héricy - France" :hfill "www.fractalconcept.com" :eol
                         (typeset::vspace 3)))))
       (typeset::draw-block center-block 0 height width height :v-align :fill :border 0 :padding 0))
     (draw-fc-logo 7 97 45)))
  
(defun fc-card (&optional (file #P"/tmp/fc-card.pdf"))
  (let ((width 241) ;255
        (height 153)) ;150
    (pdf:with-document ()
      (pdf:with-page ()
        (pdf:set-rgb-stroke 1.0 1.0 1.0)
        (pdf:set-rgb-stroke 0.0 0.0 0.0)
        (pdf:set-line-width 0.1)
        (pdf:set-rgb-stroke 1.0 1.0 1.0)
        (loop for x from 45 below 400 by (+ width 28) do
              (loop for y from 38 below 700 by height do
                   (draw-fc-card x y width height)
#+nil                   (pdf:basic-rect x y width height)
                   (pdf:stroke))))
      (pdf:write-document file))))
