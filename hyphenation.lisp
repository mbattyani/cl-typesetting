;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package #:typeset)

(defparameter *default-hyphen-language* :american)

(defun hyphenate-string (string &optional (language *default-hyphen-language*))
  "Builds a list of hyphenation points.
With optional second argument `LANGUAGE', use hyphenation data for
that language."
  (let ((min-word-size (+ cl-tt-hyph::*left-hyphen-minimum*
			  cl-tt-hyph::*right-hyphen-minimum*))
	(hyphen-language (cl-tt-hyph:language-hyphenation language)))
    (when (>= (length string) min-word-size)
      (loop
	 for prev-word-end = 0 then word-end
	 for word-start = (position-if #'alpha-char-p string :start prev-word-end)
	 for word-end = (when word-start (position-if-not #'alpha-char-p string :start word-start))
	 while word-end
	 when (>= (- word-end word-start) min-word-size)
	 nconc (mapcar #'(lambda (n) (+ word-start n))
		       (cl-tt-hyph::hyphen-find-hyphen-points
			hyphen-language
			(subseq string word-start word-end)))))))
