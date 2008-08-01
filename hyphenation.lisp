;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package #:typeset)

(defun hyphenate-string (string)
  (let ((min-word-size (+ cl-typesetting-hyphen::*left-hyphen-minimum* cl-typesetting-hyphen::*right-hyphen-minimum*)))
    (when (>= (length string) min-word-size)
      (loop
	  for prev-word-end = 0 then word-end
	  for word-start = (position-if #'alpha-char-p string :start prev-word-end)
	  for word-end = (when word-start (position-if-not #'alpha-char-p string :start word-start))
	  while word-end
	  when (>= (- word-end word-start) min-word-size)
	  nconc (mapcar #'(lambda (n) (+ word-start n))
			(cl-typesetting-hyphen::hyphen-find-hyphen-points
			 cl-typesetting-hyphen::*american-hyphen-trie* (subseq string word-start word-end)))))))

