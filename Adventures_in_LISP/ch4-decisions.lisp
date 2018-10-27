(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry) (setf *arch-enemy* 'studpid-lisp-alien')
			    '(curse you lisp alien -you ate my pudding))
	((eq person 'johnny) (setf *arch-enemy* 'useless-old johnny)
			     '(i hope you choked on my puddyng johnny))
	(t		'(why you eat tmy pudding stranger ?))))

