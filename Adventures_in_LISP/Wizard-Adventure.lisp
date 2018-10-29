;; Locations
(defparameter *nodes* '((living-room (you are in the living-room. 
					  a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				     there is a well in front of you.))
			(attic (you are in the attic.
				    there is a giant welding torch in the corner.))))
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; Paths
(defparameter *edges* '((living-room (garden west door)
				     (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))
(defun describe-path (edge)
  `(there is a, (caddr edge) going, (cadr edge) from here.)) ;why (`)?
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; Objects
(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
		     (eq (cadr (assoc obj obj-locs)) loc))) ;local function defintion at-loc-p
  (remove-if-not #'at-loc-p objs)))
(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))

;; Describe Everything
(defparameter *location* 'living-room) ;players start out in living room
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; Actions
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
  (if next 
    (progn (setf *location* (car next)) 
	   (look)) ;multiple consequences
    '(you cannot go that way.))))
(defun pickup (object)
  (cond ((member object
		 (object-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*) ;add new location, body, to the object's location list.
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; Repl
(defun game-repl()
  (let ((cmd (game-read))) ;store user command in cmd
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read() ;turns user input into commands
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")")))) ;wrap input with ()
    (flet ((quote-it (x)
		     (list 'quote x))) ;add quote to commands arguments
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory)) ;game-eval only commands
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst ;alter until reach last char
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit))) ;move on, don't alter if space
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit))) ;detect start of sentence, don't alter caps
	    ((eql item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit))) ;treat as literal, don't alter caps.
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst)) ;convirt symbols to string, no output
				     'list) ;coercing string to list
			     t
			     nil) ;tweaking text
		 'string)) ;back to string
  (fresh-line))


