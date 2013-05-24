#!/usr/bin/sbcl

(defun printNextMove ()
  (let*
    ((pid (read))
     (vars1 (list (read)(read)(read)(read)(read)(read)(read)))
     (vars2 (list (read)(read)(read)(read)(read)(read)(read)))
     (mvars (if (= pid 1) vars1 vars2))
     (ovars (if (= pid 1) vars2 vars1))
     (mscore (car mvars))
     (mholes (cdr mvars))
     (oscore (car ovars))
     (oholes (cdr ovars)))
    (print pid)))

(defun opphole (move)
  (if
    (<= move 6)
    (+ 2 (* 2 (- 6 move)))
    (+ 1 (* 2 (- 13 move)))))

(defun holeref (move mvars ovars)
  (cond
   ((<= move 6) (nth move mvars))
   ((> move 7) (nth (- move 7) ovars))
   (t (car mvars))))

(defmacro nthhole (n)
  (if (and (>= n 0) (<= n 12))
  `(nth ,(1- n) (eval '(append mholes (cons mscore oholes))))
  0))

(defun holesEmpty (vars)
  (= 0 (nth 1 vars) (nth 2 vars) (nth 3 vars) (nth 4 vars) (nth 5 vars) (nth 6 vars)))

(defun moveMarbles (turn vars1 vars2 move)
  (let*
      ((turn1 (= turn 1))
       (mvars (if turn1 vars1 vars2))
       (ovars (if turn1 vars2 vars1))
       (holes (append mvars (list (car mvars)) (cdr ovars)))
       (marbles (nth move mvars)))
      (progn
	(setf (nth move holes) 0)
	(loop
	 (if (<= marbles 0) (return))
	 (setf move (1+ (mod move 13)))
	 (setf (nth move holes) (1+ (nth move holes)))
	 (setf marbles (1- marbles)))
	(if (/= move 7) (setf turn (if turn1 2 1)))
	(if (and
	     (>= move 1)
	     (<= move 6)
	     (= (nth move holes) 1))
	    (progn
	      (setf (nth move holes) (1+ (nth (opphole move) holes)))
	      (setf (nth (opphole move) holes) 0)))
	(setf (car holes) (nth 7 holes))
	(setf mvars (butlast holes 7))
	(setf ovars (cons (car ovars) (nthcdr 8 holes)))
	(list turn (if turn1 mvars ovars) (if turn1 ovars mvars)))))
	 
	 
(defun doPlay (ind1 ind2 turn vars1 vars2)
  (let*
      ((turn1 (= turn 1))
       (cplayer (if turn1 ind1 ind2))
       (mvars (if turn1 vars1 vars2))
       (ovars (if turn1 vars2 vars1)))
    (cond
     ((or (holesEmpty vars1) (holesEmpty vars2)) (let ((s1 (car vars1)) (s2 (car vars2)))
						   (cond
						    ((< s1 s2) -1)
						    ((= s1 s2) 0.5)
						    ((> s1 s2) 1)
						    (t -1000))))
     (t
      (let
	((move (returnNextMove cplayer mvars ovars)))
	(cond
	 ((or
	   (< move 1)
	   (> move 6)
	   (= (nth move mvars) 0))
	  (cond
	   ((= turn 1) -1)
	   (t 1)))
	(t
	 (let
	   ((modvars (moveMarbles turn vars1 vars2 move)))
	   (doPlay ind1 ind2 (car modvars) (cadr modvars) (caddr modvars))))))))))


(defun play (ind1 ind2)
  (doPlay ind1 ind2 1 (list 0 4 4 4 4 4 4) (list 0 4 4 4 4 4 4)))

(defun returnNextMove (ind mvars ovars)
  (let
      ((mscore (car mvars))
       (mholes (cdr mvars))
       (oscore (car ovars))
       (oholes (cdr ovars)))
    (funcall ind mscore mholes oscore oholes)))


(defun mutate (obj) (let ((rate 0.1))
		       (cond
			   ((<= (random 1.0) rate)
			    (repl obj))
			   ((atom obj) obj)
			   (t (cons (car obj)
				    (recursive ((obj (cdr obj)))
					       (cond
						((null obj) NIL)
						(t (cons (mutate (car obj)) (recurse (cdr obj)))))))))))
				

(defun newbool ()
  (let
      ((r (random 100)))
    (cond
     ((or (> r 8) (= r 0)) 't)
     ((= r 1) (list 'not (newbool)))
     ((= r 2) (list 'and (newbool) (newbool)))
     ((= r 3) (list 'or (newbool) (newbool)))
     ((= r 4) (list '<= (newint) (newint)))
     ((= r 5) (list '>= (newint) (newint)))
     ((= r 6) (list '= (newint) (newint)))
     ((= r 7) (list '/= (newint) (newint)))
     ((= r 8) (list 'if (newbool) (newbool) (newbool))))))

(defun notint (n)
  (let (test (newint))
    (if
	(and (numberp test) (integerp test) (/= test n))
	test
      (notint n))))

(defun newint ()
  (let
      ((r (random 50)))
    (cond
     ((or (> r 5) (= r 0)) (random 20))
     ((= r 1) (list 'nthhole (newint)))
     ((= r 2) (list 'if (newbool) (newint) (newint)))
     ((= r 3) (list 'mod (newint) (notint 0)))
     ((= r 4) 'mscore)
     ((= r 5) 'oscore))))

(defun atomtype (obj)
  (cond
   ((or
     (and (numberp obj) (integerp obj))
     (eq obj 'mscore)
     (eq obj 'oscore))
    'integer)
   (t 'bool)))

(defun listtype (obj)
  (let
      ((first (car obj)))
    (cond
     ((or
       (eq first 'and)
       (eq first 'or)
       (eq first '<=)
       (eq first '>=)
       (eq first '>)
       (eq first '<)
       (eq first '=)
       (eq first '/=))
      'bool)
     ((eq first 'nth) 'integer)
     ((eq first 'if) (gettype (nth 2 obj)))
     ((eq first 'cond) (gettype (cadr (cadr obj))))
     ((eq first 'let) (gettype (caddr obj))))))
     
(defun gettype (obj)
  (cond
   ((atom obj) (atomtype obj))
   ((listp obj) (listtype obj))))

(defun repl (obj)
  (cond
   ((null obj) obj)
   (t (newexp (gettype obj)))))

(defun newexp (type)
  (cond
   ((eq type 'integer) (newint))
   ((eq type 'bool) (newbool))))
   
(defun makeind (obj)
  (eval (list 'lambda '(mscore mholes oscore oholes) obj)))
  

(defun test (obj tests)
  (let ((fitsum 0) (testn (list-length tests)) (ind1 (makeind obj)))
    (progn
      (loop for i from 0 to (1- testn) do
	  (setf fitsum (+ fitsum
		     (play ind1 (makeind (nth i tests))))))
      (/ fitsum testn))))

(defun makeobjs (n)
  (cond
   ((= n 0) NIL)
   (t (cons (list (newint) 0) (makeobjs (1- n))))))

(defun evolve (objs n)
  (let*
      ((tests (mapcar #'car objs))
       (objn (list-length objs))
       (mutant (mutate (car (nth (random objn) objs))))
       (worst (let ((worst (list (cadar objs) 0))) (progn (loop for i from 1 to (1- objn) do (let ((test (list (cadr (nth i objs)) i))) (setf worst (if (< (car test) (car worst)) test worst)))) worst)))
       (mutscore (test mutant tests)))
    (progn
      (if (> mutscore (car worst))
	(setf (nth (cadr worst) objs) (list mutant mutscore)))
      (if (> n 0) (evolve objs (1- n)) objs))))

(defun start (population iterations)
  (evolve (makeobjs population) iterations))
    
