;The Algorithms

;randomly initialize population(t)
;determine fitness of population(t)

;repeat
  ;select parents from population(t) randomly according to a fitness function distribution; calculate fitness 
  ;perform crossover on parents creating population(t+1) 70/30
  ;perform mutation of population(t+1) bitwise mutation rate = 0.001 (choose a random number between 1 and 1000, mutate if = 1)
  ;determine fitness of population(t+1)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;CALCULATE FITNESS;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fit(l)
  (reduce '+ l))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;GENERATING PARENTS;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;create an incremental percent list, for example, (1 2 3 4 5 6) --> (1 3 6 10 15 21)
;;take quadratic time
(defun incre-list (l)
  (reverse (incre (reverse l))))
(defun incre (reverse-l)
	(if (null reverse-l) '() 
  (cons (reduce '+ reverse-l) (incre (cdr reverse-l)))))

;;take linear time
(defun incre-list-2 (l)
	(incre-2 l '()))
(defun incre-2 (l add-l)
	(if (null l) add-l
	  (if (null add-l) (incre-2 (cdr l) (list (car l)))
		(incre-2 (cdr l) (append add-l (list (+ (car l) (car (reverse add-l)))))))))

;;get a list of fitness from population
;;(fit-vector '((1 1 0) (0 0 1) (0 1 0))) --> (2 1 1)
(defun fit-vector(population)
	(incre-list-2 (mapcar #'fit population)))

;;find out the closest smaller index of number in l 
;; (where '(100 200 300 400 500) 265) --> 1
(defun where(l number)
	(where-index l number 0))
(defun where-index(l number count)
	(if (null l) '()
	   (if (< number (car l)) count (where-index (cdr l) number (1+ count)))))

;;use a random number to pick 
;(defun pick-an-item(population)
;	)
;;generate a parent
;;example: (generate-parent '((1 0) (1 1) (0 1) (0 0) (1 1) (0 1))) should generate mostly (1 0) or (1 1)
(defun generate-parent(population)
	(nth (where (fit-vector population) (random (car (reverse (fit-vector population))))) population))

;;;;;;;;;;;;;;;;
;;CROSSOVER;;;;;
;;;;;;;;;;;;;;;;
;; function to perform cross over, return a list, can add a parameter representing crossover rate
(defun crossover(parent1 parent2) ;;cross over percent
  (append (subseq parent1 0 70) (subseq parent2 70)))

;;;;;;;;;;;;;;;;
;;;MUTATION;;;;;
;;;;;;;;;;;;;;;;
;;flip
(defun flip(num)
  (if (eql num 0) 1 0))
;;mutation 
;;(0 0 1 1 0 0), 6 --> (1 0 1 1 0 0) or something else
(defun mutate(l chance-pct)
  (mapcar #'(lambda (x) (if (eql 0 (random chance-pct)) (flip x) x)) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;ONLY FOR Q1: CHECK IF THE GENERATION HAS STRING FULL OF 1;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-100(list)
	(if (null list) 0 
		(if (eql 100 (car list)) 1 (check-100 (cdr list)))))
;;return 1 if the generation has string full of 1, else return 0
(defun check(population)
	(check-100 (mapcar #'fit population)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;COMPLETE TEST;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ex: (generate-strings '(0 1) 5 2) -> ((1 0 1 1 1) (0 1 0 0 1))  

;;create a new population of generation x+1 from population of generation x
(load "ga.lisp")
(defun new-generation(population)
	(loop for i from 1 to 200 collect
	(mutate (crossover (generate-parent population) (generate-parent population)) 1000))) 
(defun new-generation-no-mutate(population)
	(loop for i from 1 to 200 collect
	(crossover (generate-parent population) (generate-parent population)))) 

(setq population (generate-strings '(0 1) 100 200))

(defun test(population iter)
	(loop for i from 1 to iter collect
		(list
			(setq population (new-generation population)) 
			(format t "For the ~D th training, highest fit is ~D~%" i (reduce 'max (mapcar #'fit population)))
	)))

(defun test-no-mutate(population iter)
	(loop for i from 1 to iter collect
		(list
			(setq population (new-generation-no-mutate population)) 
			(format t "For the ~D th training, highest fit is ~D~%" i (reduce 'max (mapcar #'fit population)))
	)))
;;record
;;1st: 3; 2nd: > 20; 2nd, 2

