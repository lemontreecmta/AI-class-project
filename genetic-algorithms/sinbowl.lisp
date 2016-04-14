;The Algorithms

;randomly initialize population(t)
;determine fitness of population(t)

;repeat
  ;select parents from population(t) randomly according to a fitness function distribution; calculate fitness 
  ;perform crossover on parents creating population(t+1) 70/30
  ;perform mutation of population(t+1) bitwise mutation rate = 0.001 (choose a random number between 1 and 1000, mutate if = 1)
  ;determine fitness of population(t+1)

(load "ga.lisp")

;;create an incremental percent list, for example, (1 2 3 4 5 6) --> (1 3 6 10 15 21)
;;take linear time
(defun incre-list-2 (l)
	(incre-2 l '()))
(defun incre-2 (l add-l)
	(if (null l) add-l
	  (if (null add-l) (incre-2 (cdr l) (list (car l)))
		(incre-2 (cdr l) (append add-l (list (+ (car l) (car (reverse add-l)))))))))
;;get a list of fitness from population using sinbowl function
(defun fit-vector(population)
	(incre-list-2 (mapcar #'eval-sinbowl population)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;GENERATING PARENTS;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;get a random subset of the population
(defun random-subset (population)
	(loop for i from 1 to 10 collect ;;subset = 10
		(nth (random (length population)) population)
	))

;;get the index of the min item according to eval-sinbowl
(defun min-index (sub-population)
	(let ((index 0) (value (eval-sinbowl (car sub-population))))
	(loop for i from 1 to 9 do
		(if (< (eval-sinbowl (nth i sub-population)) value) (list (setf index i) (setf value (eval-sinbowl (nth i sub-population)))))
		) index ))

;;generate one parent by getting the min from one sub-population
(defun generate-parent(population)
	(let ((sub-population (random-subset population)))
	(nth (min-index sub-population) sub-population)))

;;;;;;;;;;;;;;;;
;;CROSSOVER;;;;;
;;;;;;;;;;;;;;;;
;; function to perform cross over, return a list, can add a parameter representing crossover rate
(defun crossover(parent1 parent2) ;;cross over percent
  (append (subseq parent1 0 25) (subseq parent2 25))) ;;crossover rate 0.5

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;COMPLETE TEST;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;create a new population of generation x+1 from population of generation x
(defun new-generation(population)
	(loop for i from 1 to 50 collect
	(mutate (crossover (generate-parent population) (generate-parent population)) 100))) ;;mutation rate 1/100

(setq population (generate-strings '(0 1) 50 50))

(defun test(population iter)
	(loop for i from 1 to iter collect
		(list
			(setq population (new-generation population)) 
			(format t "For the ~D th training, the number which minimizes sinbowl is ~D~%" i  (binary-to-num(nth (min-index population) population)))
	))))

