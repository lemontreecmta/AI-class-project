
(load "auxfns.lisp")  
(load "search.lisp")  
(load "move-generator.lisp")

;;;;;;;;;;;;;;;;;;;;;;PRINT DEPTH AND NODES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-tree-search (states goal-p successors count nodes combiner)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (list 'goal (first states) 'depth count 'nodes nodes)) ;;return base case
        (t (my-tree-search
             (funcall combiner
                      (funcall successors (first states))
                      (rest states))
             goal-p successors (1+ count) (+ nodes (length (funcall successors (first states)))) combiner))))

(defun dfs (start goal-p successors count nodes)
  "Search new states first until goal is reached."
  (my-tree-search (list start) goal-p successors count nodes #'append))

(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))

(defun is (value) #'(lambda (x) (eql x value)))

(defun prepend (x y) "Prepend y to start of x" (append y x))

(defun bfs (start goal-p successors count nodes)
  "Search old states first until goal is reached."
  (my-tree-search (list start) goal-p successors count nodes #'prepend))

(defun best-fs (start goal-p successors count nodes cost-fn)
  "Search lowest cost states first until goal is reached."
  (my-tree-search (list start) goal-p successors count nodes (sorter cost-fn)))

;;;;;;;;;;;;;;;;;;;;;;BOUNDED DFS;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-bound-tree-search (states goal-p successors count nodes bound combiner)
  "Using boundary tree search"
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
	((equal bound 0) fail)
        ((funcall goal-p (first states)) (list 'goal (first states) 'depth count 'nodes nodes)) ;;return base case	
        (t (my-bound-tree-search
             (funcall combiner
                      (funcall successors (first states))
                      (rest states))
             goal-p successors (1+ count) (+ nodes (length (funcall successors (first states)))) (1- bound) combiner))))

(defun bound-dfs (start goal-p successors count nodes bound)
  "Search new states first until goal is reached or boundary is exceeded."
  (my-bound-tree-search (list start) goal-p successors count nodes bound #'append))

(defun iterative-deepening (start goal-p successors count nodes start-depth)
"iteratively increase bound of dfs until finding results"
 (let ((a (bound-dfs start goal-p successors count nodes start-depth)))
	(if (null a) (iterative-deepening start goal-p successors count nodes (1+ start-depth)) 
	a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TEST

(defvar easy '(1 3 4 8 6 2 7 0 5))
(defvar medium '(2 8 1 0 4 3 7 6 5))
(defvar hard '(2 8 1 4 6 3 0 7 5))
(defvar worst '(5 6 7 4 0 8 3 2 1))
(defvar goal '(1 2 3 8 0 4 7 6 5))

;(print (dfs easy (my-is goal) 'next-stage 0 1))
;(print (bound-dfs easy (my-is goal) 'next-stage 0 1 10)) ;use debug: search to see path, else the majority of time it returns nil because the depth is not enough for searching
;(print (bfs easy (my-is goal) 'next-stage 0 1))
;(print (best-fs easy (my-is goal) 'next-stage 0 1 (normal-dist goal)))
;(print (best-fs easy (my-is goal) 'next-stage 0 1 (manhattan-dist goal)))
;(print (iterative-deepening easy (my-is goal) 'next-stage 0 1 1))

(print (time (best-fs easy (my-is goal) 'next-stage 0 1 (normal-dist goal))))
(print (time (best-fs medium (my-is goal) 'next-stage 0 1 (normal-dist goal))))
(print (time (best-fs hard (my-is goal) 'next-stage 0 1 (normal-dist goal))))
(print (time (best-fs worst (my-is goal) 'next-stage 0 1 (normal-dist goal))))
