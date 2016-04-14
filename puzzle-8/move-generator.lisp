;Tue Vo
;CS 356

;Design a representation for the 8-puzzle problem and functions to: 1) generate the legal moves and 2) apply an operator to a given state.  
;Implement the given search algorithms and test them on the start states and goal state shown below;  
;in each case, measure and display the number of nodes examined and the total time required to solve the puzzle, as well as the sequence of moves to solve the problem.\

(load "auxfns.lisp")  ;; must be in same directory
(load "search.lisp")  ;; ditto
;(debug :search)       ;; optional, comment out to trace back

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;GENERATING NEW LEGAL MOVES

;;function my-replace: replace two items in list. input: a list; output, the new list
;;example: (my-replace 0 1 '(1 2 3)) --> '(2 1 3)

(defun my-replace(index1 index2 list)
"replace values of two index in list (count from 0), return the new list"
 (let ((copy-list (clone list)))
 (let ((a (nth index1 copy-list)))
 (list (setf (nth index1 copy-list) (nth index2 copy-list)) 
 (setf (nth index2 copy-list) a)) copy-list )))

;;function clone: return a shallow copy of list. input: a list; output, a clone of the list
;;example: (clone '(1 2 3)) --> '(1 2 3)

(defun clone(list)
"create a shallow copy of list"
(if (null list) nil
 (cons (car list) (clone (cdr list)))))

;;function next-stage: return next possible stages of 8-puzzle game. Input: a list of current state, output: a list of possible new states
;;example: (next-stage '(0 1 2 3 4 5 6 7 8)) --> '((1 0 2 3 4 5 6 7 8) (3 1 2 0 4 5 6 7 8))

(defun next-stage(list) 
"return the next possible legal moves"
    (let ((x (position '0 list)))
    (if (equal x 0) (list (my-replace 0 1 list) (my-replace 0 3 list))  ;;if blank is at position 0
    (if (equal x 1) (list (my-replace 1 0 list) (my-replace 1 2 list) (my-replace 1 4 list)) ;; at 1
    (if (equal x 2) (list (my-replace 2 1 list) (my-replace 2 5 list))
    (if (equal x 3) (list (my-replace 3 0 list) (my-replace 3 4 list) (my-replace 3 6 list))
    (if (equal x 4) (list (my-replace 4 1 list) (my-replace 4 3 list) (my-replace 4 5 list) (my-replace 4 7 list))
    (if (equal x 5) (list (my-replace 5 2 list) (my-replace 5 4 list) (my-replace 5 8 list))
    (if (equal x 6) (list (my-replace 6 3 list) (my-replace 6 7 list))
    (if (equal x 7) (list (my-replace 7 4 list) (my-replace 7 6 list) (my-replace 7 8 list))
    (if (equal x 8) (list (my-replace 8 5 list) (my-replace 8 7 list))
)))))))))))

;;test cases (comment out the comment out to test)
;(defparameter sample '(0 1 2 3 4 5 6 7 8))
;(print (my-replace 0 1 sample))
;(print (my-replace 0 3 sample))
;(print (next-stage sample))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EVALUATE LIST EQUALITY

;;Evaluate equality for a list

(defun my-is (value) 
"evaluate equality for list"
#'(lambda (x) (equal x value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;COST FUNCTION: difference

;;function difference: return the number of dissimilar items in their respective position of two lists. Input: 2 lists; output: a number
;;example: (difference '(1 2 3) '(2 1 3)) --> 2

(defun difference(state1 state2)
;;return the number of items which are not their positions
    (if (null state1) 0
     (if (eql (car state1) (car state2)) 
     (difference (cdr state1) (cdr state2)) (+ 1 (difference (cdr state1) (cdr state2))) 
)))

(defun normal-dist(state1)
#'(lambda (state2)
	(difference state1 state2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;COST FUNCTION: MANHATTAN DISTANCE

;;function add-coor: add coordinate to each item in list.  
;;example: (add-coor (0 1 2 3 4 5 6 7 8) 1 1 3) --> ((0 1 1) (1 2 1) (2 3 1) (3 1 2) (4 2 2) (5 2 3) (6 3 1) (7 3 2) (8 3 3))
;;parameters: list, values of coordinates x and y, limit of how much x and y can expand

(defun add-coor(list x y lim) 
   (if (null list) '()
     (if (eql x lim) 
	(cons (list (car list) x y) (add-coor (cdr list) 1 (+ 1 y) lim))
     (cons (list (car list) x y) (add-coor (cdr list) (+ 1 x) y lim)) 
)))

;;function my-sort: sort the new list of list according to the first item of the list within the list. Input: a list of lists; output: the new sorted order of the list of lists
;;examples: (my-sort '((2 0 0) (3 0 0) (1 0 0))) --> '((1 0 0) (2 0 0) (3 0 0))

(defun my-sort(list)
"sort inner lists according to first items"
    (sort list #'< :key #'car) )

;;function manhattan: calculate the distance given the returned list in add-coor, with the second and third items in each inner lists as the coordinates x and y
;;example: (manhattan '(1 0 0) '(2 1 1)) --> 2
;; input: two lists (from add-coor), output: the sum of manhattan distances

(defun manhattan(coor1 coor2) 
"calculate the given add-coored distance between list1 and list2"
   (if (null coor1) 0
     (+ (abs (- (nth 1 (car coor1)) (nth 1 (car coor2)) ) ) 
	(abs (- (nth 2 (car coor1)) (nth 2 (car coor2)) ) )
	(manhattan (cdr coor1) (cdr coor2)))
))

;;getting all together, calculate the manhattan distance of two lists
;;function manhattan dist: calculate the manhattan dist of two given states
;;example: (manhattan-dist '(1 2) '(2 1)) --> 2
;;input: two lists; output: a number which is the sum of manhattan distances

(defun manhattan-dist(list1)
#'(lambda (list2)  
"calculate the given manhanttan distance given two states"
(manhattan (my-sort (add-coor list1 1 1 3 ) ) 
           (my-sort (add-coor list2 1 1 3 ) ) ) ) )

;;test manhattan dist

;(defvar easy '(1 3 4 8 6 2 7 0 5))
;(defvar medium '(2 8 1 0 4 3 7 6 5))
;(defvar hard '(2 8 1 4 6 3 0 7 5))
;(defvar worst '(5 6 7 4 0 8 3 2 1))
;(defvar goal '(1 2 3 8 0 4 7 6 5))
;(print (add-coor easy 1 1 3) )
;(print (my-sort (add-coor easy 1 1 3) ) )
;(print 'ok)
;(print (manhattan (my-sort (add-coor easy 1 1 3)) (my-sort (add-coor goal 1 1 3)) ) )
;(print (manhattan-dist easy goal))
;(print (manhattan-dist medium goal))
;(print (manhattan-dist worst goal))
;(print 'manhattan-dist-done)
;;;;





