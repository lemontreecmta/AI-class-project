;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS FOR GENETIC ALGORITHM ASSIGNMENTS
;; Original code by Michael Vollmer
;; Converted from Scheme (Racket) -> Lisp by spc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun range (end)
  (reverse (range-r end)))

;; range-r : num
;; helper function for range
(defun range-r (end)
  (if (zerop end)
      '()
      (cons end (range-r (- end 1)))))

;; shuffle : listOf a -> listOf a
;; shuffles a single list to get a new permutation
;; e.g. (shuffle '(1 2 3)) -> (3 1 2)
(defun shuffle (list)
  (let ((len (length list)))
    (loop repeat len
      do
        (rotatef
          (nth (random len) list)
          (nth (random len) list))
      finally
        (return list))))

;; generate-strings: listOf a, int, int -> listOf (listOf a))
;; generates a population of num (somewhat random) bitstrings each of length size
;; Ex: (generate-strings '(0 1) 5 2) -> ((1 0 1 1 1) (0 1 0 0 1))  
(defun generate-strings (domain size num)
  (mapcar (lambda (x) 
            (mapcar (lambda (x) 
                      (car (shuffle domain))) (range size)))
          (range num)))

;; binary-to-num: listof {0,1} -> int
;; converts a bitstring (in list representation) to the equivalent base-10 integer
;; e.g. (binary-to-num (generate-strings '(0 1) 5 2) -> (23 9)
(defun binary-to-num (lst)
  (binary-to-num-r 0 0 (reverse lst)))
  
;; binary-to-num-r: int int listOf {0,1} -> int
;; helper function for binary-to-num
(defun binary-to-num-r (i sum lst)
  (cond ((null lst) sum)
        ((zerop (car lst)) (binary-to-num-r (+ i 1) sum (cdr lst)))
        (t (binary-to-num-r (+ i 1) (+ sum (expt 2 i)) (cdr lst)))))

;; EXPERIMENTAL using higher-order fcns to improve efficiency
(defun listify (lst) lst)
(defun binary-to-num-me (lst)
  (reduce (lambda (x y) (+ y (* x 2))) (listify lst)))

;; SINBOWL FUNCTIONS
;; Your code typically will use eval-sinbowl and won't necessarily ever have
;; to call the other two directly. Note that the possible binary values encoded
;; in a bitstring 0 - 1023 are scaled to the range -60 to 120 before evaluation

(defun sinbowl (value)
  (- (* (abs value) 0.1) (sin value)))
 
(defun scale-num (x min max a b)
  (+ (/ (* (- b a) (- x min)) (- max min)) a))

;; eval-sinbowl : bitstring -> real
;; evaluates the sinbowl function at the value given by bitstring str
(defun eval-sinbowl (str)
  (sinbowl (scale-num (binary-to-num str) 0 1023  -60 120)))
 
 
;; TESTS & EXAMPLES OF USE

;; BINARY-TO-NUM STRESS TEST - run the following multiple times
;; with both larger and smaller numbers of generated strings
;; (sort (mapcar #'binary-to-num (generate-strings '(0 1) 10 200)) #'<)

;; EVAL-SINBOWL
(eval-sinbowl '(1 0 0 1 1 1 0)) ;; should be 5.3776774

(mapcar #'eval-sinbowl (generate-strings '(0 1) 10 200))



