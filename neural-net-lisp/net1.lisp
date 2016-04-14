;;the structure of the neural network

;;create a set of connection (representing the architecture of the network)
;;initialize random weights for each connection
;;for each training example: 
;;1. a forward function to calculate the value of each node
;;2. a back propagation function to adjust the weights

(load "neurons.lisp")

(setq length-in 2)
(setq length-hid 2)
(setq length-out 1)
(setq net1 (createnetwork '(2 2 1))) ;create the first neural network

;;create a list of struct holding connections and initialize random weights for each connection
(setq connections-in-hid 
  (loop for i from 0 to (1- length-in) collect
  (loop for j from 0 to (1- length-hid) collect
    (make-connection
      :from (nth i (net-in-layer net1)) :to (nth j (net-hid-layer net1))
      :value (/ (random 100) 100)))))

(setq connections-hid-out 
  (loop for i from 0 to (1- length-hid) collect
  (loop for j from 0 to (1- length-out) collect
    (make-connection
      :from (nth i (net-hid-layer net1))
      :to (nth j (net-out-layer net1))
      :value (/ (random 100) 100))))) ;; initialize weights to random values

; sigmoid function
(defun sigmoid (a)
  (/ 1 (+ 1 (exp (- 0 a)))))

; deriv sigmoid function
(defun deriv-sigmoid(a)
  (* (sigmoid a) (- 1 (sigmoid a))))

;;training function, which takes into account training example and target
(defun train(obs tar)
  (list
;;put training example in the input layer
  (loop for i from 0 to (1- length-in) do
    (setf (node-value (nth i (net-in-layer net1))) (nth i obs))
  )

  ;;calculate the nodes of the hidden layers
  ;;input of the node of hidden layer
  (loop for i from 0 to (1- length-hid) do
    (setf (node-input (nth i (net-hid-layer net1)))  
        (reduce '+ (loop for j from 0 to (1- length-in) collect 
          (* (node-value (nth j (net-in-layer net1))) 
            (connection-value (nth i (nth j connections-in-hid))
            ))))))
  ;;value of the node of hidden layer
  (loop for i from 0 to (1- length-hid) do
    (setf (node-value (nth i (net-hid-layer net1)))
      (sigmoid (node-input (nth i (net-hid-layer net1))))))

  ;; calculate the nodes of the output layers
  ;input of the node of output layer
  (loop for i from 0 to (1- length-out) do
    (setf (node-input (nth i (net-out-layer net1)))  
        (reduce '+ (loop for j from 0 to (1- length-hid) collect 
          (* (node-value (nth j (net-hid-layer net1))) 
            (connection-value (nth i (nth j connections-hid-out))
            ))))))
  ;value of the node of output layer
  (loop for i from 0 to (1- length-out) do
    (setf (node-value (nth i (net-out-layer net1)))
      (sigmoid (node-input (nth i (net-out-layer net1))))))

  ;;2. a back propagation function to adjust the weights (from the connection list)
  ;; calculate the delta 
  (setq delta-out (loop for i from 0 to (1- length-out) collect
    (* (deriv-sigmoid (node-input (nth i (net-out-layer net1)))) ;;g'
      (- (nth i tar) ;; (y_j - a_j)
        (node-value (nth i (net-out-layer net1)))))))

  (setq sum-hid (loop for i from 0 to (1- length-hid) collect
    (reduce '+ (loop for j from 0 to (1- length-out) collect
      (* (nth j delta-out) (connection-value (nth j (nth i connections-hid-out)))
       )))))

  (setq delta-hid (loop for i from 0 to (1- length-hid) collect
    (* (deriv-sigmoid (node-input (nth i (net-hid-layer net1)))) ;;g'
      (nth i sum-hid)
      )))

  (setq sum-in (loop for i from 0 to (1- length-in) collect
    (reduce '+ (loop for j from 0 to (1- length-hid) collect
      (* (nth j delta-hid) (connection-value (nth j (nth i connections-in-hid)))
       )))))

  (setq delta-in (loop for i from 0 to (1- length-in) collect
    (* (deriv-sigmoid (node-input (nth i (net-in-layer net1)))) ;;g'
      (nth i sum-in)
      )))

  (setq decay (/ 1 50)) ;;set decay rate = 0.02

  ;;readjust the weights connecting hidden and output layers
  (loop for i from 0 to (1- length-hid) do
    (loop for j from 0 to (1- length-out) do
      (setf (connection-value (nth j (nth i connections-hid-out)))
        (+ (connection-value (nth j (nth i connections-hid-out)))
          (* decay (* (node-value (nth i (net-hid-layer net1))) (nth j delta-out)))))))
  
  ;;readjust the weights connecting input and hidden layers
  (loop for i from 0 to (1- length-in) do
    (loop for j from 0 to (1- length-hid) do
      (setf (connection-value (nth j (nth i connections-in-hid)))
        (+ (connection-value (nth j (nth i connections-in-hid)))
          (* decay (* (node-value (nth i (net-in-layer net1))) (nth j delta-hid)))))))
))


;;training examples: 

(setq obs1 '(1 1))
(setq obs2 '(0 1))
(setq obs3 '(1 0))
(setq obs4 '(0 0))
(setq tar1 '(0))
(setq tar2 '(1))
(setq tar3 '(1))
(setq tar4 '(0))

(defun train-net-1 (times)
    (loop for i from 1 to times collect
     (list
      (train obs1 tar1) (setq delta1 (abs (reduce '+ (loop for i from 0 to (1- length-out) collect 
        (- (nth i tar1) ;; (y_j - a_j)
        (node-value (nth i (net-out-layer net1))))))))
      (train obs2 tar2) (setq delta2 (abs (reduce '+ (loop for i from 0 to (1- length-out) collect 
        (- (nth i tar2) ;; (y_j - a_j)
        (node-value (nth i (net-out-layer net1))))))))
      (train obs3 tar3) (setq delta3 (abs (reduce '+ (loop for i from 0 to (1- length-out) collect 
        (- (nth i tar3) ;; (y_j - a_j)
        (node-value (nth i (net-out-layer net1))))))))
      (train obs4 tar4) (setq delta4 (abs (reduce '+ (loop for i from 0 to (1- length-out) collect 
        (- (nth i tar4) ;; (y_j - a_j)
        (node-value (nth i (net-out-layer net1))))))))
      (setq delta-list (list delta-list (/ (+ delta1 delta2 delta3 delta4) 4)))
      (format t "For the ~D th training, error rate is ~D~%" i (/ (+ delta1 delta2 delta3 delta4) 4))
    ))
)

