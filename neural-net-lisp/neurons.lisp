;; Structure to hold the neural net being used
(defstruct (net (:print-function print-net))
  in-layer
  hid-layer
  out-layer
  in-set
  bias-nodes)

(defun print-net (struct out depth)
  (declare (ignore depth))
  (format out "Input Layer: ~%")
  (loop for in-node in (net-in-layer struct) do
    (format out "~A ~%" in-node))
  (format out "~%")
  (format out "Hidden Layer: ~%")
  (loop for hid-node in (net-hid-layer struct) do
    (format out "~A ~%" hid-node))
  (format out "~%")
  (format out "Output Layer: ~%")
  (loop for out-node in (net-out-layer struct) do
    (format out "~A ~%" out-node)))

;; Structure for representing each node
(defstruct (node (:print-function 
                  (lambda (struct out depth)
                    (declare (ignore depth))
                    (format out "Node: ~A~%Value:~A~%Input:~A~%Delta:~A~%Connections:~%~A~%"
                            (node-name struct)
                            (node-value struct)
                            (node-input struct)
                            (node-delta struct)
                            (node-connections struct)))))
  (name "No Name")
  (value 0)
  (input 0)
  (error 0)
  connections  ;;  a list of connections coming in to the current node
  (delta 0)
  (prev-delta 0))

(defun print-node (struct out depth)
  (declare (ignore depth))
  (format out "Node: ~A~%Value:~A~%Input:~A~%Delta:~A~%Connections:~A~%"
          (node-name struct)
          (node-value struct)
          (node-input struct)
          (node-delta struct)
          (node-connections struct)))

;; Structure to represent each connection
(defstruct (connection (:print-function 
                        (lambda (struct out depth)
                          (declare (ignore depth))
                          (format out "Connection: ~A to ~A~%Weight:~A~%" 
                                  (node-name (connection-from struct))
                                  (node-name (connection-to struct))
                                  (connection-value struct)))))
  from
  to
  (delta 0)
  (prev-delta 0)
  (value 0)
  (error 0)
  (prev-error 0))

;; createnetwork : listOf numbers -> structureOf network
(defun createnetwork (layers &optional bias) 
  (make-net 
   :in-layer (createnodes (first layers) 'input)
   :hid-layer (createnodes (second layers) 'hidden)
   :out-layer (createnodes (third layers) 'output)))

;; Create a number of nodes and return them in a list
;; createnodes : number, string -> listOf nodes
(defun createnodes (number name)
  (loop for i from 0 to (- number 1) collect 
    (make-node
     :name (format nil "~A of ~A" (+ 1 i) name)
     :value -1)))

