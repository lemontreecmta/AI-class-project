README

Implementation of neural networks in LISP

The structure of the neural network

Create a set of connection (representing the architecture of the network)
Initialize random weights for each connection
For each training example: 
1. a forward function to calculate the value of each node
2. a back propagation function to adjust the weights

How to run: Run (train-net-1 number-of-times) with number of times as an integer
