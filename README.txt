My code for projects in Artificial Intelligence CS 356 - Spring 2015. 

Projects include:

1. Differentiation with LISP.

2. Feed-forward neural network construction using LISP

The structure of the neural network

Create a set of connection (representing the architecture of the network)
Initialize random weights for each connection
For each training example: 
1. a forward function to calculate the value of each node
2. a back propagation function to adjust the weights

How to run: Run (train-net-1 number-of-times) with number of times as an integer

Error rate: Error rate = sum|predict result - actual result|/ number of training examples

Result: After 2 training times (each consists of going through all four training examples), error rates are 0.497.


3. Genetic algorithms (in /genetic-algorithms/)

Assignment 1: Implement a genetic algorithm (GA) to generate from a random set of bitstrings, the bitstring with the maximum number of 1's.

How-to-run: To run, load "all1.lisp" and call (test population num-of-iter) (for example, for 20 iterations, call (test population 20))
Parameter: Population size = 200, length = 100, cross-over rate 0.7, bitwise mutation rate 0.001 
Result: (it takes a long time so I don't run 20) Test for 5 generations, run for 5 times: Highest fit is 65, 65, 66, 64, 66. Mean = 65.2

Assignment 2: Repeat the previous experiment with crossover turned off (pc=0.0), recording the same information. 

How-to-run: To run, load "all1.lisp" and call (test-no-mutate population num-of-iter)
Result: Test for 5 generations, run for 5 times: Highest fit is 70, 67, 67, 67, 67. Mean = 67.6 
Observation: Population without mutation has better fit, though the fit doesn't increase very significantly over generations. 

Assignment 3: Implement a GA that minimizes the sinbowl function (that is, it finds the function value at the deepest part of the bowl). The sinbowl function is defined as: f(x)=0.1⋅|x|-sin(x)

How-to-run: To run, load "sinbowl.lisp" and call (test population num-of-iter)
Parameter: Population size = 50, length = 50, cross-over rate 0.5, bitwise mutation rate 0.01, size of tournament selection = 10 
Result: When I call (test population 50) 5 times, the number steadily decreases and ends up at 385, 352, 349, 527, 349.
