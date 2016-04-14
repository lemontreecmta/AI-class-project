Tue Vo
CS 356 project 3
README:

Assignment 1: Implement a genetic algorithm (GA) to generate from a random set of bitstrings, the bitstring with the maximum number of 1's.

Note: 1. I wrote a function which checks where the random number is on a list of items in population with their respective percentage. This function is brute-force and iterates from the start of the list until it reaches the target. 
2. To run different test, you need to reload the file to recreate a new population object. 

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
