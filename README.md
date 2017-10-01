# Remote Assignment

## Build Assignment

Build with sbt.

## Task 1

The first version is not tail recursive as the recursive call is not in
tail position and is therefore not stack safe,
but I have included a stack safe one that uses streams instead of Lists.

This works because instead of evaluating all values recursively and
constructing the list it calculates the first value, passing the
function for the calculation of other values and returns immediately.

Task2 can be run with the assignment tests

## Task 2

One assumption I have made is that inputs can be duplicates as this was
neither confirmed nor denied in the specification but this doesn't
really change the problem.

Many of the functions in this are not tail recursive but I deemed this
acceptable because in general the structures reduce by a large factor at
each stage of recursion.

The solutions could be optimised in many ways by not searching such a
large problem space. For example, the commutativity of the addition and
multiply functions could be taken advantage of so that 2 * 5 and 5 * 2
are not distinct solutions.

Also the searching could be optimized to prioritize functions to get
closer to the target quicker but I haven't really considered this as it
would be overly complex for this application.

Task2 program can be run by running task2.Main with sbt
