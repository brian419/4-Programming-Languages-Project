"Create a Set with elements 1 and 2"
x := Set new.
x add: 1; add: 2.

"Create two strings 'second' and 'third'"
y := 'second'.
z := 'third'.

"Create an array with 3 elements"
a := Array new: 3.
a at: 1 put: x.  "Put the Set x at index 1 of array a"
a at: 2 put: y.  "Put the string y at index 2 of array a"
a at: 3 put: z.  "Put the string z at index 3 of array a"

"Assign references to arrays b and c"
b := a.    "b points to the same array as a"
c := a copy.    "c is a shallow copy of array a"
d := a deepCopy.    "d is a deep copy of array a"

"Print the contents of arrays a, b, c, and d"
a printNl.
b printNl.
c printNl.
d printNl.

"Modify array a"
a at: 2 put: '2nd'.    "Change the element at index 2 of array a to '2nd'"
(a at: 1) add: 3.    "Add element 3 to the Set at index 1 of array a"

"Print the contents of arrays a, b, c, and d after modification"
a printNl.
b printNl.
c printNl.
d printNl.
