-- LAB 1: COMPLEX NUMBERS

The purpose of this lab is to give you training in gradually developing a program
consisting of a variety of functions. As this is the first laboratory, you will receive help
with which functions to be manufactured and in what order. In a sharp case, of course
the division into functions, and how they are based on each other, a very important part of
development. In the same way, we should be aware that certain parts of the laboratory would
be able to be solved in a better way by using concepts that we will learn later in
the course, such as own types, recursion and higher order functions. 

z = a + bi (rectangular form)
   z = re^(iv) = |z|(cos(v)+i*sin(v)), let z = a+bi and |z| = sqrt(x^2 + y^2)
   the form above is called: "polar form". r is the length of the radius from the 
   origin whereas the angle v is the angle found between the positive x axis
   and the radius r. 


RELATIONSSHIPS BETWEEN RECTANGULAR AND POLAR FORM

-- a) a = rcos(v)

-- b) b = rsin(v)

-- c) r = sqrt(a^2 + b^2)

-- d) v = arctan(b/a) if a>0 sqrt(2)|pi/4

-- e) v = arctan(b/a)+pi if a>0

-- f) v = pi/2 if x = 0 and y>=0

-- g) v = 3pi/2 if x=0 and y<0

-- format

-- (<type>,Floating,Floating), where for type we can choose between R and P.
   
-- I.e. R = Rectangular form and P = Polar form. 
   
-- For the R form, Floating equals the real values of the imaginary number, a && b.
   
-- in z = a+bi. In the polar form, the first float represents r and the second 
   
-- represents the angle v.
   
-- EXAMPLE: (”R”, 1.0, 1.0) and (”P”, 1.4142, 0.7854)

The Program should NOT "just work". 
Quite the opposite is true. The code should upload a HIGH STANDARD.
   
-- a) aim to generate small but clear functions
   
-- b) use existing functions when creating new ones
   
-- c) all function declarations should be preceeded by a typesignature
   
-- d) only break apart the complex numbers by the use of pattern-matching when needed
   
-- e) use anonymous variables if possible
   
-- f) utilize the error command