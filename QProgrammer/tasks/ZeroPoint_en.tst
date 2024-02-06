{
Write a function that calculates the right x value where 
the following function f(x) = ax^2+bx+c = 0.

If there are no or more than 2 zero crossings, the function
should return the value -1000
}
Function Nullstelle(A,B,C: Integer): Integer;
Test1:(0,0,0):-1000
Test2:(-1,20,-100):10
Test3:(-1,4,-3):3
Test4:(-1,4,-5):-1000
Test5:(1,0,0):0
Test6:(1,-2,3):-1000
Test7:(1,-4,3):3
Test8:(1,4,3):-1
Test9:(1,4,4):-2
Test10:(1,4,5):-1000
Test11:(0,1,-4):4
Test12:(0,-1,-4):-4
Test13:(1,0,3):-1000
Test14:(-1,0,9):3
Test15:(1,0,-9):3
Test16:(-1,0,-9):-1000
Test17:(0,0,1):-1000