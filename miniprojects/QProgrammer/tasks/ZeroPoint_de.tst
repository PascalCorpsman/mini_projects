{
Schreiben sie ein Programm, welches nach Eingabe
der Paremter a,b,c die "rechte" Nullstelle der
Normalparabel berechnet ( f(x) = ax^2+bx+c )

Wenn keine Nullstelle Existiert oder mehr als 2,
soll der Wert : -1000 ausgegeben werden.

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