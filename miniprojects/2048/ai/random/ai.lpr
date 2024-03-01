Library ai;

{$MODE objfpc}{$H+}

Uses
  Classes
  { you can add units after this };

Type
  TField = Array Of Array Of int64;

Const
  MOVE_LEFT = 37;
  MOVE_UP = 38;
  MOVE_RIGHT = 39;
  MOVE_DOWN = 40;
  GIVE_UP = 0;

Var
  (*
   * Add here some informations about your library
   *)
  AiInfoString: String = 'Ai - Random walk.';

Function AiMove(data: TField; Boardsize: integer): integer; cdecl;
Begin
  result := GIVE_UP;
  Case random(4) Of
    0: result := MOVE_LEFT;
    1: result := MOVE_UP;
    2: result := MOVE_RIGHT;
    3: result := MOVE_DOWN;
  End;
End;

Function Info: PChar; cdecl;
Begin
  result := @AiInfoString[1];
End;

Exports
  AiMove, Info;

End.

