Unit urgb_level;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  { TRGB_Level }

  TRGB_Level = Class
  private
  public
    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Function LoadLevel(Const aFilename: String): Boolean;

    Procedure Render();
  End;

Implementation

{ TRGB_Level }

Constructor TRGB_Level.Create;
Begin
  Inherited Create;
End;

Destructor TRGB_Level.Destroy;
Begin

End;

Function TRGB_Level.LoadLevel(Const aFilename: String): Boolean;
Begin
  result := false;
  Hier weiter !

End;

Procedure TRGB_Level.Render;
Begin

End;

End.

