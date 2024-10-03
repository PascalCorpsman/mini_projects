Unit Unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Shape1: TShape;
    Procedure Shape1Click(Sender: TObject);
  private

  public

  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

{ TForm4 }

Procedure TForm4.Shape1Click(Sender: TObject);
Begin
  ColorDialog1.Color := Shape1.Brush.Color;
  If ColorDialog1.Execute Then Begin
    Shape1.Brush.Color := ColorDialog1.Color;
  End;
End;

End.

