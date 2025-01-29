Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Options';
  constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

End.

