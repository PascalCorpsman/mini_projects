Unit Unit5;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    ScrollBar1: TScrollBar;
    Procedure FormCreate(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
  private

  public

  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

{ TForm5 }

Procedure TForm5.ScrollBar1Change(Sender: TObject);
Begin
  If ScrollBar1.Position = 0 Then Begin
    edit1.text := 'Exakt match';
  End
  Else Begin
    edit1.text := format('%d%%', [ScrollBar1.Position]);
  End;
End;

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  Caption := 'color match options';
  ScrollBar1Change(Nil);
End;

End.

