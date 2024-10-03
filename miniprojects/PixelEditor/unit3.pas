Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Procedure Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  private

  public

  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses LCLType;

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'New';
End;

Procedure TForm3.FormShow(Sender: TObject);
Begin
  Edit1.SetFocus;
End;

Procedure TForm3.Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If key = VK_ESCAPE Then ModalResult := mrCancel;
  If key = VK_RETURN Then Button1.Click;
End;

End.

