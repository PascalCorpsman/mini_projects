Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    fAbortCallback: TNotifyEvent;
  public
    Procedure RegisterAbortCallback(aValue: TNotifyEvent);
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Progress';
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  fAbortCallback := Nil;
End;

Procedure TForm3.RegisterAbortCallback(aValue: TNotifyEvent);
Begin
  fAbortCallback := aValue;
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  // Abort File Transfer
  If assigned(fAbortCallback) Then Begin
    fAbortCallback(self);
  End;
End;

End.

