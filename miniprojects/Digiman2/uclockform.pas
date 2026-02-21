Unit uclockform;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TClockForm }

  TClockForm = Class(TForm)
    Button1: TButton;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure SetInterval(aIntervalms: Integer);
    Function GetIntervalms(): Integer;
  End;

Implementation

{$R *.lfm}

{ TForm2 }

Procedure TClockForm.FormCreate(Sender: TObject);
Begin
  caption := 'Please choose:';
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
End;

Procedure TClockForm.SetInterval(aIntervalms: Integer);
Begin
  RadioGroup1.ItemIndex := 1;
  Case aIntervalms Of
    500: RadioGroup1.ItemIndex := 2;
    1000: RadioGroup1.ItemIndex := 1;
    2000: RadioGroup1.ItemIndex := 0;
  End;
End;

Function TClockForm.GetIntervalms(): Integer;
Begin
  Case RadioGroup1.ItemIndex Of
    0: result := 2000;
    1: result := 1000;
    2: result := 500;
  End;
End;


End.

