(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of SimpleRay                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus;

Type
  TForm2 = Class(TForm)
    PopupMenu1: TPopupMenu;
    backtomainwindow1: TMenuItem;
    saveimage1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Procedure FormPaint(Sender: TObject);
    Procedure backtomainwindow1Click(Sender: TObject);
    procedure saveimage1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses Unit1;

Procedure TForm2.FormPaint(Sender: TObject);
Begin
  form2.canvas.Draw(20, 20, bm);
End;

Procedure TForm2.backtomainwindow1Click(Sender: TObject);
Begin
  form1.show;
End;

procedure TForm2.saveimage1Click(Sender: TObject);
begin
  If Savedialog1.execute Then Begin
    Savedialog1.InitialDir := ExtractFilePath(Savedialog1.FileName);
    bm.SaveToFile(Savedialog1.FileName);
  End;

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  SaveDialog1.initialdir := ExtractFilePath(paramstr(0));

end;

End.

