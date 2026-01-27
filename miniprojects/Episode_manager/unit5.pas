(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Episode manager                                       *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit unit5;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uepisodenmanager;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure Init(Const aDataSet: TDatensatz);
    Function lclToDatensatz(): TDatensatz;
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  caption := 'Detailview';
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
End;

Procedure TForm5.Init(Const aDataSet: TDatensatz);
Begin
  CheckBox1.Checked := aDataSet.Gesehen;
  edit1.text := aDataSet.Serie;
  edit2.text := aDataSet.Staffel;
  edit3.text := aDataSet.Episodenname;
End;

Function TForm5.lclToDatensatz(): TDatensatz;
Begin
  result.Gesehen := CheckBox1.Checked;
  result.Serie := Edit1.Text;
  result.Staffel := Edit2.Text;
  result.Episodenname := Edit3.Text;
End;

End.

