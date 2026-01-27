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
Unit unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Procedure Button1Click(Sender: TObject);
  private

  public
    Procedure Init(Const aIniFile: TIniFile);
    Procedure StoreSettings(Const aIniFile: TIniFile);

  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses
  usslconnector, uepisodenmanager;

{ TForm3 }

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  If Login(edit1.text, edit2.text, ClientID, edit3.text, edit4.text) Then Begin
    showmessage('Succeed.');
  End
  Else Begin
    showmessage('Failed to log in.');
  End;
End;

Procedure TForm3.Init(Const aIniFile: TIniFile);
Begin
  Edit1.text := aIniFile.ReadString('Filechecker', 'URL', 'https://127.0.0.1');
  Edit2.text := aIniFile.ReadString('Filechecker', 'Port', '8443');
  Edit3.text := aIniFile.ReadString('Filechecker', 'User', 'Username');
  Edit4.text := aIniFile.ReadString('Filechecker', 'Password', '');
End;

Procedure TForm3.StoreSettings(Const aIniFile: TIniFile);
Begin
  aIniFile.WriteString('Filechecker', 'URL', Edit1.text);
  aIniFile.WriteString('Filechecker', 'Port', Edit2.text);
  aIniFile.WriteString('Filechecker', 'User', Edit3.text);
  aIniFile.WriteString('Filechecker', 'Password', Edit4.text);
End;

End.

