(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ALT_F2                                                *)
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

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, ExtCtrls, ExtDlgs;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses lazutf8, unit1;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Shortcut Editor';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
{$IFDEF Unix}
  OpenDialog1.DefaultExt := '';
  OpenDialog1.Filter := '';
{$ENDIF}
End;

Procedure TForm2.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Begin
  If length(FileNames) > 0 Then Begin
    edit2.Text := FileNames[0];
  End;
End;

Procedure TForm2.FormShow(Sender: TObject);
Begin
  Edit1.SetFocus;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    edit2.text := OpenDialog1.FileName;
  End;
End;

Procedure TForm2.Button4Click(Sender: TObject);
Var
  p: TPortableNetworkGraphic;
  b: TBitmap;
Begin
  // Set Icon
  OpenPictureDialog1.InitialDir := Form1.Ini_File.ReadString('General', 'LastOpenPictureDialogFolder', OpenPictureDialog1.InitialDir);
  If OpenPictureDialog1.Execute Then Begin
    Form1.Ini_File.WriteString('General', 'LastOpenPictureDialogFolder', ExtractFilePath(OpenPictureDialog1.FileName));
    Case lowercase(ExtractFileExt(OpenPictureDialog1.FileName)) Of
      '.bmp': Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      '.png': Begin
          p := TPortableNetworkGraphic.Create;
          p.LoadFromFile(OpenPictureDialog1.FileName);
          b := TBitmap.Create;
          b.Assign(p);
          p.free;
          Image1.Picture.Assign(b);
          b.free;
        End;
    Else Begin
        showmessage('Error, fileformat not supported.');
      End;
    End;
  End;
End;

Procedure TForm2.Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If key = VK_ESCAPE Then Button2.Click;
  If key = VK_RETURN Then Button1.Click;
End;

End.

