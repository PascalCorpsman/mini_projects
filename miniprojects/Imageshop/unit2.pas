(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Imageshop                                             *)
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit,
  SynHighlighterPas;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
  private

    Procedure HandleOP(aName: String);
    Procedure HandleBlend(aName: String);

  public

    Function GetOPList(): TStringlist;

    Procedure RefreshOPList();

  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses fileutil, Unit3, LazFileUtils;

{ TForm2 }

Procedure TForm2.Button1Click(Sender: TObject);
Var
  f: String;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    f := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'operations' + PathDelim + ListBox1.Items[ListBox1.ItemIndex];
    If FileExists(f) Then Begin
      If DeleteFile(f) Then Begin
        ListBox1.Items.Delete(ListBox1.ItemIndex);
      End;
    End;
  End;
End;

Procedure TForm2.HandleBlend(aName: String);
Var
  n, f: String;
  sl: TStringList;
Begin
  // Create Blend Operation
  Form3.PrepareForBlend;
  form3.ModalResult := mrNone;
  f := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'operations' + PathDelim + aName + '.blend';
  If FileExistsUTF8(f) Then Begin
    sl := TStringList.create;
    sl.LoadFromFile(f);
    Form3.SynEdit1.text := sl.Text;
    sl.free;
  End;

  form3.ShowModal;
  If (form3.ModalResult = mrOK) Then Begin
    n := InputBox('Question', 'Enter a new Blend Operationname:', aName);
    f := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'operations' + PathDelim + n + '.blend';
    sl := TStringList.Create;
    sl.Text := Form3.SynEdit1.Text;
    If Not FileExistsUTF8(f) Then Begin
      ListBox1.Items.Add(ExtractFileName(f));
    End;
    sl.SaveToFile(f);
    sl.free;
  End;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  HandleBlend('');
End;

Procedure TForm2.HandleOP(aName: String);
Var
  n, f: String;
  sl: TStringList;
Begin
  // Create Pixel Operation
  form3.PrepareForOp;
  form3.ModalResult := mrNone;
  f := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'operations' + PathDelim + aName + '.op';
  If FileExistsUTF8(f) Then Begin
    sl := TStringList.create;
    sl.LoadFromFile(f);
    Form3.SynEdit1.text := sl.Text;
    sl.free;
  End;
  form3.ShowModal;
  If (form3.ModalResult = mrOK) Then Begin
    n := InputBox('Question', 'Enter a new Blend Operationname:', aName);
    f := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'operations' + PathDelim + n + '.op';
    sl := TStringList.Create;
    sl.Text := Form3.SynEdit1.Text;
    If Not FileExistsUTF8(f) Then Begin
      ListBox1.Items.Add(ExtractFileName(f));
    End;
    sl.SaveToFile(f);
    sl.free;
  End;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  HandleOP('');
End;

Procedure TForm2.Button4Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'List of all available operations.';
End;

Procedure TForm2.ListBox1DblClick(Sender: TObject);
Var
  f: String;
Begin
  // Öffnen der Gerade Ausgewählten Operation
  If ListBox1.ItemIndex <> -1 Then Begin
    f := ListBox1.Items[ListBox1.ItemIndex];
    If lowercase(ExtractFileExt(f)) = '.op' Then Begin
      HandleOP(ExtractFileNameWithoutExt(f));
    End
    Else Begin
      HandleBlend(ExtractFileNameWithoutExt(f));
    End;
  End;
End;

Function TForm2.GetOPList(): TStringlist;
Var
  f: String;
Begin
  f := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'operations';
  result := FindAllFiles(f, '*.op;*.blend', false);
End;

Procedure TForm2.RefreshOPList();
Var
  sl: TStringList;
  i: Integer;
Begin
  sl := GetOPList();
  ListBox1.Clear;
  For i := 0 To sl.Count - 1 Do Begin
    ListBox1.Items.Add(ExtractFileName(sl[i]));
  End;
  sl.free;
End;

End.

