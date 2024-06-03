(******************************************************************************)
(* Sequoia                                                         02.06.2024 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : visualizes sizes of folders and subfolders                   *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Show file / folder infos                              *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  usunburstchart;

Const
  Colors: Array Of TColor =
  (
    $00FEF906, $00B9F8C5, $004BF44C, $0002BA0A, $00FD39FD,
    $00B60AB3, $002FF4FB, $00F5433C, $00C40000, $00FEBFBA,
    $000001BF, $00BEC4F8
    );

Type

  TScanDirResult = Record
    aSize: uint64;
    aCount: integer;
  End;

  TKind = (kFile, kFolder);

  TUserData = Record
    Kind: TKind;
    Folder, Name: String;
    Size: UInt64;
    // if Kind = kFolder -> Filecount
    Files: integer;
    // if Kind = kFolder -> Filecount of all subfolders
    RecursiveFiles: integer;
  End;

  PUserData = ^TUserData;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    StatusBar1: TStatusBar;
    SunburstChart1: TSunburstChart;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
  private
    aColor: Integer;
    aStack: TStringArray;

    Function DefaultElement(): TSunBurstChartElement;

    Function ScanDir(aDir: String; ParentNode: PSunBurstChartElement
      ): TScanDirResult;

    Procedure LoadDirectory(Const aDirectory: String);

    Procedure OnChartResize(Sender: Tobject);
    Procedure OnChartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OnChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnChartDeleteElementsUserData(Sender: TObject; aUserData: Pointer);
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses math, LazFileUtils;

Function FileSizeToString(Value: Int64): String;
Var
  s: char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value > 1024 Then Begin
    s := 'K';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'M';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'G';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'T';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'P';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If (r Div 100) <> 0 Then
    result := inttostr(value) + ',' + inttostr(r Div 100) + s + 'B'
  Else
    result := inttostr(value) + s + 'B'
End;

Function TForm1.DefaultElement: TSunBurstChartElement;
Var
  userData: PUserData;
Begin
  result := DefaultSunBurstChartElement();
  result.Color.BrushColor := Colors[aColor];
  result.Color.PenWitdh := 1;
  aColor := (aColor + 1) Mod length(Colors);
  new(userData);
  result.UserData := userData;
  userData^.Kind := kFile;
  userData^.Folder := '';
  userData^.Name := '';
  userData^.Size := 0;
  userData^.Files := 0;
  userData^.RecursiveFiles := 0;
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  e: TSunBurstChartElement;
Begin
  caption := 'Sequoia view ver. 0.01 by Corpsman';
  // Create the component by hand, so we do not need to register it into the IDE
  SunburstChart1 := TSunburstChart.Create(self);
  SunburstChart1.Name := 'SunburstChart1';
  SunburstChart1.Parent := self;
  SunburstChart1.Top := 100;
  SunburstChart1.Left := 8;
  SunburstChart1.OnResize := @OnChartResize;
  SunburstChart1.OnMouseDown := @OnChartMouseDown;
  SunburstChart1.OnMouseMove := @OnChartMouseMove;
  SunburstChart1.OnDeleteElementsUserData := @OnChartDeleteElementsUserData;
  e := DefaultElement;
  e.Caption := 'Please load a directory';
  edit1.text := '';
  SunburstChart1.AddChildElement(Nil, e);
  SunburstChart1.Align := alClient;
  If ParamCount > 0 Then Begin
    If DirectoryExists(ParamStr(1)) Then Begin
      LoadDirectory(ParamStr(1));
    End;
  End;
  aStack := Nil;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Begin
  If DirectoryExists(trim(FileNames[0])) Then Begin
    LoadDirectory(trim(FileNames[0]));
  End;
  If FileExists(trim(FileNames[0])) Then Begin
    LoadDirectory(ExtractFilePath(trim(FileNames[0])));
  End;
End;

Function TForm1.ScanDir(aDir: String; ParentNode: PSunBurstChartElement
  ): TScanDirResult;
Var
  node, child: TSunBurstChartElement;
  NodeP: PSunBurstChartElement;
  sR: TSearchRec;
  ud: PUserData;
  recursiveres: TScanDirResult;
  RecursiveCount: integer;
  Folders: Integer;
Begin
  result.aCount := 0;
  result.aSize := 0;
  RecursiveCount := 0;
  Folders := 0;
  If FindFirstUTF8(aDir + '*', faAnyFile, sr) = 0 Then Begin
    node := DefaultElement;
    node.Value := 0;
    ud := node.UserData;
    ud^.Folder := IncludeTrailingPathDelimiter(ExtractFileDir(ExcludeTrailingPathDelimiter(aDir)));
    ud^.Name := ExtractFileName(ExcludeTrailingPathDelimiter(aDir));
    ud^.Kind := kFolder;
    If ParentNode = Nil Then Begin
      node.Caption := ud^.Name;
    End;
    nodep := SunburstChart1.AddChildElement(ParentNode, Node);
    Try
      Repeat
        If CheckBox1.Checked Then Begin
          If (sr.Attr And faHidden) = faHidden Then Continue;
        End;
        If (sR.Attr And faDirectory = faDirectory) Then Begin
          If (sr.Name <> '.') And (sr.Name <> '..') Then Begin
            Folders := Folders + 1;
            recursiveres := ScanDir(IncludeTrailingPathDelimiter(adir + sr.Name), nodep);
            nodep^.Value := nodep^.Value + recursiveres.aSize;
            RecursiveCount := RecursiveCount + recursiveres.aCount;
          End;
        End
        Else Begin
          result.aCount := result.aCount + 1;
          nodep^.Value := nodep^.Value + sR.Size;
          child := DefaultElement;
          child.Value := sR.Size;
          ud := child.UserData;
          ud^.Folder := aDir;
          ud^.Name := sr.Name;
          ud^.Size := sR.Size;
          ud^.Kind := kFile;
          SunburstChart1.AddChildElement(nodep, child);
        End;
      Until FindNextUTF8(sr) <> 0;
    Finally
      FindCloseUTF8(sr);
    End;
    ud := node.UserData;
    ud^.Size := nodep^.Value;
    ud^.Files := Result.aCount + Folders;
    ud^.RecursiveFiles := RecursiveCount + result.aCount;
    result.aSize := nodep^.Value;
  End;
End;

Procedure TForm1.LoadDirectory(Const aDirectory: String);
Begin
  SunburstChart1.Clear;
  edit1.text := ExcludeTrailingPathDelimiter(aDirectory);
  aColor := 0;
  ScanDir(IncludeTrailingPathDelimiter(aDirectory), Nil);
  OnChartMouseMove(Nil, [], -1, -1);
  SunburstChart1.Invalidate;
End;

Procedure TForm1.OnChartResize(Sender: Tobject);
Begin
  SunburstChart1.PieCenter := point(SunburstChart1.Width Div 2, SunburstChart1.Height Div 2);
  SunburstChart1.PieRadius := min(SunburstChart1.Width / 2, SunburstChart1.Height / 2) - 10;
  SunburstChart1.Invalidate;
End;

Procedure TForm1.OnChartMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  segment: PSunBurstChartElement;
  ud: PUserData;
Begin
  If ssleft In shift Then Begin
    If SunburstChart1.GetSegmentAtPos(x, y, segment) Then Begin
      ud := segment^.UserData;
      If Not assigned(ud) Then exit;
      If Not FileExists(ud^.Folder + ud^.Name) Then Begin
        If IncludeTrailingPathDelimiter(edit1.text) <> IncludeTrailingPathDelimiter(ud^.Folder + ud^.Name) Then Begin
          setlength(aStack, high(aStack) + 2);
          aStack[high(aStack)] := IncludeTrailingPathDelimiter(edit1.text);
          LoadDirectory(IncludeTrailingPathDelimiter(ud^.Folder + ud^.Name));
          OnChartMouseMove(sender, [], x, y);
        End;
      End;
    End;
  End;
  If ssRight In shift Then Begin
    If high(aStack) <> -1 Then Begin
      LoadDirectory(aStack[high(aStack)]);
      setlength(aStack, high(aStack));
    End;
  End;
End;

Procedure TForm1.OnChartMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  segment: PSunBurstChartElement;
  ud: PUserData;
Begin
  If SunburstChart1.GetSegmentAtPos(x, y, segment) Then Begin
    ud := segment^.UserData;
    If Not assigned(ud) Then exit;
    StatusBar1.Panels[0].Text := FileSizeToString(ud^.Size);
    StatusBar1.Panels[1].Text := ud^.Folder + ud^.Name;
    If ud^.Kind = kFolder Then Begin
      StatusBar1.Panels[0].Text := StatusBar1.Panels[0].Text + format(' [%d %d]', [ud^.Files, ud^.RecursiveFiles]);
    End;
  End
  Else Begin
    StatusBar1.Panels[0].Text := '';
    StatusBar1.Panels[1].Text := '';
  End;
End;

Procedure TForm1.OnChartDeleteElementsUserData(Sender: TObject;
  aUserData: Pointer);
Var
  userData: PUserData;
Begin
  userData := aUserData;
  dispose(userData);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  If DirectoryExists(Edit1.Text) Then Begin
    LoadDirectory(IncludeTrailingPathDelimiter(Edit1.text));
  End;
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  button2.Click;
End;

Procedure TForm1.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then button2.Click;
End;

End.

