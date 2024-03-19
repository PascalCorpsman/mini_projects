(******************************************************************************)
(* SimpleRay                                                       ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of a custom raytracer                         *)
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
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Raytracer, Raytracer_Math, ExtCtrls, RayParser,
  SynEdit, SynEditHighlighter, Menus,
  SynEditTypes, SynHighlighterAny, SynCompletion;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button4: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog2: TSaveDialog;
    Button6: TButton;
    SynAnySyn1: TSynAnySyn;
    Button7: TButton;
    Label3: TLabel;
    Panel1: TPanel;
    SynAutoComplete1: TSynAutoComplete;
    ListBox1: TListBox;
    Button8: TButton;
    Button9: TButton;
    PopupMenu1: TPopupMenu;
    SaveScene1: TMenuItem;
    Button10: TButton;
    CodeFormat1: TMenuItem;
    Raytrace1: TMenuItem;
    SynEdit1: TSynEdit;
    Syntaxcheck1: TMenuItem;
    Replace1: TMenuItem;
    Comment1: TMenuItem;
    Find1: TMenuItem;
    FindDialog1: TFindDialog;
    Button11: TButton;
    //    SynEditSearch1: TSynEditSearch;
        //    SynCompletionProposal1: TSynCompletionProposal;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: Char);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button8Click(Sender: TObject);
    Procedure SynEdit1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure Button9Click(Sender: TObject);
    Procedure SaveScene1Click(Sender: TObject);
    Procedure Button10Click(Sender: TObject);
    Procedure CodeFormat1Click(Sender: TObject);
    Procedure Raytrace1Click(Sender: TObject);
    Procedure Syntaxcheck1Click(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure Replace1Click(Sender: TObject);
    Procedure Comment1Click(Sender: TObject);
    Procedure Find1Click(Sender: TObject);
    Procedure FindDialog1Find(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure SynEdit1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Const
  Version = '0.01';
  cpt = 'SimpleRay ver ' + Version + ', by Corpsman | Targetsoft | support : www.Corpsman.de';

Var
  Form1: TForm1;
  bm: Tbitmap;
  ray: TRaytracer;
  TexManager: TTexturemanager;
  RayParse: TRayparser;
  Gerror: Boolean;
  changed_: boolean;
  Filename: String;
  bla: Boolean;

Implementation

Uses Unit2, Replace, Unit4;

{$R *.lfm}

// Ermittelt die letze Position von Substr in S und gibt diese zurück

Function ReversePos(Substr: String; S: String): Integer;
Var
  i, j: Integer;
  b: Boolean;
Begin
  result := 0;
  For i := length(s) - Length(Substr) Downto 1 Do Begin
    b := true;
    For j := 1 To Length(substr) Do
      If s[i + j] <> substr[j] Then Begin
        b := false;
        break;
      End;
    If b Then Begin
      result := i + 1;
      exit;
    End;
  End;
End;

// Wird ein Index im Text überge ben so Ermittelt die Function die Zeile in der der Index steht

Function IndextoLine(Const Data: Tstrings; Index: Integer): integer;
Var
  erg: Integer;
  x: integer;
Begin
  erg := 0;
  x := 0;
  While x < index Do Begin
    If data.Text[x] = #13 Then inc(erg);
    inc(x);
  End;
  result := erg;
End;

Function gedwordat(index: Integer; Text: String): String;
Var
  i: Integer;
  a, e: integer;
Begin
  If (index < 1) Or (Index > length(Text)) Then Begin
    result := '';
    exit;
  End;
  a := 1;
  For i := index Downto 1 Do
    If Not (text[i] In ['a'..'z', 'A'..'Z']) Then Begin
      a := i + 1;
      break;
    End;
  e := length(text);
  For i := index To Length(text) Do
    If Not (text[i] In ['a'..'z', 'A'..'Z']) Then Begin
      e := i - 1;
      break;
    End;
  result := copy(text, a, e - a + 1);
End;

// Löscht Führende Leerzeichen

Function DelFrontspace(Value: String): String;
Var
  erg: String;
Begin
  erg := value;
  While pos(' ', erg) = 1 Do
    delete(erg, 1, 1);
  result := erg;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  (*

  Die Autocomplete Funktion geht nicht mehr..

  Wenn der Code Formater sein Werk getan hat,
    dann kann die File nicht mehr compiliert werden
    - es sieht danach aus das der // Kommentar irgend etwas kaputt macht in der Rayparser.Format
  siehe Problem2.scene

  Es Fehlt noch:

  Cool wäre nun noch ein Formelparser für Single Variablen...

  Fertigbaun der Online Hilfe

  Kompliziertere Primitive wie: Kreisflächen, Bezierpatches, Fraktale...

  BumpMapping: derart das 0..255 auf - 1..1 gemappt wird.R = x, G = y, b = z
  Die Normalen werden aber nur geoffsettet

  // *)
  bla := True;
  SaveDialog2.initialdir := ExtractFilePath(paramstr(0));
  OpenDialog1.initialdir := ExtractFilePath(paramstr(0));
  Listbox1.Visible := False;
  ray := TRaytracer.create;
  texmanager := TTextureManager.create;
  RayParse := TRayparser.create(Ray, texmanager);
  bm := Tbitmap.create;
  bm.width := 266;
  bm.height := 200;
  bm.pixelformat := pf24bit;
  Button8Click(Nil);
  SynAutoComplete1.AutoCompleteList.clear;
  For i := 0 To SynAnySyn1.Objects.count - 1 Do
    SynAutoComplete1.AutoCompleteList.Add(OnlyFirstUP(SynAnySyn1.Objects[i]));
  SynAutoComplete1.AutoCompleteList.Add('Clamp');
  SynAutoComplete1.AutoCompleteList.Add('Scale');
  SynAutoComplete1.AutoCompleteList.Add('On');
  SynAutoComplete1.AutoCompleteList.Add('Off');
  SynAutoComplete1.AutoCompleteList.Add('Front');
  SynAutoComplete1.AutoCompleteList.Add('Back');
  SynAutoComplete1.AutoCompleteList.Add('Repeat');
  SynAutoComplete1.AutoCompleteList.Add('Nearest');
  SynAutoComplete1.AutoCompleteList.Add('Biliniar');
  SynAutoComplete1.AutoCompleteList.Add('Triliniar');
  SynAutoComplete1.AutoCompleteList.Add('Cos');
  SynAutoComplete1.AutoCompleteList.Add('Linear');
  If length(PAramstr(1)) <> 0 Then Begin
    If FileExists(paramstr(1) { *Converted from FileExists*  }) Then Begin
      Filename := paramstr(1);
      Synedit1.lines.LoadFromFile(Filename);
      caption := cpt + ' : ' + extractfilename(Filename);
    End;
  End;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  bm.free;
  ray.free;
  texmanager.free;
  Rayparse.free;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  If Length(Filename) <> 0 Then
    SaveDialog2.InitialDir := extractfilepath(FileName);
  If SAvedialog2.execute Then Begin
    changed_ := False;
    SaveDialog2.InitialDir := extractfilepath(SAvedialog2.FileName);
    form2.SaveDialog1.InitialDir := extractfilepath(SAvedialog2.FileName);
    OpenDialog1.InitialDir := extractfilepath(SAvedialog2.FileName);
    Synedit1.lines.SaveToFile(SAvedialog2.FileName);
    Filename := SAvedialog2.FileName;
    caption := cpt + ' : ' + extractfilename(Filename);
  End;
End;

Procedure TForm1.Edit1KeyPress(Sender: TObject; Var Key: Char);
Begin
  If key = #13 Then button1.onclick(Nil);
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  If changed_ Then
    If id_no = application.messagebox('Not saved yet, load new file anyway ?', 'Error', MB_YESNO Or MB_ICONWARNING) Then exit;
  If Length(Filename) <> 0 Then
    Opendialog1.InitialDir := extractfilepath(FileName);
  If Opendialog1.execute Then Begin
    changed_ := False;
    SaveDialog2.InitialDir := extractfilepath(Opendialog1.FileName);
    form2.SaveDialog1.InitialDir := extractfilepath(Opendialog1.FileName);
    OpenDialog1.InitialDir := extractfilepath(Opendialog1.FileName);
    Synedit1.lines.LoadfromFile(Opendialog1.FileName);
    caption := cpt + ' : ' + extractfilename(Opendialog1.FileName);
    Filename := Opendialog1.FileName;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  s, errors: String;
  b: Boolean;
Begin
  b := False;
  label3.caption := 'Status : Parsing...                                 '#13'                                                                  ';
  label3.Refresh;
  errors := RayParse.Parse(synedit1.text, Filename);
  Listbox1.visible := false;
  Listbox1.items.clear;
  If length(errors) <> 0 Then Begin
    If pos('Error', errors) <> 0 Then
      Gerror := True;
    While pos(#13, errors) <> 0 Do Begin
      s := copy(errors, 1, pos(#13, errors) - 1);
      delete(errors, 1, pos(#13, errors));
      If Length(s) <> 0 Then Begin
        Listbox1.Items.add(s);
        If (pos('Error', s) <> 0) And Not b Then Begin
          b := true;
          s := copy(s, ReversePos(' ', s) + 1, length(s));
          SynEdit1.CaretY := strtoint(s);
          SynEdit1.SetFocus;
        End;
      End;
    End;
    If Length(Errors) <> 0 Then Begin
      Listbox1.Items.add(errors);
    End;
  End;
  Listbox1.visible := listbox1.items.count <> 0;
  label3.caption := 'Status : ready';
  label3.Refresh;
  If Gerror And form2.visible Then form2.close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  d1: Cardinal;
Begin
  Gerror := False;
  button6.onclick(Nil);
  If Not Gerror Then Begin
    bm.width := RayParse.Imagewidth;
    bm.height := RayParse.Imageheight;
    label3.caption := 'Status : rendering...                             '#13'                                              ';
    label3.Refresh;
    d1 := GetTickCount;
    ray.Render(bm.canvas, bm.width, bm.height);
    Form2.width := bm.width + 45;
    form2.height := bm.height + 40 + 24;
    form2.Constraints.MinHeight := bm.height + 40 + 24;
    form2.Constraints.MaxHeight := bm.height + 40 + 24;
    Form2.Constraints.MinWidth := bm.width + 45;
    Form2.Constraints.MaxWidth := bm.width + 45;
    d1 := gettickcount - d1;
    If form2.visible Then Begin
      form2.Refresh;
      form2.SetFocus;
    End
    Else Begin
      form2.top := (screen.height - form2.height) Div 2;
      form2.left := (screen.width - form2.width) Div 2;
      Form2.show;
    End;
    label3.caption := 'Status : ready [' + floattostrf(d1 / 1000, fffixed, 7, 3) + ' s, ' + inttostr(ray.PrimitiveCount) + ' p]'#13'Max used rekursion : ' + inttostr(ray.MaxusedRecursionDeth);
  End;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  form2.show;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Var
  buffer: String;
Begin
  If high(filenames) > -1 Then Begin
    buffer := filenames[0];
    If FileExists(Buffer) Then Begin
      If changed_ Then Begin
        If id_YES = application.Messagebox('Old File not saved, open new File anyway ?', 'Error', MB_YESNO Or MB_ICONQUESTION) Then Begin
          Filename := Buffer;
          Synedit1.lines.LoadFromFile(Filename);
          caption := cpt + ' : ' + extractfilename(Filename);
          form1.BringToFront;
          SetForegroundWindow(self.handle);
        End;
      End
      Else Begin
        Filename := Buffer;
        Synedit1.lines.LoadFromFile(Filename);
        caption := cpt + ' : ' + extractfilename(Filename);
        form1.BringToFront;
        SetForegroundWindow(self.handle);
      End;
    End;
  End;
End;

Procedure TForm1.ListBox1DblClick(Sender: TObject);
Var
  s: String;
Begin
  s := listbox1.items[listbox1.itemindex];
  s := copy(s, ReversePos(' ', s) + 1, length(s));
  SynEdit1.CaretY := strtoint(s);
  SynEdit1.SelectLine(true);
  SynEdit1.SetFocus;
End;

Procedure TForm1.ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  ListBox1.ItemIndex := ListBox1.ItemAtPos(point(x, y), true);
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  Caption := cpt;
  FileName := '';
  changed_ := False;
  SynEdit1.Lines.clear;
  listbox1.visible := False;
End;

Procedure TForm1.SynEdit1Change(Sender: TObject);
Begin
  If Not Changed_ Then
    caption := cpt + ' : ' + extractfilename(Filename) + ' [Changed]';
  changed_ := True;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If changed_ Then Begin
    CanClose := ID_YES = application.messagebox('Not saved yet, close anyway ?', 'Question', MB_YESNO Or MB_ICONQUESTION);
  End;
End;

Procedure TForm1.Button9Click(Sender: TObject);
Begin
  If length(Filename) = 0 Then
    button2.onclick(Nil)
  Else Begin
    Changed_ := False;
    SynEdit1.lines.SaveToFile(FileName);
    caption := cpt + ' : ' + extractfilename(Filename);
  End;
End;

Procedure TForm1.SaveScene1Click(Sender: TObject);
Begin
  Button9.onclick(Nil);
End;

Procedure TForm1.Button10Click(Sender: TObject);
Var
  z, x, y, i: Integer;
  keyw, arr: TStringArr;
  t, s: String;
Begin
  showmessage('Deaktivated, in case of converting troubles.');
  exit;
  z := SynEdit1.TopLine;
  y := SynEdit1.Carety;
  x := SynEdit1.Caretx;
  (*
  in Arr stehen alle Schlüsselworte die Großgeschrieben werden müssen
  *)
  setlength(arr, SynAnySyn1.Objects.count);
  //  SynGeneralSyn1.KeyWords.count);
  For i := 0 To SynAnySyn1.Objects.count - 1 Do
    arr[i] := SynAnySyn1.Objects[i];
  //                                              // Alle Schlüsselworte nach denen ein CRT kommt.
  setlength(keyw, 7);
  keyw[0] := 'Material';
  keyw[1] := 'Raytracer';
  keyw[2] := 'Sphere';
  keyw[3] := 'Triangle';
  keyw[4] := 'Quad';
  keyw[5] := 'External';
  keyw[6] := 'Light';
  synedit1.text := RayParse.format(synedit1.text, keyw, arr);
  synedit1.CaretY := y;
  synedit1.Caretx := x;
  synedit1.TopLine := z;
  changed_ := True;
  caption := cpt + ' : ' + extractfilename(Filename) + ' [Changed]';
  synedit1.SetFocus;
  setlength(arr, 0);
End;

Procedure TForm1.CodeFormat1Click(Sender: TObject);
Begin
  button10.onclick(Nil);
End;

Procedure TForm1.Raytrace1Click(Sender: TObject);
Begin
  button1.onclick(Nil);
End;

Procedure TForm1.Syntaxcheck1Click(Sender: TObject);
Begin
  button6.onclick(Nil);
End;

Procedure TForm1.FormPaint(Sender: TObject);
Begin
  If bla Then Begin
    SynEdit1.SetFocus;
    bla := False;
  End;
End;

Procedure TForm1.Replace1Click(Sender: TObject);
Begin
  form3.CheckBox2.Checked := synEdit1.SelStart <> synEdit1.Selend;
  Form3.showmodal;
End;

Procedure TForm1.Comment1Click(Sender: TObject);
Var
  i, j, sa, se, x: Integer;
  off, s: String;
Begin
  changed_ := True;
  caption := cpt + ' : ' + extractfilename(Filename) + ' [Changed]';
  sa := IndextoLine(synedit1.Lines, synedit1.selstart);
  se := IndextoLine(synedit1.Lines, synedit1.selEnd);
  If (sa = 0) And (se = 0) Then Begin
    sa := synedit1.CaretY - 1;
    se := sa;
  End;
  For x := min(sa, se) To max(sa, se) Do Begin
    s := DelFrontspace(Synedit1.lines[x]);
    If Length(s) > 1 Then Begin
      If (s[1] <> '/') And (s[2] <> '/') Then Synedit1.lines[x] := '//' + Synedit1.lines[x];
      If (s[1] = '/') And (s[2] = '/') Then Begin
        i := pos('//', Synedit1.lines[x]);
        off := '';
        For j := 1 To i Do
          off := off + ' ';
        Synedit1.lines[x] := off + copy(s, 3, length(s));
      End;
    End;
  End;
End;

Procedure TForm1.Find1Click(Sender: TObject);
Begin
  If Length(SynEdit1.SelText) > 0 Then FindDialog1.FindText := SynEdit1.SelText;
  FindDialog1.Execute;
  FindDialog1.Options := FindDialog1.Options - [frFindNext];
End;

Procedure TForm1.FindDialog1Find(Sender: TObject);
Var
  S: TSynSearchOptions;
  l: Integer;
  aword: String;
Begin
  S := [];
  If Not (frDown In FindDialog1.Options) Then
    S := S + [ssoBackwards];
  If (frWholeWord In FindDialog1.Options) Then
    S := S + [ssoWholeWord];
  If (frMatchCase In FindDialog1.Options) Then
    S := S + [ssoMatchCase];
  l := SynEdit1.SearchReplace(FindDialog1.FindText, '', S);
  If l = 0 Then Begin
    If ssoBackwards In s Then
      aword := 'end'
    Else
      aword := 'beginning';

    If MessageDlg(Format('Text not found. Start from the %s?', [aword]), mterror, [mbyes, mbno], 0) = mrno Then
      FindDialog1.CloseDialog
    Else Begin
      SynEdit1.CaretX := 1;
      If ssoBackwards In s Then
        SynEdit1.CaretY := SynEdit1.Lines.Count
      Else
        SynEdit1.CaretY := 1;
      FindDialog1Find(Sender);
    End;
  End;

End;

Procedure TForm1.Button11Click(Sender: TObject);
Begin
  Form4.TreeView1.Items[0].Selected := True;
  Form4.TreeView1Click(Nil);
  form4.show;
End;

Procedure TForm1.SynEdit1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  s: String;
  i: integer;
Begin
  If key = vk_f1 Then Begin
    s := lowercase(gedwordat(synedit1.selstart, synedit1.text));
    If length(s) <> 0 Then Begin
      If s = 'nearest' Then s := 'textureinterpolation';
      If s = 'cos' Then s := 'textureinterpolation';
      If s = 'biliniar' Then s := 'textureinterpolation';
      If s = 'triliniar' Then s := 'textureinterpolation';
      If s = 'clamp' Then s := 'lightoverflowmode';
      If s = 'scale' Then s := 'lightoverflowmode';
      If s = 'repeat' Then s := 'textureparameter';
      If s = 'front' Then s := 'cullfacemode';
      If s = 'back' Then s := 'cullfacemode';
      For i := 0 To Form4.TreeView1.Items.Count - 1 Do
        If lowercase(Form4.TreeView1.Items[i].Text) = s Then Begin
          Form4.TreeView1.Items[i].Selected := True;
          Form4.TreeView1Click(Nil);
          form4.show;
          break;
        End;
    End
    Else Begin
      Form4.TreeView1.Items[0].Selected := True;
      Form4.TreeView1Click(Nil);
      form4.show;
    End;
  End;
End;

End.

