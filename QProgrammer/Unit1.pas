(******************************************************************************)
(* Q-Programmer                                                    ??.??.???? *)
(*                                                                            *)
(* Version     : 0.07                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : is a interpreter with integrated testsystem to solve easy    *)
(*               puzzles for beginners                                        *)
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
(* History     : 0.01 - 0.04 Initialversion / Unknown                         *)
(*               0.05 - Bugfix function ohne Parameter aber mit () wurden     *)
(*                        fehlerhaft geparst                                  *)
(*               0.06 - Genmathcalc unterstützt nun Unäres Minus              *)
(*               0.07 - Bugfix beim Parsen von Ausdrücken ohne Parameter      *)
(*                                                                            *)
(******************************************************************************)
(*

Bekannte Bugs :
- Tinterpreter.CheckVarname nicht implementiert

*)
Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, utokenizer, uinterpreter, ExtCtrls, LResources,
  SynEdit, SynHighlighterPas, lcltype;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Label1: TLabel;
    PopupMenu1: TPopupMenu;
    LoadSource1: TMenuItem;
    SaveSource1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button2: TButton;
    Compilieren1: TMenuItem;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    OpenDialog2: TOpenDialog;
    Panel1: TPanel;
    Splitter1: TSplitter;
    ListBox1: TListBox;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    Testlauf1: TMenuItem;
    Button1: TButton;
    Button5: TButton;
    Button9: TButton;
    Procedure FormCreate(Sender: TObject);
    Procedure LoadSource1Click(Sender: TObject);
    Procedure SaveSource1Click(Sender: TObject);
    Procedure Compilieren1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure Button3Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Testlauf1Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure SynEdit1KeyPress(Sender: TObject; Var Key: Char);
    Procedure SynEdit1Change(Sender: TObject);
  private
    { Private-Deklarationen }

  public
    { Public-Deklarationen }
    Interpreter: Tinterpreter;
  End;

  TTestCase = Record
    name: String;
    case_: String;
    erg: String;
  End;

  TTestcasearr = Array Of TTestCase;

Var
  Testcases: TTestcasearr;
  Form1: TForm1;
  Fehler: Boolean;
  changed_: Boolean = false;

Implementation

Uses Unit2, Unit3, Unit4;

{$R *.lfm}

Function toCase(Value: String): TTestCase;
Begin
  result.name := copy(value, 1, pos(':', value) - 1);
  delete(Value, 1, pos(':', value));
  result.case_ := copy(value, 1, pos(':', value) - 1);
  delete(Value, 1, pos(':', value));
  result.erg := value;
End;

Function TokenstoString(Const value: TTokenarray): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 0 To high(Value) Do Begin
    result := result + value[i].Value + ' ' + inttostr(value[i].Line) + LineEnding;
  End;

End;

// Zum finden des Monitors auf dem sich die Maus befindet

Function PointInRect(P: TPoint; {classes.} R: TRect): boolean;
Var
  t: Integer;
Begin
  If r.left > r.right Then Begin
    t := r.left;
    r.left := r.right;
    r.right := t;
  End;
  If r.top > r.bottom Then Begin
    t := r.Bottom;
    r.bottom := r.top;
    r.top := t;
  End;
  result := (r.left <= p.x) And (r.right >= p.x) And
    (r.top <= p.y) And (r.bottom >= p.y);
End;

// Dann bei OnCreate des Formulars

Procedure TForm1.FormCreate(Sender: TObject);
Var
{$IFNDEF WINDOWS}
  i: Integer;
  r: classes.Trect;
{$ENDIF}
  s: String;
Begin

  (*
   Bei einem Multimonitorsystem wollen wir die Anwendung immer da starten wo der Mauscursor ist.
   *)
{$IFNDEF WINDOWS}
  If screen.MonitorCount <> 1 Then Begin
    For i := 0 To screen.MonitorCount - 1 Do Begin
      r := screen.Monitors[i].BoundsRect;
      If PointInRect(Mouse.CursorPos, r) Then Begin
        left := (screen.Monitors[i].width - form1.width) Div 2 + screen.Monitors[i].BoundsRect.left;
        top := (screen.Monitors[i].height - form1.height) Div 2 + screen.Monitors[i].BoundsRect.top;
        break;
      End;
    End;
  End
  Else Begin
    left := (screen.width - form1.width) Div 2;
    top := (screen.height - form1.height) Div 2;
  End;
{$ELSE}
  left := (screen.width - form1.width) Div 2;
  top := (screen.height - form1.height) Div 2;
{$ENDIF}
  // Weiter mit OnCreate Code
  setlength(Testcases, 0);
  caption := 'QProgrammer ver 0.07 created by Corpsman | support : www.Corpsman.de';
  //  label1.caption := 'Function Fibonacci(N: Integer): Integer;'; // Debugg AN
  label1.caption := 'Please load a task'; // Debugg AUS
  SynEdit1.text := ''; // Debugg AUS
  s := ExtractFilePath(paramstr(0));
  OpenDialog1.InitialDir := s;
  OpenDialog2.InitialDir := s;
  saveDialog1.InitialDir := s;
  Interpreter := TInterpreter.create;
  Splitter1.visible := False;
  Listbox1.visible := False;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If canclose Then Begin
    Interpreter.free;
  End;
End;

Procedure TForm1.LoadSource1Click(Sender: TObject);
Begin
  If opendialog1.execute Then Begin
    opendialog1.InitialDir := ExtractFilePath(opendialog1.FileName);
    SynEdit1.Lines.LoadFromFile(opendialog1.FileName);
    changed_ := false;
  End;
End;

Procedure TForm1.SaveSource1Click(Sender: TObject);
Begin
  If savedialog1.execute Then Begin
    savedialog1.InitialDir := ExtractFilePath(savedialog1.FileName);
    SynEdit1.Lines.SavetoFile(savedialog1.FileName);
    changed_ := False;
  End;
End;

Procedure TForm1.Compilieren1Click(Sender: TObject);
Begin
  button2.onclick(Nil);
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  Fehler := Not Interpreter.Compile(label1.caption + LineEnding + SynEdit1.Text);
  If Fehler Then Begin
    splitter1.visible := True;
    Listbox1.visible := True;
    listbox1.items.text := Interpreter.Errormessage;
  End
  Else Begin
    splitter1.visible := false;
    Listbox1.visible := false;
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  f: TFunction;
  i: Integer;
  l: TLabel;
  e: Tedit;
Begin
  // Erst Compilieren
  button2.onclick(Nil);
  If Not Fehler Then Begin
    f := Interpreter.giveFirstFunction As TFunction;
    fname := f.name;
    If assigned(f) Then Begin
      setlength(paras, f.ParamVarcount);
      setlength(names, f.ParamVarcount);
      oi := 0;
      For i := 0 To high(paras) Do Begin
        paras[i] := '';
        names[i] := f.Params[i].Name + '[' + VarTypeToStr(f.Params[i].Typ) + ']';
      End;
      Form2.ScrollBar1.Visible := false;
      Case f.ParamVarcount Of
        0..6: Begin
            For i := 0 To 5 Do Begin
              l := TLabel(form2.findcomponent('Label' + inttostr(i + 1)));
              e := Tedit(form2.findcomponent('Edit' + inttostr(i + 1)));
              If f.ParamVarcount - 1 >= i Then Begin
                l.visible := True;
                e.visible := True;
                //                e.text := ''; // Debugg
                l.caption := f.Params[i].Name + '[' + VarTypeToStr(f.Params[i].Typ) + ']';
              End
              Else Begin
                l.visible := False;
                e.visible := false;
              End;
            End;
          End
      Else Begin
          Form2.ScrollBar1.Visible := True;
          Form2.ScrollBar1.Max := f.ParamVarcount - 6;
        End;
      End;
      form2.label7.Caption := 'Result : ';
      form2.showmodal;
    End
    Else
      showmessage('Error could not vaild...');
  End;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Var
  s: Tstringlist;
  st, stt: String;
  v, t: Integer;
Begin
  If changed_ Then
    If application.Messagebox(
      pchar('Source was not saved yet. Loading a new task will empty the source field' + LineEnding + 'Would you really want to continue ?'), 'Warning', MB_ICONWARNING Or MB_YESNO) = ID_NO Then
      exit;
  If opendialog2.execute Then Begin
    s := TStringList.create;
    s.LoadFromFile(opendialog2.FileName);
    // Ausgabe der Aufgabenstellung
    v := pos('{', s.text);
    t := pos('}', s.text);
    st := copy(s.text, v + 1, t - v - 1);
    //    showmessage(st);
    form4.memo1.text := st;
    form4.show;
    stt := s.text;
    delete(stt, 1, length(st));
    v := pos('function', lowercase(stt));
    delete(stt, 1, v - 1);
    s.text := stt;
    label1.caption := s[0];
    setlength(Testcases, s.count - 1);
    For t := 1 To s.count - 1 Do Begin
      Testcases[t - 1] := toCase(s[t]);
    End;
    s.free;
    synedit1.text := '';
    synedit1.Lines.add('begin');
    synedit1.Lines.add(' result := ??;');
    synedit1.Lines.add('end;');
  End;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  c, i, j: Integer;
  s, Ausgabe: String;
  erg: String;
  Fname: String;
  f: TFunction;
  paras: TSringArray;
Begin
  Button2.onclick(Nil);
  If Not Fehler Then Begin
    If (high(Testcases) <> -1) Then Begin
      Ausgabe := 'The following teststeps failed :' + LineEnding;
      c := 0;
      f := Interpreter.giveFirstFunction As TFunction;
      paras := Nil;
      setlength(paras, f.ParamVarcount);
      fname := f.name;
      For i := 0 To high(Testcases) Do Begin
        // Auslesen des Testcases in die Parameterliste
        s := trim(Testcases[i].case_);
        If length(s) <> 0 Then Begin
          delete(s, 1, 1);
          s[length(s)] := ',';
          For j := 0 To high(paras) Do Begin
            paras[j] := copy(s, 1, pos(',', s) - 1);
            delete(s, 1, pos(',', s));
          End;
          // Aufruf der Fnuction
          erg := Interpreter.CallFunction(fname, Paras);
          // Prüfen der Ergebnisse und Ausgabe
          If lowercase(Erg) <> lowercase(Testcases[i].erg) Then Begin
            inc(c);
            Ausgabe := Ausgabe + LineEnding +
              Testcases[i].name + ' , Parameters : "' + Testcases[i].case_ + '" results to : "' + erg + '" , this is not as expected';
          End;
        End;
      End;
      If c = 0 Then Begin
        Ausgabe := 'Congratulation all ' + inttostr(high(testcases) + 1) + ' test cases could be validated.';
      End
      Else
        Ausgabe := Ausgabe + LineEnding + LineEnding +
          inttostr(high(testcases) + 1 - c) + ' of ' + inttostr(high(testcases) + 1) + ' test cases have been correct.';
    End
    Else
      Ausgabe := 'No test cases defined, please load a task.';
    showmessage(Ausgabe);
  End;
End;

Procedure TForm1.Testlauf1Click(Sender: TObject);
Begin
  button7.onclick(Nil);
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  form3.showmodal;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  form4.show;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  LoadSource1Click(Nil);
End;

Procedure TForm1.Button9Click(Sender: TObject);
Begin
  SaveSource1Click(Nil);
End;

Procedure TForm1.SynEdit1KeyPress(Sender: TObject; Var Key: Char);
Begin
  changed_ := True;
End;

Procedure TForm1.SynEdit1Change(Sender: TObject);
Begin
  changed_ := True;
End;

End.

