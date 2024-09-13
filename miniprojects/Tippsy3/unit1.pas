(******************************************************************************)
(* Tippsy3                                                         ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Program to learn / teach keyboard layouts                    *)
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

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure SetLineNextLine;
    Procedure SaveAktLine;
    Procedure CalculateResults;
    Procedure CalculateTmpResuts;
    Procedure CalculateHiddenResult;
  End;

Const
  ConfigLineCount = 4; // Es gibt 3 Zeilen in denen Konfigurationsinformationen stehen

Var
  Form1: TForm1;
  Unit_Data: TStringlist;
  Unit_Data_Ptr: Integer;
  Written_Data: TStringlist;
  //  Written_Data_Ptr: integer;
  olength: Integer;
  Time: Integer; // Bisher verstrichene Zeit
  maxtime: integer; // Maximal Mögliche Zeit
  UnlockPW: String;
  BlockFinish: Boolean = false;
  Finished: Boolean = false;

Implementation

Uses Unit2, LazUTF8, math;

{$R *.lfm}

{ TForm1 }

Function ToLength(FrontChar, EndChar, Value: String; To_Length: Integer): String;
Begin
  result := value;
  If FrontChar <> '' Then Begin
    While utf8Length(result) < to_Length Do
      result := FrontChar + result;
  End
  Else If EndChar <> '' Then Begin
    While utf8Length(result) < to_Length Do
      result := result + EndChar;
  End
  Else Begin
    Raise exception.Create('Error, invalid parameters, FrontChar or EndChar have to be <> nil');
  End;
End;

Procedure TForm1.CalculateTmpResuts;
Var
  dt: Integer;
  Chars: Integer;
  Errors: Integer;
  i: Integer;
  a, b: String;
Begin
  // Alles Bisher Geschriebene
  dt := time;
  chars := 0;
  Errors := 0;
  For i := 0 To Written_Data.Count - 1 Do Begin
    Chars := Chars + UTF8Length(Unit_Data[i + ConfigLineCount]);
    a := Unit_Data[i + ConfigLineCount];
    b := Written_Data[i];
    While utf8Length(a) <> 0 Do Begin
      If utf8copy(a, 1, 1) <> utf8copy(b, 1, 1) Then
        inc(errors);
      UTF8delete(a, 1, 1);
      UTF8delete(b, 1, 1);
    End;
  End;
  // Die Aktuelle Zeile
  Chars := Chars + Utf8Length(Edit1.text);
  a := Label1.caption + label2.caption;
  b := Edit1.text;
  While utf8Length(b) <> 0 Do Begin
    If utf8copy(a, 1, 1) <> utf8copy(b, 1, 1) Then
      inc(errors);
    UTF8delete(a, 1, 1);
    UTF8delete(b, 1, 1);
  End;
  If Edit1.PasswordChar <> #0 Then Begin
    Label4.caption := 'Time : ' + inttostr((maxtime - dt) Div 60) + ':' + ToLength('0', '', inttostr((maxtime - dt) Mod 60), 2) +
      ' Chars : ' + inttostr(chars) + ' Errors : ' + inttostr(Errors);
  End
  Else Begin
    Label4.caption := 'Time : ' + inttostr(dt Div 60) + ':' + ToLength('0', '', inttostr(dt Mod 60), 2) +
      ' Chars : ' + inttostr(chars) + ' Errors : ' + inttostr(Errors);
  End;
End;

Procedure TForm1.CalculateHiddenResult;
Var
  dt: Integer;
  rChars: integer;
  Chars: Integer;
  rErrors, Errors: Integer;
  i, j: Integer;
  a, b: String;
Begin
  Finished := True;
  // Alles Bisher Geschriebene
  dt := time;
  chars := 0;
  Errors := 0;
  //  i := Written_Data.Count - 1;
  //  j := Unit_Data.count;
  For i := 0 To Written_Data.Count - 1 Do Begin
    Chars := Chars + UTF8Length(Unit_Data[i + ConfigLineCount]);
    a := Unit_Data[i + ConfigLineCount];
    b := Written_Data[i];
    While utf8Length(a) <> 0 Do Begin
      If utf8copy(a, 1, 1) <> utf8copy(b, 1, 1) Then
        inc(errors);
      UTF8delete(a, 1, 1);
      UTF8delete(b, 1, 1);
    End;
  End;
  // Die Aktuelle Zeile
  Chars := Chars + Utf8Length(Edit1.text);
  a := Label1.caption + label2.caption;
  b := Edit1.text;
  While utf8Length(b) <> 0 Do Begin
    If utf8copy(a, 1, 1) <> utf8copy(b, 1, 1) Then
      inc(errors);
    UTF8delete(a, 1, 1);
    UTF8delete(b, 1, 1);
  End;
  rChars := Chars;
  chars := Chars + utf8length(Label2.caption);
  rerrors := Errors;
  errors := errors + utf8length(Label2.caption);
  // Alle fehlenden Zeilen sind Fehler
  For j := Written_data.Count + 1 To Unit_Data.count - 1 - ConfigLineCount Do Begin
    Chars := Chars + UTF8Length(Unit_Data[j + ConfigLineCount]);
    errors := errors + UTF8Length(Unit_Data[j + ConfigLineCount]);
  End;
  //  form2.Label1.caption := 'Time : ' + inttostr((maxtime - dt) Div 60) + ':' + ToLength('0', '', inttostr((maxtime - dt) Mod 60), 2) +
  //    ' Chars : ' + inttostr(chars) + ' Errors : ' + inttostr(Errors);
  form2.Label1.caption :=
    'Written chars : ' + inttostr(rChars) + #13 +
    'total amount of chars : ' + inttostr(chars) + #13 +
    'errors in written chars : ' + inttostr(rErrors) + #13 +
    'total amount of errors : ' + inttostr(Errors) + #13 +
    'error rate : ' + floattostrf(rErrors / max(1, rchars) * 100, fffixed, 7, 2) + '%' + #13 +
    'total error rate : ' + floattostrf(Errors / max(1, chars) * 100, fffixed, 7, 2) + '%' + #13 +
    'Typing speed : ' + Floattostrf(rchars / max(1, dt), fffixed, 7, 2) + ' symbols per second.'
    ;
End;

Procedure TForm1.CalculateResults;
Var
  dt: Integer;
  Chars: Integer;
  Errors: Integer;
  i: Integer;
  a, b: String;
Begin
  dt := time;
  chars := 0;
  Errors := 0;
  For i := ConfigLineCount To Unit_Data.Count - 1 Do Begin
    Chars := Chars + UTF8Length(Unit_Data[i]);
    a := Unit_Data[i];
    b := Written_Data[i - ConfigLineCount];
    While utf8Length(a) <> 0 Do Begin
      If utf8copy(a, 1, 1) <> utf8copy(b, 1, 1) Then
        inc(errors);
      UTF8delete(a, 1, 1);
      UTF8delete(b, 1, 1);
    End;
  End;
  Showmessage('Total amount of chars : ' + inttostr(chars) + #13 +
    'Total amount of errors : ' + inttostr(Errors) + #13 + #13 +
    'Error rate : ' + floattostrf(Errors / chars * 100, fffixed, 7, 2) + '%' + #13 +
    'Typing speed : ' + Floattostrf(chars / dt, fffixed, 7, 2) + ' symbols per second.'
    );
  If Edit1.PasswordChar <> #0 Then Begin
    // Im Klassenarbeitsmodus
  End
  Else Begin
    // Normal
  End;
End;

Procedure TForm1.SetLineNextLine;
Begin
  // Nächste Zeile
  inc(Unit_data_ptr);
  // Mittendrin statt nur dabei
  If Unit_data_ptr <= Unit_data.Count Then Begin
    Label2.caption := Unit_data[Unit_data_ptr - 1];
    label1.caption := '';
    label2.left := Label1.left + label1.width;
    Edit1.text := '';
    olength := 0;
  End
  Else Begin
    // Der Text ist Abgeschlossen
    Timer1.Enabled := false;
    Edit1.Enabled := false;
    Button1.Enabled := true;
    If Edit1.PasswordChar <> #0 Then Begin
      // Im Klassenarbeitsmodus
      BlockFinish := true;
      Timer1.enabled := false;
      CalculateHiddenResult;
      form2.ShowModal;
    End
    Else Begin
      // Normal
      CalculateResults;
    End;
  End;
End;

Procedure TForm1.SaveAktLine;
Begin
  Written_Data.Add(Edit1.text);
  Edit1.text := '';
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Tippsy 3, ver. 0.01, by Corpsman, support : www.Corpsman.de';
  label4.caption := '';
  Edit1.Enabled := false;
  edit1.text := '';
  Label1.caption := '';
  Label2.caption := 'Press Start to begin.';
  Button1.Enabled := false;
  label1.left := 12;
  label2.left := Label1.left + label1.width;
  OpenDialog1.InitialDir := ExtractFilePath(paramstr(0));
  SaveDialog1.InitialDir := ExtractFilePath(paramstr(0));
  Unit_data := TStringList.create;
  Written_Data := TStringList.create;
End;

Procedure TForm1.Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  s: String;
  //  i: Integer;
Begin
  CalculateTmpResuts;
  If Key = 13 Then Begin
    // Workaround wegen eines Win32 Bugs, unter Linux ist die Variable "Finished" unnötig
    If (utf8length(label2.caption) = 0) And (Not Finished) Then Begin
      // neue Zeile
      SaveAktLine;
      SetLineNextLine;
    End;
  End
  Else Begin
    { If Edit1.PasswordChar <> #0 Then Begin
       // Im Klassenarbeitsmodus gibt es kein "Löschen"
       If (utf8Length(Label2.caption) > 0) And (olength <> utf8length(edit1.text)) Then Begin
         olength := utf8length(edit1.text);
         label1.caption := label1.caption + utf8copy(label2.Caption, 1, 1);
         s := label2.Caption;
         utf8delete(s, 1, 1);
         label2.Caption := s;
         label2.left := Label1.left + label1.width;
       End;
     End
     Else Begin  }
       // Im "Normal" Modus ist alles Erlaubt
    If (olength <> utf8length(edit1.text)) Then Begin
      olength := utf8length(edit1.text);
      s := Label1.caption + Label2.caption;
      label1.caption := utf8copy(s, 1, olength);
      label2.caption := utf8copy(s, olength + 1, length(s));
      label2.left := Label1.left + label1.width;
      //      End;
    End;
  End;
  BlockFinish := false;
  // Wenn das Finish erreicht wurde, während eine Taste gedrückt war.
  If time >= maxtime Then Begin
    dec(time);
    Timer1Timer(Nil);
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  Unit_data.free;
  Written_Data.free;
End;

Procedure TForm1.Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If (key = 8) And (Edit1.PasswordChar = '*') Then key := 0;
  BlockFinish := True;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  Finished := false;
  BlockFinish := false;
  Unit_data_ptr := ConfigLineCount;
  time := 0;
  SetLineNextLine;
  Written_Data.Clear;
  Button1.Enabled := false;
  Edit1.enabled := True;
  Timer1.enabled := True;
  Edit1.SetFocus;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  i: Integer;
  s: String;
Begin
  If OpenDialog1.Execute Then Begin
    Timer1.enabled := false; // falls man in einer Lektion neu startet
    Edit1.text := ''; // falls man in einer Lektion neu startet
    Unit_data.Clear;
    Unit_data.LoadFromFile(OpenDialog1.FileName);
    // Unötige Leerzeichen raus
    For i := 0 To Unit_Data.Count - 1 Do
      Unit_Data[i] := Trim(Unit_Data[i]);
    If Unit_data.Count < ConfigLineCount + 1 Then Begin
      showmessage('Error, invalid unit file, please read the readme.txt');
      exit;
    End;
    // Klassenarbeitsmodus
    If pos('true', lowercase(Unit_data[0])) <> 0 Then Begin
      Edit1.PasswordChar := '*';
    End
    Else Begin
      Edit1.PasswordChar := #0;
    End;
    // Zeitbegrenzung im KA Modus
    s := trim(Unit_data[1]);
    While pos(' ', s) <> 0 Do Begin
      delete(s, pos(' ', s), 1);
    End;
    s := copy(s, pos('=', s) + 1, length(s));
    Try
      maxtime := strtoint(s)
    Except
      maxtime := 0;
    End;
    // Unlock Passwort für den KA Modus
    UnlockPW := copy(Unit_data[2], pos('=', Unit_data[2]) + 1, length(Unit_data[2]));
    UnlockPW := trim(UnlockPW);
    // Farbig Modus
    If pos('true', lowercase(Unit_data[3])) <> 0 Then Begin
      label1.Font.Color := clgreen;
    End
    Else Begin
      label1.Font.Color := clblack;
    End;
{$IFDEF Windows}
    Label1.Font.Name := 'Courier New';
    Label2.Font.Name := 'Courier New';
    Edit1.Font.Name := 'Courier New';
{$ELSE}
    Label1.Font.Name := 'Monospace';
    Label2.Font.Name := 'Monospace';
    Edit1.Font.Name := 'Monospace';
{$ENDIF}
    Label1.Caption := '';
    label2.Caption := 'Press Start to begin.';
    label2.Left := label1.left;
    label4.caption := '';
    Edit1.enabled := false;
    Button1.Enabled := true;
    Button1.Caption := '&Start';
    Button1.SetFocus;
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Var
  f: TStringList;
  i: Integer;
Begin
  If timer1.enabled Then Begin
    showmessage('Saving only after a lection possible.');
    exit;
  End;
  //  Macht eigentlich nur im KA Modus Sinn..
  If SaveDialog1.Execute Then Begin
    f := TStringList.Create;
    CalculateHiddenResult;
    // Die Info
    f.add(form2.Label1.caption);
    f.add('--------------------------------------------------------------------------------');
    f.add('1. Orig text');
    f.add('2. Written by user');
    f.add('--------------------------------------------------------------------------------');
    // Schreiben der Lektions, bzw Geschriebenen Daten
    For i := 0 To Unit_Data.count - 1 - ConfigLineCount Do Begin
      f.add(Unit_Data[i + ConfigLineCount]);
      If i < Written_Data.count Then Begin
        If Written_Data[i] <> Unit_Data[i + ConfigLineCount] Then Begin
          f.add(ToLength('', ' ', Written_Data[i], 79 - 24) + ' -- Errors in this line.');
        End
        Else Begin
          f.add(Written_Data[i]);
        End;
      End
      Else If i = Written_Data.count Then Begin
        If edit1.text <> Unit_Data[i + ConfigLineCount] Then Begin
          f.add(ToLength('', ' ', edit1.text, 79 - 24) + ' -- Errors in this line.');
        End
        Else Begin
          f.add(edit1.text);
        End;
      End
      Else Begin
        f.add('');
      End;
    End;
    f.SaveToFile(SaveDialog1.FileName);
    f.free;
  End;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  inc(time);
  // Anzeige der Infos
  CalculateTmpResuts;
  // im KA Modus Anhalten !!
  If (Edit1.PasswordChar = '*') And (time >= maxtime) And (Not BlockFinish) And (Not finished) Then Begin
    // Anzeige der Ergebnisse
    Timer1.enabled := false;
    CalculateHiddenResult;
    form2.ShowModal;
    // "Löschen"
    Edit1.Enabled := false;
    Button1.Enabled := true;
  End;
End;

End.

