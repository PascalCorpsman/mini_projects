(******************************************************************************)
(* CRC-Calculator                                                ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
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
(*               0.02 - Einfügen der Routine "ValueToString"                  *)
(*               0.03 - Einfügen der Option Ausgabe CRC Byte reversed         *)
(*               0.04 - CRC32 eingefügt.                                      *)
(*               0.05 - Set Mode Section nach oben gezogen und Optisch        *)
(*                      getrennt                                              *)
(*               0.06 - Fix Orderin set model                                 *)
(*                      Custom Models                                         *)
(*               0.07 - Eingabedaten als Stream erlauben                      *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, math, ucrc, IniFiles;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    Button7: TButton;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure CheckBox10Click(Sender: TObject);
    Procedure CheckBox5Click(Sender: TObject);
    Function convert(Value: int64; InHex: Boolean): String;
    Procedure Button2Click(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure UpdateGenPols(Sender: TObject);
    Function getPolyIndex(Poly: int64): integer;
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form1: TForm1;
  crc: TCRC_Calculator;
  ini: TIniFile;

Implementation

{$R *.lfm}

Function ValueToString(Value: int64; Order: integer; AsHex: Boolean): String;
Var
  i: integer;
  b: Byte;
  t: integer;
Begin
  If AsHex Then Begin
    result := '';
    t := 0;
    If order Mod 8 <> 0 Then t := 1;
    For i := 1 To (order Div 8) + t Do Begin
      b := BYTE(value And $FF);
      result := format('%0.2X', [b]) + result;
      If i <> (order Div 8) + t Then Begin
        result := ' ' + Result;
      End;
      value := value Shr 8;
    End;
  End
  Else Begin
    result := inttostr(value);
  End;
End;

Function HextoIntdef(Value: String; DefValue: integer): integer;
Var
  m: integer;
  c: byte;
Begin
  value := uppercase(value);
  result := 0;
  m := 1;
  While length(value) <> 0 Do Begin
    c := ord(value[length(value)]);
    delete(value, length(value), 1);
    If c In [48..57] Then Begin
      result := result + (c - 48) * m;
    End
    Else Begin
      If c In [65..70] Then Begin
        result := result + (c - 55) * m;
      End
      Else Begin
        result := DefValue;
        exit;
      End;
    End;
    m := m * 16;
  End;
End;

Function inToOut(value: String; isHex: boolean): integer;
Begin
  If isHex Then Begin
    result := HextoIntdef(value, -1);
  End
  Else Begin
    result := strtoint(value);
  End;
End;

{ TForm1 }

Procedure TForm1.UpdateGenPols(Sender: TObject);
Begin
  // Generator Polynom
  ComboBox1.Items.BeginUpdate;
  ComboBox1.Clear;

  // Todo : weitere Einfügen von http://en.wikipedia.org/wiki/Cyclic_redundancy_check
  Case ComboBox2.ItemIndex Of
    0: Begin
        ComboBox1.Items.Add('CRC-1 Parity : ' + convert(1, CheckBox4.Checked));
      End;
    4: Begin
        ComboBox1.Items.Add('CRC-5 : ' + convert(5, CheckBox4.Checked));
      End;
    7: Begin
        ComboBox1.Items.Add('CRC-8 : ' + convert(7, CheckBox4.Checked));
      End;
    15: Begin
        ComboBox1.Items.Add('CRC-16 : ' + convert($8005, CheckBox4.Checked));
        ComboBox1.Items.Add('CRC-CCITT : ' + convert($1021, CheckBox4.Checked));
      End;
    31: Begin
        ComboBox1.Items.Add('CRC-32 : ' + convert($04C11DB7, CheckBox4.Checked));
      End;
  End;
  // Dem User zeigen wie er sein eigenes Generatorpolynom angeben kann
  If length(ComboBox2.Text) <> 0 Then Begin
    crc.Order := strtoint(ComboBox2.Text);
    ComboBox1.Items.Add('Custom : ' + convert(crc.crcInit, CheckBox4.Checked));
  End;
  ComboBox1.Items.EndUpdate;
  ComboBox1.ItemIndex := 0;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  crc := TCRC_Calculator.create();
  ini := TIniFile.Create(GetAppConfigFile(false));
  caption := 'CRC-Calculator ver. 0.07';
  panel1.caption := '';
  Application.Title := caption;
  Constraints.MinHeight := Height;
  Constraints.Minwidth := width;
  Constraints.MaxHeight := Height;
  Constraints.Maxwidth := width;

  // Alle Ausgaben in Hex
  CheckBox1.Checked := true;
  CheckBox3.Checked := true;
  CheckBox4.Checked := true;
  CheckBox5.Checked := true;
  CheckBox8.Checked := true;
  CheckBox10.Checked := true; // Normalerweise werden die Bytes von "Low" nach "High" in den Datenstrom eingefügt

  // Erzeugen der Verfügbaren Bit Breiten
  ComboBox2.Clear;
  For i := 1 To 32 Do Begin
    ComboBox2.Items.Add(inttostr(i));
  End;

  // Die Unterstützen Models, taken from here : http://www.tty1.net/pycrc/crc-models_en.html
  // Online CRC-Validator: http://www.zorc.breitbandkatze.de/crc.html
  ComboBox3.Items.Clear;
  ComboBox3.Items.Add('CRC-5');
  ComboBox3.Items.Add('CRC-8');
  ComboBox3.Items.Add('CRC-16');
  ComboBox3.Items.Add('CRC-16-Modbus');
  ComboBox3.Items.Add('CRC-CCITT');
  ComboBox3.Items.Add('Xmodem');
  ComboBox3.Items.Add('CRC-32');

  // Alle User Spezifischen Modelle
  For i := 0 To ini.ReadInteger('Models', 'Count', 0) - 1 Do Begin
    ComboBox3.Items.Add(ini.ReadString('Models', 'Model' + inttostr(i), ''));
  End;

  // Die Default Eingabe
  edit1.text := '14 02 03 E8 00 16 7B 71'; // CRC-16-Modbus
  // edit1.text := '00 03 01 05 FF 84 00 FD 00 00 4D 5A'; // Xmodem

  ComboBox3.Text := 'CRC-16-Modbus';
  Button7.OnClick(Nil); // Laden des CRC-16

  // Ausgabe Löschen
  Edit3.text := '';
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  // More Modes under : http://www.tty1.net/pycrc/crc-models_en.html
  // Additional you need to modify the OnFormCreate Code.
  Case ComboBox3.Text Of
    'CRC-5': Begin
        ComboBox2.ItemIndex := 5 - 1; // 5 Bit
        UpdateGenPols(Nil);
        ComboBox1.ItemIndex := getPolyIndex($5); // Select Generator Polynom
        CheckBox6.Checked := true; // Reflect in
        edit2.Text := ValueToString($1F, 5, CheckBox8.Checked); // Xor In
        CheckBox7.Checked := true; // Reflect Out
        edit4.Text := ValueToString($1F, 5, CheckBox8.Checked); // Xor Out
        CheckBox9.Checked := false; // Direct => (Order mod 8 = 0)
      End;
    'CRC-8': Begin
        ComboBox2.ItemIndex := 8 - 1; // 8 Bit
        UpdateGenPols(Nil);
        ComboBox1.ItemIndex := getPolyIndex($7); // Select Generator Polynom
        CheckBox6.Checked := false; // Reflect in
        edit2.Text := ValueToString(0, 8, CheckBox8.Checked); // Xor In
        CheckBox7.Checked := false; // Reflect Out
        edit4.Text := ValueToString(0, 8, CheckBox8.Checked); // Xor Out
        CheckBox9.Checked := true; // Direct => (Order mod 8 = 0)
      End;
    'CRC-16': Begin
        ComboBox2.ItemIndex := 16 - 1; // 16 Bit
        UpdateGenPols(Nil);
        ComboBox1.ItemIndex := getPolyIndex($8005); // Select Generator Polynom
        CheckBox6.Checked := true; // Reflect in
        edit2.Text := ValueToString(0, 16, CheckBox8.Checked); // Xor In
        CheckBox7.Checked := true; // Reflect Out
        edit4.Text := ValueToString(0, 16, CheckBox8.Checked); // Xor Out
        CheckBox9.Checked := true; // Direct => (Order mod 8 = 0)
      End;
    'CRC-16-Modbus': Begin
        ComboBox2.ItemIndex := 16 - 1; // 16 Bit
        UpdateGenPols(Nil);
        ComboBox1.ItemIndex := getPolyIndex($8005); // Select Generator Polynom
        CheckBox6.Checked := true; // Reflect in
        edit2.Text := ValueToString($FFFF, 16, CheckBox8.Checked); // Xor In
        CheckBox7.Checked := true; // Reflect Out
        edit4.Text := ValueToString(0, 16, CheckBox8.Checked); // Xor Out
        CheckBox9.Checked := true; // Direct => (Order mod 8 = 0)
      End;
    'CRC-CCITT': Begin
        ComboBox2.ItemIndex := 16 - 1; // 16 Bit
        UpdateGenPols(Nil);
        ComboBox1.ItemIndex := getPolyIndex($1021); // Select Generator Polynom
        CheckBox6.Checked := false; // Reflect in
        edit2.Text := ValueToString($FFFF, 16, CheckBox8.Checked); // Xor In
        CheckBox7.Checked := false; // Reflect Out
        edit4.Text := ValueToString(0, 16, CheckBox8.Checked); // Xor Out
        CheckBox9.Checked := true; // Direct => (Order mod 8 = 0)
      End;
    'Xmodem': Begin
        ComboBox2.ItemIndex := 16 - 1; // 16 Bit
        UpdateGenPols(Nil);
        ComboBox1.ItemIndex := getPolyIndex($1021); // Select Generator Polynom
        CheckBox6.Checked := false; // Reflect in
        edit2.Text := ValueToString(0, 16, CheckBox8.Checked); // Xor In
        CheckBox7.Checked := false; // Reflect Out
        edit4.Text := ValueToString(0, 16, CheckBox8.Checked); // Xor Out
        CheckBox9.Checked := true; // Direct => (Order mod 8 = 0)
      End;
    'CRC-32': Begin
        ComboBox2.ItemIndex := 32 - 1; // 32 Bit
        UpdateGenPols(Nil);
        ComboBox1.ItemIndex := getPolyIndex($04C11DB7); // Select Generator Polynom
        CheckBox6.Checked := true; // Reflect in
        edit2.Text := ValueToString($FFFFFFFF, 32, CheckBox8.Checked); // Xor In
        CheckBox7.Checked := true; // Reflect Out
        edit4.Text := ValueToString($FFFFFFFF, 32, CheckBox8.Checked); // Xor Out
        CheckBox9.Checked := true; // Direct => (Order mod 8 = 0)
      End;
  Else Begin // Alle Modelle die aus der ini Datei geladen werden
      ComboBox2.Text := ini.ReadString(ComboBox3.Text, 'Order', '0');
      CheckBox3.Checked := ini.ReadBool(ComboBox3.Text, 'StartHex', false);
      CheckBox9.Checked := ini.ReadBool(ComboBox3.Text, 'StartDirect', false);
      Edit2.Text := ini.ReadString(ComboBox3.Text, 'StartValue', '');
      CheckBox4.Checked := ini.ReadBool(ComboBox3.Text, 'PolynomHex', false);
      ComboBox1.Text := ini.ReadString(ComboBox3.Text, 'Polynom', '');
      CheckBox6.Checked := ini.ReadBool(ComboBox3.Text, 'ReverseData', false);
      CheckBox7.Checked := ini.ReadBool(ComboBox3.Text, 'ReverseCRC', false);
      CheckBox8.Checked := ini.ReadBool(ComboBox3.Text, 'XORHex', false);
      Edit4.Text := ini.ReadString(ComboBox3.Text, 'XORValue', '');
    End;
  End;
End;

Procedure TForm1.CheckBox10Click(Sender: TObject);
Begin
  If CheckBox10.Checked Then Begin
    label6.caption := 'result is from "Low" byte to "High" byte';
  End
  Else Begin
    label6.caption := 'result is from "High" byte to "Low" byte';
  End;
  edit3.text := '';
End;

Procedure TForm1.CheckBox5Click(Sender: TObject);
Begin
  CheckBox10.Enabled := CheckBox5.Checked;
  label6.Enabled := CheckBox5.Checked;
End;

Function TForm1.getPolyIndex(Poly: int64): integer;
Var
  tmp: integer;
  i, j: integer;
  t, s: String;
  value: int64;
Begin
  // Sucht aus der Combobox1 einen Itemindex, der dem Polygon entspricht
  result := -1;
  For j := 0 To ComboBox1.Items.Count - 1 Do Begin
    // Ausschneiden des Polynom Wertes
    s := ComboBox1.Items[j];
    If pos(':', s) <> 0 Then Begin
      s := copy(s, pos(':', s) + 1, length(s));
    End;
    s := Trim(s);

    If CheckBox4.Checked Then Begin
      value := 0;
      s := s + ' ';
      While s <> '' Do Begin
        t := copy(s, 1, pos(' ', s));
        delete(s, 1, length(t));
        t := trim(t);
        i := inToOut(t, true);
        If (i >= 0) And (i <= 255) Then Begin
          value := (value * 256) + i;
        End
        Else Begin
          showmessage('Error "' + t + '" is a invalid value.');
          exit;
        End;
      End;
    End
    Else Begin
      value := StrToInt64(s);
    End;
    If value = Poly Then Begin
      result := j;
      break;
    End;
  End;
  If result = -1 Then Begin
    tmp := strtoint(ComboBox2.text);
    If tmp Mod 4 = 0 Then Begin
      tmp := tmp Div 4;
    End
    Else Begin
      tmp := (tmp Div 4) + 1;
    End;
    Raise exception.create(format('Error could not find generator polynom "$%0.' + inttostr(tmp) + 'X" in list.', [Poly]));
  End;
End;

Function TForm1.convert(Value: int64; InHex: Boolean): String;
Var
  t: integer;
Begin
  If InHex Then Begin
    result := '';
    While value <> 0 Do Begin
      t := value Mod 256;
      value := value Div 256;
      result := format('%0.2X', [t]) + result;
      If value <> 0 Then
        result := ' ' + result;
    End;
  End
  Else Begin
    result := IntToStr(value);
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  s, pol, t: String;
  oval, crcval, sval, poly: int64;
  i: integer;
  data: TBytes;
  fs: TFileStream;
  ui8: UInt8;
Begin
  edit3.text := '';
  // Extrahieren des Generatorpolynoms
  pol := trim(ComboBox1.Text);
  If pos(':', pol) <> 0 Then Begin
    pol := trim(copy(pol, pos(':', pol) + 1, length(pol)));
  End;
  If CheckBox4.Checked Then Begin
    poly := 0;
    pol := pol + ' ';
    While pol <> '' Do Begin
      t := copy(pol, 1, pos(' ', pol));
      delete(pol, 1, length(t));
      t := trim(t);
      i := inToOut(t, true);
      If (i >= 0) And (i <= 255) Then Begin
        poly := (poly * 256) + i;
      End
      Else Begin
        showmessage('Error "' + t + '" is a invalid value.');
        exit;
      End;
    End;
  End
  Else Begin
    poly := StrToInt64(pol);
  End;
  // Auspacken der Eingabe
  If CheckBox2.Checked Then Begin
    If Not FileExists(edit1.text) Then Begin
      If OpenDialog1.Execute Then Begin
        Edit1.Text := OpenDialog1.FileName;
      End
      Else Begin
        exit;
      End;
    End;
    Try
      fs := TFileStream.Create(Edit1.Text, fmOpenRead);
    Except
      showmessage('Error, could not load: ' + Edit1.Text);
      fs.free;
      exit;
    End;
    setlength(data, fs.Size);
    For i := 0 To high(data) Do Begin
      ui8 := 0;
      fs.Read(ui8, sizeof(ui8));
      data[i] := ui8;
    End;
    fs.free;
  End
  Else Begin
    s := edit1.text + ' ';
    setlength(data, 0);
    While length(s) <> 0 Do Begin
      t := copy(s, 1, pos(' ', s) - 1);
      delete(s, 1, length(t) + 1);
      t := trim(t);
      If t <> '' Then Begin
        If CheckBox1.Checked Then Begin
          i := HextoIntdef(t, -1);
        End
        Else Begin
          i := strtointdef(t, -1);
        End;
        If (i >= 0) And (i <= 255) Then Begin
          setlength(data, high(data) + 2);
          data[high(data)] := i;
        End
        Else Begin
          showmessage('Error "' + t + '" is a invalid value.');
          setlength(data, 0);
          exit;
        End;
      End;
    End;
  End;
  // Auspacken des Startwertes
  sval := 0;
  s := edit2.text + ' ';
  While length(s) <> 0 Do Begin
    t := copy(s, 1, pos(' ', s) - 1);
    delete(s, 1, length(t) + 1);
    t := trim(t);
    If t <> '' Then Begin
      If CheckBox3.Checked Then Begin
        i := HextoIntdef(t, -1);
        sval := (sval Shl 8) + i;
      End
      Else Begin
        sval := strtointdef(t, -1);
      End;
    End;
  End;

  // Auspacken des Xor Out Wertes
  oval := 0;
  s := edit4.text + ' ';
  While length(s) <> 0 Do Begin
    t := copy(s, 1, pos(' ', s) - 1);
    delete(s, 1, length(t) + 1);
    t := trim(t);
    If t <> '' Then Begin
      If CheckBox8.Checked Then Begin
        i := HextoIntdef(t, -1);
        oval := (oval Shl 8) + i;
      End
      Else Begin
        oval := strtointdef(t, -1);
      End;
    End;
  End;

  (*
   * Übernehmen der Gesammelten Daten in die CRC Klasse !!
   *)
  crc.Order := strtoint(ComboBox2.Text); // Order
  crc.Polynom := poly; // Poly
  crc.RefIn := CheckBox6.Checked; // Reflect In
  crc.crcInit := sval; // XOR In
  crc.RefOut := CheckBox7.Checked; // Reflect Out
  crc.CRCXor := oval; // Xor Out

  If CheckBox9.Checked Then // Direct
    crc.Direct := dmDirect
  Else
    crc.Direct := dmNondirect;

  //  Das eigentliche Berechnen des CRC
  crcval := crc.CalculateCRC(data);

  // Ausgeben
  If CheckBox10.Checked And CheckBox5.Checked Then Begin
    // Ausgabe als Hex aber in umgekehrter Byte Reihenfolge
    s := ValueToString(crcval, crc.Order, CheckBox5.Checked);
    t := '';
    For i := Length(s) Downto 0 Do Begin
      If (i = 0) Or (s[i] = ' ') Then Begin
        t := t + trim(copy(s, max(i, 1), length(s)));
        If i <> 0 Then t := t + ' ';
        delete(s, i, length(s));
      End;
    End;
    edit3.text := t;
  End
  Else Begin
    // Ausgabe "Mathematisch" Korrekt
    edit3.text := ValueToString(crcval, crc.Order, CheckBox5.Checked);
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  s: String;
  i: Integer;
Begin
  // Add Model
  s := trim(InputBox('Question', 'Please enter a Modelname', ''));
  If s = '' Then exit;
  // 1. Check ob es den Namen schon gibt, ja => Fehler Raus
  For i := 0 To ComboBox3.Items.Count - 1 Do Begin
    If s = ComboBox3.Items[i] Then Begin
      showmessage('Error, modelname "' + s + '" already in use, please choose a other one.');
      exit;
    End;
  End;
  // 2. Anlegen als weiteren Namen
  i := ini.ReadInteger('Models', 'Count', 0);
  ini.WriteInteger('Models', 'Count', i + 1);
  ini.WriteString('Models', 'Model' + inttostr(i), s);
  // 2.1 Das Eigentliche Model
  ini.writeString(s, 'Order', ComboBox2.Text);
  ini.WriteBool(s, 'StartHex', CheckBox3.Checked);
  ini.WriteBool(s, 'StartDirect', CheckBox9.Checked);
  ini.writeString(s, 'StartValue', Edit2.Text);
  ini.WriteBool(s, 'PolynomHex', CheckBox4.Checked);
  ini.writeString(s, 'Polynom', ComboBox1.Text);
  ini.WriteBool(s, 'ReverseData', CheckBox6.Checked);
  ini.WriteBool(s, 'ReverseCRC', CheckBox7.Checked);
  ini.WriteBool(s, 'XORHex', CheckBox8.Checked);
  ini.writeString(s, 'XORValue', Edit4.Text);
  // 2.2 In die Liste eintragen
  ComboBox3.Items.Add(s);
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  s: String;
  i, j, k: integer;
  found: Boolean;
Begin
  // Delete Model
  s := ComboBox3.Text;
  // 1. Löschen aus der Modelsection
  j := ini.ReadInteger('Models', 'Count', 0);
  found := false;
  For i := 0 To j - 1 Do Begin
    If ini.ReadString('Models', 'Model' + inttostr(i), '') = s Then Begin
      For k := i To j - 2 Do Begin
        ini.WriteString('Models', 'Model' + inttostr(k), ini.ReadString('Models', 'Model' + inttostr(k + 1), ''));
      End;
      ini.DeleteKey('Models', 'Model' + inttostr(j - 1));
      found := True;
      break;
    End;
  End;
  If Not found Then Begin
    Showmessage('Default models can not be deleted.');
    exit;
  End;
  ini.WriteInteger('Models', 'Count', j - 1);
  // 2. Löschen der Section
  ini.EraseSection(s);
  // 3. Löschen aus der Combobox
  For i := 0 To ComboBox3.Items.Count - 1 Do Begin
    If ComboBox3.Items[i] = s Then Begin
      ComboBox3.Items.Delete(i);
      ComboBox3.ItemIndex := i - 1;
      break;
    End;
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  sval, v: int64;
  i: integer;
  t, s: String;
  pol: String;
  poly: Int64;
Begin
  // Extrahieren des Generatorpolynoms
  pol := trim(ComboBox1.Text);
  If pos(':', pol) <> 0 Then Begin
    pol := trim(copy(pol, pos(':', pol) + 1, length(pol)));
  End;
  If CheckBox4.Checked Then Begin
    poly := 0;
    pol := pol + ' ';
    While pol <> '' Do Begin
      t := copy(pol, 1, pos(' ', pol));
      delete(pol, 1, length(t));
      t := trim(t);
      i := inToOut(t, true);
      If (i >= 0) And (i <= 255) Then Begin
        poly := (poly * 256) + i;
      End
      Else Begin
        showmessage('Error "' + t + '" is a invalid value.');
        exit;
      End;
    End;
  End
  Else Begin
    poly := StrToInt64(pol);
  End;
  crc.Polynom := poly;
  crc.Order := strtoint(ComboBox2.text);
  // Auspacken des Startwertes
  sval := 0;
  s := edit2.text + ' ';
  While length(s) <> 0 Do Begin
    t := copy(s, 1, pos(' ', s) - 1);
    delete(s, 1, length(t) + 1);
    t := trim(t);
    If t <> '' Then Begin
      If CheckBox3.Checked Then Begin
        i := HextoIntdef(t, -1);
        sval := (sval Shl 8) + i;
      End
      Else Begin
        sval := strtointdef(t, -1);
      End;
    End;
  End;
  // Umrechnen
  If CheckBox9.Checked Then Begin
    v := crc.ConvertCRCdirect(sval, dmDirect, dmNondirect);
  End
  Else Begin
    v := crc.ConvertCRCdirect(sval, dmNondirect, dmDirect);
  End;
  CheckBox9.Checked := Not CheckBox9.Checked;
  edit2.text := ValueToString(v, crc.Order, CheckBox3.Checked);
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.ComboBox2Change(Sender: TObject);
Begin
  // Meistens ist das Startvalue FFF..
  edit2.Text := convert((1 Shl strtoint(ComboBox2.text)) - 1, CheckBox3.Checked);
  // Meistens ist das XOR Out 0
  edit4.text := convert(0, CheckBox3.Checked);
  UpdateGenPols(Nil);
End;

Procedure TForm1.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Begin
    Button1.OnClick(Nil);
  End;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  ini.free;
  crc.Free;
End;

End.

