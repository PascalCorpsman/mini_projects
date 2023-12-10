Unit Unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uChunkmanager;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    fconnection: TChunkManager;

  public

    Procedure Init(Const Connection: TChunkManager; Participants: Array Of String);

  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Uses md5, ulanchatcommon;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Server settings';
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
End;

Procedure TForm4.Button1Click(Sender: TObject);
Var
  data: TMemoryStream;
  pwHash: String;
Begin
  If (trim(Edit1.Text) = '') Or
    (trim(Edit2.Text) = '') Or
    (trim(Edit3.Text) = '') Then Begin
    showmessage('Error, empty passwords are not allowed.');
    exit;
  End;
  If trim(edit2.text) <> trim(edit3.text) Then Begin
    showmessage('Error, new password is different to password repetition');
    exit;
  End;
  data := TMemoryStream.Create;
  pwHash := MD5Print(MD5String(edit1.text));
  data.WriteAnsiString(pwHash);
  pwHash := MD5Print(MD5String(edit2.text));
  data.WriteAnsiString(pwHash);
  fconnection.SendChunk(MSG_Change_Password, data);
End;

Procedure TForm4.Button2Click(Sender: TObject);
Var
  pw, pwhash: String;
  data: TMemoryStream;
Begin
  // Remove Participant
  If ListBox1.ItemIndex = -1 Then Begin
    showmessage('Error, no participant selected.');
    exit;
  End;
  pw := PasswordBox('Action', 'Please enter server password to remove participant.');
  pwhash := MD5Print(MD5String(pw));
  data := TMemoryStream.Create;
  data.WriteAnsiString(pwhash);
  data.WriteAnsiString(ListBox1.items[ListBox1.ItemIndex]);
  fconnection.SendChunk(MSG_Remove_Known_Participant, data);
End;

Procedure TForm4.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm4.Init(Const Connection: TChunkManager;
  Participants: Array Of String);
var
  i: Integer;
Begin
  fconnection := Connection;
  Edit1.Text := '';
  Edit2.Text := '';
  Edit3.Text := '';
  ListBox1.Clear;
  For i := 0 To high(Participants) Do Begin
    ListBox1.Items.Add(Participants[i]);
  End;
End;

End.

