(******************************************************************************)
(* Bingo                                                           27.01.2025 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
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
(*               0.02 - customizable max count of numbers                     *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls;

Const
  MaxNumber: integer = 75;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses
  math
  , Unit2 // Print dialog
  , Unit3 // Settings
  ;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Bingo ver. 0.01';
  Constraints.MinWidth := 640;
  Constraints.MinHeight := 480;
  randomize;
  MenuItem4Click(Nil);

End;

Procedure TForm1.FormResize(Sender: TObject);
Begin
  label1.OptimalFill := false;
  Application.ProcessMessages;
  label1.OptimalFill := true;
End;

Procedure TForm1.Button1Click(Sender: TObject);

  Procedure Quick(li, re: integer);
  Var
    l, r, p: Integer;
  Begin
    If Li < Re Then Begin
      // Achtung, das Pivotelement darf nur einam vor den While schleifen ausgelesen werden, danach nicht mehr !!
      p := strtoint(ListBox1.Items[Trunc((li + re) / 2)]); // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While strtoint(ListBox1.Items[l]) < p Do
          inc(l);
        While strtoint(ListBox1.Items[r]) > p Do
          dec(r);
        If L <= R Then Begin
          ListBox1.Items.Exchange(l, r);
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Var
  Num: String;
  found: Boolean;
  i: Integer;
Begin
  If ListBox1.Items.Count >= MaxNumber Then Begin
    showmessage('Error, no new numbers possible.');
    exit;
  End;
  // Next Number
  found := true;
  While found Do Begin
    num := inttostr(Random(MaxNumber) + 1);
    found := false;
    For i := 0 To ListBox1.Items.Count - 1 Do Begin
      If ListBox1.Items[i] = Num Then Begin
        found := true;
        break;
      End;
    End;
  End;
  ListBox1.Items.Add(num);
  Quick(0, ListBox1.Items.Count - 1);
  Label1.Caption := num;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // Print Cards
  form2.ShowModal;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Start new Round
  ListBox1.Clear;
  label1.Caption := '';
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  form3.edit1.text := inttostr(MaxNumber);
  If Form3.ShowModal = mrOK Then Begin
    MaxNumber := strtointdef(Form3.edit1.text, 75);
  End;
End;

End.

