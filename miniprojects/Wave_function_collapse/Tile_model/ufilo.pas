(******************************************************************************)
(* ufilo.pas                                                       12.06.2005 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implements a First in last out element                       *)
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
(*               0.02 - Umstellen auf Generics                                *)
(*                                                                            *)
(******************************************************************************)
Unit ufilo;

{$MODE objfpc}{$H+}

Interface

Uses sysutils; // Für die Exception

Type

  { TFiLo }

  generic TFiLo < T > = Class
  private
    { Private-Deklarationen }
    Type
    // Pointer des stacks
      PStackP = ^TStackT;

      // Ein Stack Element
      TStackT = Record
        value: T;
        next: PStackP;
      End;
    // Pointer auf Oberstes Element
    var
      fTopItem: PStackP;
      fcount:integer;
  public
    { Public-Deklarationen }
    Property Count:integer read fcount;
    // Initialisieren
    Constructor create;
    // Freigeben
    Destructor Destroy; override;
    // Leeren
    Procedure Clear;
    // Hinzufügen eines Wertes
    Procedure Push(Value: T);
    // Rückgabe des Obersten Elementes und Löschen
    Function Pop: T;
    // Rückgabe des Obersten Elements
    Function Top: T;
    // Gibt True zurück wenn Leer
    Function IsEmpty: Boolean;
  End;

  // Die Exeption
  StackException = Class(Exception);

Implementation

constructor TFiLo.create;
Begin
  fTopItem := Nil;
  fcount := 0;
End;

destructor TFiLo.Destroy;
Begin
  Clear;
End;

function TFiLo.IsEmpty: Boolean;
Begin
  result := Nil = fTopItem;
End;

procedure TFiLo.Clear;
Var
  v, v2: PStackP;
Begin
  v := fTopitem;
  While v <> Nil Do Begin
    v2 := v;
    v := v^.next;
    dispose(v2);
  End;
  fTopitem := Nil;
  fcount := 0;
End;

procedure TFiLo.Push(Value: T);
Var
  v: PStackP;
Begin
  inc(fcount);
  new(v);
  v^.value := Value;
  V^.next := fTopitem;
  fTopitem := v;
End;

function TFiLo.Top: T;
Begin
  // wird von einer Leeren Schlange Gepoppt dann Exception
  If fTopItem = Nil Then Begin
    Raise StackException.create('Error Stack Empty');
  End
  Else Begin
    // Rückgabe des Wertes
    result := fTopItem^.value;
  End;
End;

function TFiLo.Pop: T;
Var
  b: PStackP;
Begin
  // wird von einer Leeren Schlange Gepoppt dann Exception
  If fTopItem = Nil Then Begin
    Raise StackException.create('Error Stack Empty');
  End
  Else Begin
    dec(fcount);
    // Rückgabe des Wertes
    result := fTopItem^.value;
    // Löschen des Knoten in dem Stack
    b := fTopItem;
    fTopItem := fTopItem^.Next;
    // Freigeben des Speichers
    Dispose(b);
  End;
End;

End.

