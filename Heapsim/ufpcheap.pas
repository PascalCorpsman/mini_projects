(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of heapsim                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ufpcheap;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ubaseHeap;

Type

  { TFPCHeap

    Wrapper Class to adopt the FPC internal mechanisms to access
    through the testing environment
  }
  TFPCHeap = Class(TBaseHeap)
  protected
    Function AllocatedByteCount(P: Pointer): integer; override;
  public
    Procedure ReCreate(); override;
    Constructor Create(Size: uint32); override;
    Function GetMem(size: ptruint): Pointer; override;
    Function FreeMem(P: Pointer): ptruint; override;
  End;

Implementation

{ TFPCHeap }

Function TFPCHeap.AllocatedByteCount(P: Pointer): integer;
Begin
{$WARNING das hier stimmt nicht so ganz, da system.memsize irgend etwas anderes (aber ähnliches) zurückgibt}
  Result := system.MemSize(p);
End;

Procedure TFPCHeap.ReCreate;
Begin
  Inherited ReCreate;
End;

Constructor TFPCHeap.Create(Size: uint32);
Begin
  size := 1;
  Inherited Create(size);
End;

Function TFPCHeap.GetMem(size: ptruint): Pointer;
Begin
  Result := Inherited GetMem(size);
  result := system.GetMem(size);
End;

Function TFPCHeap.FreeMem(P: Pointer): ptruint;
Begin
  Result := Inherited FreeMem(P);
  result := system.Freemem(p);
End;

End.

