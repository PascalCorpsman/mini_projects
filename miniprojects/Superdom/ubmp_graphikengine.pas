(******************************************************************************)
(* ubmp_graphikengine.pas                                          ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description :This unit is like OpenGLGraphikEngine.pas but only for        *)
(*               TBitmap.                                                     *)
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
Unit ubmp_graphikengine;

{$MODE ObjFPC}{$H+}

Interface

(*
Ist dieser Schalter an, werden diverse Informationen in die Konsole geschrieben
 - Anzeige des Allokierten Speichers
 - Anzeige der Anzahl der Allokierten Texturen

*)
{.$DEFINE DEBUGGOUTPUT}


Uses sysutils, Graphics; //, math;

Type

  TGraphikItem = Record
    Image: TBitmap;
    Name: String;
  End;

  { TBMP_GraphicEngine }

  TBMP_GraphicEngine = Class
  private
    Fimages: Array Of TGraphikItem;
  public
    Function Find(Value: String; ExceptionOnNotExists: Boolean = True): TBitmap; // Gibt die Textur wieder die unter dem Namen gespeichert ist. Pfadangaben nur bei doppeldeutigen namen notwendig.
    //    Function LoadGraphik(Filename: String): TBitmap; // Lädt die Graphik und gibt OpenGL Index Zurück
    //    Function LoadAlphaGraphik(FileName, MaskName: String): TBitmap; overload; // Läd eine Alpha Graphik, Luminazdaten der Maske = Alpha, Filename = Interner Name
    //    Function LoadAlphaGraphik(Filename: String; r, g, b: Byte): TBitmap; overload; // Läd eine Alpha Graphik und setzt dabei die werte von r,g,b auf Durchsichtig
    Function LoadAlphaGraphik(Const Bitmap: TBitmap; ImageName: String): Tbitmap;
    Procedure Clear;
    Constructor Create;
    Destructor Destroy; override;
  End;

Var
  BMP_GraphikEngine: TBMP_GraphicEngine = Nil;

Implementation

//Function TBMP_GraphicEngine.LoadAlphaGraphik(Filename: String; r, g,
//  b: Byte): TBitmap;
//Begin
//  // Gibts die file schon ?
//{$IFDEF FPC}
//  filename := ClearString(Filename);
//{$ENDIF}
//  result := Find(Filename, false);
//  If result <> Nil Then exit;
//  // Existiert die File überhaupt ?
//  If Not Fileexists(Filename) Then Begin
//{$IFDEF FPC}
//    writeln('Error unable to find "' + Filename + '"');
//{$ENDIF}
//    Raise exception.create('Error unable to find "' + Filename + '"');
//  End;
//  result := TBitmap.create;
//  result.LoadFromFile(Filename);
//  result.TransparentColor := ToColor(R, g, b);
//  result.Transparent := True;
//  // Übernehmen in die Engine
//  setlength(Fimages, high(Fimages) + 2);
//  Fimages[high(Fimages)].Image := Result;
//  Fimages[high(Fimages)].Name := Filename;
//End;
//
//Function TBMP_GraphicEngine.LoadGraphik(Filename: String): Tbitmap;
//Begin
//  // Gibts die file schon ?
//{$IFDEF FPC}
//  filename := ClearString(filename);
//{$ENDIF}
//  result := Find(Filename, false);
//  If result <> Nil Then exit;
//  // Existiert die File überhaupt ?
//  If Not Fileexists(Filename) Then Begin
//{$IFDEF FPC}
//    writeln('Error unable to find "' + Filename + '"');
//{$ENDIF}
//    Raise exception.create('Error unable to find "' + Filename + '"');
//  End;
//  result := TBitmap.create;
//  result.LoadFromFile(Filename);
//  // Übernehmen in die Engine
//  setlength(Fimages, high(Fimages) + 2);
//  Fimages[high(Fimages)].Image := Result;
//  Fimages[high(Fimages)].Name := Filename;
//End;

{ TBMP_GraphicEngine }

Constructor TBMP_GraphicEngine.Create;
Begin
  Inherited create;
  setlength(Fimages, 0);
End;

Destructor TBMP_GraphicEngine.Destroy;
Begin
  clear;
End;

Function TBMP_GraphicEngine.Find(Value: String; ExceptionOnNotExists: Boolean
  ): TBitmap;
Var
  i: Integer;
Begin
  result := Nil;
  Value := LowerCase(Value);
  For i := 0 To high(Fimages) Do
    If pos(value, Fimages[i].Name) <> 0 Then Begin
      result := Fimages[i].Image;
      exit;
    End;
  If ExceptionOnNotExists Then Begin
{$IFDEF DEBUGGOUTPUT}
    writeln('Error Could not Find "' + Value + '" in List.');
{$ENDIF}
    Raise Exception.create('Error Could not Find "' + Value + '" in List.');
  End;
End;

Function TBMP_GraphicEngine.LoadAlphaGraphik(Const Bitmap: TBitmap;
  ImageName: String): Tbitmap;
Var
  b: Tbitmap;
Begin
  result := find(ImageName, false);
  If Not assigned(result) Then Begin
    b := TBitmap.Create;
    b.Width := bitmap.Width;
    b.Height := bitmap.Height;
    bitmap.Transparent := false;
    b.Canvas.Draw(0, 0, bitmap);
    b.TransparentColor := clfuchsia;
    b.Transparent := true;
    setlength(Fimages, high(Fimages) + 2);
    Fimages[high(Fimages)].Image := b;
    Fimages[high(Fimages)].Name := lowercase(ImageName);
  End;
End;

Procedure TBMP_GraphicEngine.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(Fimages) Do
    If Fimages[i].image <> Nil Then
      Fimages[i].image.free;
  setlength(Fimages, 0);
End;

Initialization

  BMP_GraphikEngine := TBMP_GraphicEngine.create;

Finalization

  If assigned(BMP_GraphikEngine) Then Begin
    BMP_GraphikEngine.free;
    BMP_GraphikEngine := Nil;
  End;

End.

