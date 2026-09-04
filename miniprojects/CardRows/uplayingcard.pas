(******************************************************************************)
(* uplayingcard.pas                                                25.04.2011 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : component for displaying a playingcard. This component is    *)
(*               rewritten / ported from old Delphi 2.0 to Lazarus            *)
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
Unit uplayingcard;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

Const
  CardWidth = 71;
  CardHeight = 96;

Type
  EPlayingCard = Class(Exception);

  TShowOption = (soFace, soTool, soBack);

  TPlayingCard = Class(TGraphicControl)
  private
    FFaceIndex: Integer;
    FToolIndex: Integer;
    FBackIndex: Integer;
    FValue: Integer;
    FSuit: Integer;
    FShow: TShowOption;
    FSelected: Boolean;
    Procedure UpdateFaceIndex;
  protected
    Procedure SetFaceIndex(AnIndex: Integer);
    Procedure SetToolIndex(AnIndex: Integer);
    Procedure SetBackIndex(AnIndex: Integer);
    Procedure SetValue(AValue: Integer);
    Procedure SetSuit(ASuit: Integer);
    Procedure SetShow(AnOption: TShowOption);
    Procedure SetSelected(AValue: Boolean);
    Function GetToolCount: Integer;
    Function GetBackCount: Integer;
    Procedure Paint; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Function AddTool(Bitmap: TBitmap): Integer;
    Function LoadTool(Const Filename: String): Integer;
    Function AddBack(Bitmap: TBitmap): Integer;
    Function LoadBack(Const Filename: String): Integer;
    Property ToolCount: Integer read GetToolCount;
    Property BackCount: Integer read GetBackCount;
  published
    {New properties}
    Property FaceIndex: Integer read FFaceIndex write SetFaceIndex; // 0 .. 53
    Property ToolIndex: Integer read FToolIndex write SetToolIndex;
    Property BackIndex: Integer read FBackIndex write SetBackIndex;
    Property Value: Integer read FValue write SetValue; // 1 = Ass, 13 = King
    Property Suit: Integer read FSuit write SetSuit;
    Property Show: TShowOption read FShow write SetShow;
    Property Selected: Boolean read FSelected write SetSelected;
    {Existing properties}
    Property DragCursor;
    Property DragMode;
    Property ParentShowHint;
    Property PopupMenu;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    //    {$IFDEF WIN32}
    //    property OnStartDrag;
    //    {$ENDIF}
    Property Align;
    Property ShowHint;
    Property Visible;
    Property Enabled;
    Property Left;
    Property Top;
    Property Width;
    Property Height;
    Property Cursor;
    Property Hint;
  End;

  (*
  Es Scheint als seien diese Proceduren Rausgeführt, um dem User die "Erweiterung" der Kartendecks zu Ermöglichen
  *)

  // Die "Tools" was auch immer das sein soll
Function AddCardTool(Bitmap: TBitmap): Integer;
Function LoadCardTool(Const Filename: String): Integer;
Function GetCardTool(Index: Integer): TBitmap;
Function GetCardToolCount: Integer;

// Die Rückseiten der Karten
Function AddCardBack(Bitmap: TBitmap): Integer;
Function LoadCardBack(Const Filename: String): Integer;
Function GetCardBack(Index: Integer): TBitmap;
Function GetCardBackCount: Integer;

// Rendert eine Kartenseite auf ein Canvas
Procedure DrawCardFace(Index: Integer; Canvas: TCanvas; CRect: TRect; Invert: Boolean);
Procedure DrawCardBack(Index: Integer; Canvas: TCanvas; CRect: TRect; Invert: Boolean);
Procedure DrawCardTool(Index: Integer; Canvas: TCanvas; CRect: TRect; Invert: Boolean);

Procedure Register;

Implementation

Procedure Register;
Begin
  RegisterComponents('Additional', [TPlayingCard]);
End;

{$R uplayingcard.rc}

(*
Durch das Globale Laden, werden die Graphiken nur 1 mal im Speicher gehalten, egal wie viele TPlayingcards instanziiert werden.
*)
Var
  CardFaces, CardBacks, CardTools: TList;
  CardMask, Work1Bmp, Work2Bmp: TBitmap;

Function AddCardFace(Bitmap: TBitmap): Integer;
Begin
  Result := CardFaces.Add(Bitmap);
End;

Function GetCardFaceCount: Integer;
Begin
  Result := CardFaces.Count;
End;

Function GetCardFace(Index: Integer): TBitmap;
Begin
  If (Index < 0) Or (Index >= GetCardFaceCount) Then
    Raise EPlayingCard.Create('Face index out of range');
  Result := TBitmap(CardFaces[Index]);
End;

Function AddCardTool(Bitmap: TBitmap): Integer;
Begin
  Result := CardTools.Add(Bitmap);
End;

Function LoadCardTool(Const Filename: String): Integer;
Var
  Bitmap: TBitmap;
Begin
  Bitmap := TBitmap.Create;
  Try
    Bitmap.LoadFromFile(Filename);
    Result := AddCardTool(Bitmap);
  Except
    Bitmap.Free;
  End;
End;

Function GetCardToolCount: Integer;
Begin
  Result := CardTools.Count;
End;

Function GetCardTool(Index: Integer): TBitmap;
Begin
  If (Index < 0) Or (Index >= GetCardToolCount) Then
    Raise EPlayingCard.Create('Tool index out of range');
  Result := TBitmap(CardTools[Index]);
End;

Function AddCardBack(Bitmap: TBitmap): Integer;
Begin
  Result := CardBacks.Add(Bitmap);
End;

Function LoadCardBack(Const Filename: String): Integer;
Var
  Bitmap: TBitmap;
Begin
  Bitmap := TBitmap.Create;
  Try
    Bitmap.LoadFromFile(Filename);
    Result := AddCardBack(Bitmap);
  Except
    Bitmap.Free;
  End;
End;

Function GetCardBackCount: Integer;
Begin
  Result := CardBacks.Count;
End;

Function GetCardBack(Index: Integer): TBitmap;
Begin
  If (Index < 0) Or (Index >= GetCardBackCount) Then
    Raise EPlayingCard.Create('Back index out of range');
  Result := TBitmap(CardBacks[Index]);
End;

Procedure DrawCardBitmap(Bmp: TBitmap; Canvas: TCanvas; CRect: TRect; Invert: Boolean);
Var
  WRect, RRect, MRect: TRect;
  OldCopyMode: TCopyMode;
Begin
  WRect := Rect(0, 0, CRect.Right - CRect.Left, CRect.Bottom - CRect.Top);
  Work1Bmp.Width := WRect.Right;
  Work1Bmp.Height := WRect.Bottom;
  Work2Bmp.Width := WRect.Right;
  Work2Bmp.Height := WRect.Bottom;
  RRect := Rect(0, 0, Bmp.Width, Bmp.Height);
  MRect := Rect(0, 0, CardWidth, CardHeight);
  OldCopyMode := Canvas.CopyMode;
  {Copy the inverted mask into Work1}
  Work1Bmp.Canvas.CopyMode := cmNotSrcCopy;
  Work1Bmp.Canvas.CopyRect(WRect, CardMask.Canvas, MRect);
  {Copy the background into Work1, masking out the card area}
  Work1Bmp.Canvas.CopyMode := cmSrcAnd;
  Work1Bmp.Canvas.CopyRect(WRect, Canvas, CRect);
  {Apparent bug means that some resource bitmaps are treated properly only}
  {with SRCCOPY and NOTSRCCOPY. Other raster codes cause strange pixel errors}
  {Copy the card into Work2}
  If Invert Then
    Work2Bmp.Canvas.CopyMode := cmNotSrcCopy
  Else
    Work2Bmp.Canvas.CopyMode := cmSrcCopy;
  Work2Bmp.Canvas.CopyRect(WRect, Bmp.Canvas, RRect);
  {AND the mask into Work2, to remove non-card areas}
  Work2Bmp.Canvas.CopyMode := cmSrcAnd;
  Work2Bmp.Canvas.CopyRect(WRect, CardMask.Canvas, MRect);
  {Merge Work1 into Work2}
  Work2Bmp.Canvas.CopyMode := cmSrcPaint;
  Work2Bmp.Canvas.CopyRect(WRect, Work1Bmp.Canvas, WRect);
  {Copy Work2 onto the canvas}
  Canvas.CopyMode := cmSrcCopy;
  Canvas.CopyRect(CRect, Work2Bmp.Canvas, WRect);
  Canvas.CopyMode := OldCopyMode;
End;

Procedure DrawCardFace(Index: Integer; Canvas: TCanvas; CRect: TRect; Invert: Boolean);
Var
  AdjIndex: Integer;
Begin
  If (Index < 0) Or (Index >= GetCardFaceCount) Then
    AdjIndex := 0
  Else
    AdjIndex := Index;
  DrawCardBitmap(Tbitmap(CardFaces[AdjIndex]), Canvas, CRect, Invert);
End;

Procedure DrawCardBack(Index: Integer; Canvas: TCanvas; CRect: TRect; Invert: Boolean);
Var
  AdjIndex: Integer;
Begin
  If (Index < 0) Or (Index >= GetCardBackCount) Then
    AdjIndex := 0
  Else
    AdjIndex := Index;
  DrawCardBitmap(TBitmap(CardBacks[AdjIndex]), Canvas, CRect, Invert);
End;

Procedure DrawCardTool(Index: Integer; Canvas: TCanvas; CRect: TRect; Invert: Boolean);
Var
  AdjIndex: Integer;
Begin
  If (Index < 0) Or (Index >= GetCardToolCount) Then
    AdjIndex := 0
  Else
    AdjIndex := Index;
  DrawCardBitmap(TBitmap(CardTools[AdjIndex]), Canvas, CRect, Invert);
End;

{TPlayingCard methods}

Constructor TPlayingCard.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  Width := CardWidth;
  Height := CardHeight;
  FShow := soFace;
  FFaceIndex := 1;
  FToolIndex := 0;
  FBackIndex := 0;
  FValue := 1;
  FSuit := 0;
  FSelected := False;
End;

Procedure TPlayingCard.UpdateFaceIndex;
Begin
  If FValue = 0 Then
    Case FSuit Of
      0, 3: FFaceIndex := 0;
      1, 2: FFaceIndex := 53;
    End
  Else Begin
    FFaceIndex := FSuit * 13 + FValue;
  End;
End;

Procedure TPlayingCard.SetFaceIndex(AnIndex: Integer);
Begin
  If AnIndex <> FFaceIndex Then Begin
    If (AnIndex < 0) Or (AnIndex >= GetCardFaceCount) Then
      Raise EPlayingCard.Create('Face index out of range');
    FFaceIndex := AnIndex;
    Case FFaceIndex Of
      0: Begin
          FValue := 0;
          If (FSuit = 0) Or (FSuit = 3) Then
            FSuit := 0;
        End;
      53: Begin
          FValue := 0;
          If (FSuit = 1) Or (FSuit = 2) Then
            FSuit := 1;
        End;
    Else
      FValue := ((FFaceIndex - 1) Mod 13) + 1;
      FSuit := (FFaceIndex - 1) Div 13;
    End;
    If FShow = soFace Then
      Invalidate;
  End;
End;

Procedure TPlayingCard.SetToolIndex(AnIndex: Integer);
Begin
  If AnIndex <> FToolIndex Then Begin
    If (AnIndex < 0) Or (AnIndex >= GetCardToolCount) Then
      Raise EPlayingCard.Create('Tool index out of range');
    FToolIndex := AnIndex;
    If FShow = soTool Then
      Invalidate;
  End;
End;

Procedure TPlayingCard.SetBackIndex(AnIndex: Integer);
Begin
  If AnIndex <> FBackIndex Then Begin
    If (AnIndex < 0) Or (AnIndex >= GetCardBackCount) Then
      Raise EPlayingCard.Create('Back index out of range');
    FBackIndex := AnIndex;
    If FShow = soBack Then
      Invalidate;
  End;
End;

Procedure TPlayingCard.SetValue(AValue: Integer);
Begin
  If AValue <> FValue Then Begin
    If (AValue < 0) Or (AValue > 13) Then
      Raise EPlayingCard.Create('Value out of range');
    FValue := AValue;
    UpdateFaceIndex;
    If FShow = soFace Then
      Invalidate;
  End;
End;

Procedure TPlayingCard.SetSuit(ASuit: Integer);
Begin
  If ASuit <> FSuit Then Begin
    If (ASuit < 0) Or (ASuit > 3) Then
      Raise EPlayingCard.Create('Suit out of range');
    FSuit := ASuit;
    UpdateFaceIndex;
    If FShow = soFace Then
      Invalidate;
  End;
End;

Procedure TPlayingCard.SetShow(AnOption: TShowOption);
Begin
  If AnOption <> FShow Then Begin
    FShow := AnOption;
    Invalidate;
  End;
End;

Procedure TPlayingCard.SetSelected(AValue: Boolean);
Begin
  If AValue <> FSelected Then Begin
    FSelected := AValue;
    Invalidate;
  End;
End;

Function TPlayingCard.AddTool(Bitmap: TBitmap): Integer;
Begin
  Result := AddCardTool(Bitmap);
End;

Function TPlayingCard.LoadTool(Const Filename: String): Integer;
Begin
  Result := LoadCardTool(Filename);
End;

Function TPlayingCard.GetToolCount: Integer;
Begin
  Result := GetCardToolCount;
End;

Function TPlayingCard.AddBack(Bitmap: TBitmap): Integer;
Begin
  Result := AddCardBack(Bitmap);
End;

Function TPlayingCard.LoadBack(Const Filename: String): Integer;
Begin
  Result := LoadCardBack(Filename);
End;

Function TPlayingCard.GetBackCount: Integer;
Begin
  Result := GetCardBackCount;
End;

Procedure TPlayingCard.Paint;
Begin
  Case FShow Of
    soFace: Begin
        DrawCardFace(FFaceIndex, Canvas, ClientRect, FSelected);
      End;
    soTool: Begin
        DrawCardTool(FToolIndex, Canvas, ClientRect, FSelected);
      End;
    soBack: Begin
        DrawCardBack(FBackIndex, Canvas, ClientRect, FSelected);
      End;
  End;
End;

Var
  Index: Integer;
  ResName: String;
  Bitmap: TBitmap;

Initialization

  CardMask := TBitmap.Create;
  CardFaces := TList.Create;
  CardTools := TList.Create;
  CardBacks := TList.Create;
  {Load card faces}
  For Index := 0 To 53 Do Begin
    Bitmap := TBitmap.Create;
    ResName := 'F' + IntToStr(Index);
    Bitmap.LoadFromResourceName(HInstance, ResName);
    AddCardFace(Bitmap);
  End;
  {Load utility faces}
  For Index := 0 To 4 Do Begin
    Bitmap := TBitmap.Create;
    ResName := 'T' + IntToStr(Index);
    Bitmap.LoadFromResourceName(HInstance, ResName);
    AddCardTool(Bitmap);
  End;
  {Load card back images}
  For Index := 0 To 10 Do Begin
    Bitmap := TBitmap.Create;
    ResName := 'B' + IntToStr(Index);
    Bitmap.LoadFromResourceName(HInstance, ResName);
    AddCardBack(Bitmap);
  End;
  {Load the mask bitmap}
  CardMask.LoadFromResourceName(HInstance, 'MASK');
  Work1Bmp := TBitmap.Create;
  Work2Bmp := TBitmap.Create;

Finalization

  For Index := 0 To GetCardFaceCount - 1 Do
    GetCardFace(Index).Free;
  For Index := 0 To GetCardToolCount - 1 Do
    GetCardTool(Index).Free;
  For Index := 0 To GetCardBackCount - 1 Do
    GetCardBack(Index).Free;
  CardMask.Free;
  CardFaces.Free;
  CardTools.Free;
  CardBacks.Free;
  Work1Bmp.Free;
  Work2Bmp.Free;

  (*
   * Wenn hier ein Compilerfehler kommt, das winres nicht gefunden werden kann
   * dann:
   *
   * ACHTUNG, damit das auch unter Linux geht muss folgendes gemacht werden:
   * (Quelle: https://wiki.freepascal.org/Lazarus_Resources)
   *
   *  sudo apt install binutils-mingw-w64-x86-64
   *
   * und dann die /etc/fpc.cfg um die folgnden Zeilen erweitern:

# MS Windows .rc resource compiler
#IFDEF CPUAMD64
-FCx86_64-w64-mingw32-windres
#ENDIF
#IFDEF cpui386
-FCi386-w64-mingw32-windres
#ENDIF

   *)
End.

