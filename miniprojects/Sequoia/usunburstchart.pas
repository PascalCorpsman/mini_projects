(******************************************************************************)
(* uSunburstChart                                                  28.07.2023 *)
(*                                                                            *)
(* Version     : 0.05                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Realises a SunBurstchart that is configurable as much as     *)
(*               possible.                                                    *)
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
(*               0.02 - rewrite with pointers -> more stable and faster.      *)
(*                      load/save                                             *)
(*               0.03 - support multi line captions                           *)
(*                      OnDeleteElementsUserData                              *)
(*               0.04 - OnAfterSegmentPaint                                   *)
(*                      Fix Crash when selecting multiple elements            *)
(*               0.05 - change datatype of TSunBurstChartElement.value to     *)
(*                        uint64                                              *)
(*                                                                            *)
(******************************************************************************)
Unit usunburstchart;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, Graphics, ufifo;

Const
  (*
   * History: 1 = Initialversion
   *          2 = TSunBurstChartElement.value (integer -> uint64)
   *)
  SunBurstChartFileVersion: integer = 2;

Type

  TPointArray = Array Of TPoint;

  TColorSet = Record
    BrushColor: TColor;
    PenColor: TColor;
    PenWitdh: integer;
    FontColor: TColor;
  End;

  (*
   * Needed to give write access to Selected elements ..
   *)
  PSunBurstChartElement = ^TSunBurstChartElement;

  TSunBurstChartElement = Record
    // Everything that a user is allowed to access to
    Caption: String;
    Color: TColorSet;
    SelectedColor: TColorSet;
    Value: UInt64;
    UserData: Pointer;
    Selected: Boolean; // If True, then Selected Brush / Selected will be used (will not be stored!)

    // Everything that the user is not allowed to access (can and will be overwriten by TSunburstChart)
    Child: PSunBurstChartElement; // Pointer to first Child
    PrevSibling: PSunBurstChartElement; // Pointer to previos Sibling (nil if first child)
    NextSibling: PSunBurstChartElement; // Pointer to next Sibling
    Parent: PSunBurstChartElement; // Pointer Back to Parent only needed when deleting Elements
    AbsStartAngle: Single;
    AbsEndAngle: Single;
    Level: Integer; // 0 = most inner circle
    LevelRadius: Single; // is more or less constant, but with this and some constraints the width could be dynamic ..
  End;

  TOnSaveUserData = Procedure(Sender: TObject; Const Stream: TStream; aUserData: Pointer) Of Object;
  TOnLoadUserData = Function(Sender: TObject; Const Stream: TStream): Pointer Of Object;
  TOnDeleteElementsUserData = Procedure(Sender: TObject; aUserData: Pointer) Of Object;
  TOnSegmentPaint = Procedure(Sender: TObject; Const aElement: PSunBurstChartElement) Of Object;

  TPSunBurstChartElementFifo = Specialize TBufferedFifo < PSunBurstChartElement > ;

  { TSunburstChart }

  TSunburstChart = Class(TGraphicControl)
  private
    fRoot: PSunBurstChartElement; // Pointer to Root Element
    fSelectedStack: Array Of PSunBurstChartElement; // only used during Paint
    fSelectedStackCnt: integer; // only used during Paint
    FAngleOffset: Single;
    fInitialArc: Single;
    fPieCenter: Tpoint;
    fPieRadius: Single;
    fLevelMargin: integer;
    fChanged: Boolean;

    // Helper for iterator pattern
    fIterator: PSunBurstChartElement;
    fIteratorFifo: TPSunBurstChartElementFifo;

    (*
     * Variables for callback handlings
     *)
    fOnAfterSegmentPaint: TOnSegmentPaint;
    fOnAfterPaint: TNotifyEvent;
    fOnBeforeSegmentPaint: TOnSegmentPaint;
    fOnDeleteElementsUserData: TOnDeleteElementsUserData; // Only needed if UserData Pointer is <> nil
    fOnLoadUserData: TOnLoadUserData; // Only needed if UserData Pointer is <> nil
    fOnSaveUserData: TOnSaveUserData; // Only needed if UserData Pointer is <> nil

    Procedure setAngleOffset(AValue: Single);
    Procedure SetInitialArc(AValue: Single);
    Procedure SetPieCenter(AValue: Tpoint);

    Function CalcMaxlevelCount(): Integer;

    Procedure RenderElement(Const aElement: PSunBurstChartElement; RenderAll: Boolean);
    Procedure SetPieRadius(AValue: Single);
    Procedure SetLevelMargin(AValue: integer);
    Procedure CalcAllMetaData(); // Calc Level, Angle informations for each Element accessable by Root

    Procedure FreeChild(Var Child: PSunBurstChartElement);

  protected
    Procedure Paint; override;

  public
    Property Changed: Boolean read fChanged;
    Property PieCenter: Tpoint read fPieCenter write SetPieCenter; // According to the compiler this can not be published :-(

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy(); override;

    Function LoadFromStream(Const Stream: TStream): Boolean;
    Function LoadFromFile(aFilename: String): Boolean;
    Function SaveToStream(Const Stream: TStream): Boolean;
    Function SaveToFile(aFilename: String): Boolean;

    Function AddChildElement(Const aParent: PSunBurstChartElement; Child: TSunBurstChartElement): PSunBurstChartElement; // if aParent = nil, child will always be added as sibling to root
    Function AddSiblingElement(Const aElement: PSunBurstChartElement; Sibling: TSunBurstChartElement): PSunBurstChartElement;
    Function DelElement(Const aElement: PSunBurstChartElement): boolean;

    Procedure Clear; // Removes all elements at once

    Procedure DeselectAll; // Iterates through all Childs and "desectes" them (but without changing the iterator values !

    // Gives the Segment which collides with x,y
    // false if none exist
    // Returns a pointer to give write access to the Element!
    Function GetSegmentAtPos(x, y: integer; Out aElement: PSunBurstChartElement): Boolean;

    (*
     * Iterator Pattern to walk all Childs
     *)
    Function Iterator: PSunBurstChartElement;
    Function IterFirst: PSunBurstChartElement;
    Function IterNext: PSunBurstChartElement;

  published
    Property AngleOffset: Single read FAngleOffset write setAngleOffset; // in Radian
    Property Color;
    Property InitialArc: Single read fInitialArc write SetInitialArc; // in Radian
    Property LevelMargin: integer read fLevelMargin write SetLevelMargin;
    Property PieRadius: Single read fPieRadius write SetPieRadius;
    Property ShowHint;

    Property OnAfterSegmentPaint: TOnSegmentPaint read fOnAfterSegmentPaint write fOnAfterSegmentPaint;
    Property OnAfterPaint: TNotifyEvent read fOnAfterPaint write fOnAfterPaint;
    Property OnBeforeSegmentPaint: TOnSegmentPaint read fOnBeforeSegmentPaint write fOnBeforeSegmentPaint;
    Property OnClick;
    Property OnDeleteElementsUserData: TOnDeleteElementsUserData read fOnDeleteElementsUserData write fOnDeleteElementsUserData;
    Property OnDblClick;
    Property OnLoadUserData: TOnLoadUserData read fOnLoadUserData write fOnLoadUserData;
    Property OnMouseUp;
    Property OnMouseMove;
    Property OnMouseDown;
    Property OnSaveUserData: TOnSaveUserData read fOnSaveUserData write fOnSaveUserData;
    Property OnShowHint;
    Property OnResize;
  End;

  (*
   * Use this function to get a proper initialized TSunBurstChartElement !
   *)
Function DefaultSunBurstChartElement(): TSunBurstChartElement;

Implementation

Uses
  math;

Const
  Epsilon = 0.0001;

Procedure Nop(); // Debug only
Begin

End;

Function DefaultSunBurstChartElement(): TSunBurstChartElement;
Begin
  // The values the user is allowed to access to
  result.Caption := '';
  result.Color.BrushColor := clGray;
  result.Color.PenColor := clblack;
  result.Color.PenWitdh := 1;
  result.Color.FontColor := clBlack;
  result.SelectedColor.BrushColor := clNavy;
  result.SelectedColor.PenColor := clred;
  result.SelectedColor.PenWitdh := 4;
  result.SelectedColor.FontColor := clWhite;
  result.Value := 1;
  result.UserData := Nil;
  result.Selected := false;

  // The values the user shall not change / or edit, will be calculated through TSunburstChart
  result.Child := Nil;
  result.PrevSibling := Nil;
  result.NextSibling := Nil;
  result.Parent := Nil;
  result.AbsStartAngle := 0;
  result.AbsEndAngle := 0;
  result.Level := 0;
  result.LevelRadius := 0;
End;

{ TSunburstChart }

Constructor TSunburstChart.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  fOnAfterSegmentPaint := Nil;
  fOnAfterPaint := Nil;
  fOnBeforeSegmentPaint := Nil;
  fOnDeleteElementsUserData := Nil;
  fOnLoadUserData := Nil;
  fOnSaveUserData := Nil;
  fChanged := false;
  Width := 300;
  Height := 300;
  fPieRadius := 140;
  Color := clwhite;
  fPieCenter := point(150, 150);
  fInitialArc := 2 * pi;
  fAngleOffset := 0;
  fLevelMargin := 0;
  fRoot := Nil;
  fSelectedStack := Nil;
  fSelectedStackCnt := 0;
  fIterator := Nil;
  fIteratorFifo := TPSunBurstChartElementFifo.create(16);
End;

Procedure TSunburstChart.Clear;
Begin
  fChanged := false;
  FreeChild(froot);
  fRoot := Nil;
  InitialArc := 2 * pi;
  AngleOffset := 0;
  LevelMargin := 0;
  Color := clwhite;
  Invalidate;
End;

Destructor TSunburstChart.Destroy;
Begin
  Clear();
  setlength(fSelectedStack, 0);
  fIteratorFifo.free;
  Inherited Destroy;
End;

Function TSunburstChart.SaveToStream(Const Stream: TStream): Boolean;

  Function GetSiblingCount(Const aElement: PSunBurstChartElement): Integer;
  Var
    p: PSunBurstChartElement;
  Begin
    result := 0;
    If Not assigned(aElement) Then exit;
    p := aElement;
    While assigned(p) Do Begin
      inc(result);
      p := p^.NextSibling;
    End;
  End;

  Function ElementsToStream(Const aElement: PSunBurstChartElement): Boolean;
  Var
    Siblings: Integer;
    b: UInt8;
    p: PSunBurstChartElement;
  Begin
    If Not Assigned(aElement) Then Begin
      Siblings := 0;
      stream.Write(Siblings, SizeOf(Siblings));
      result := true;
      exit;
    End;
    result := false;
    Siblings := GetSiblingCount(aElement);
    stream.Write(Siblings, SizeOf(Siblings));
    p := aElement;
    While Assigned(p) Do Begin
      stream.WriteAnsiString(p^.Caption);
      stream.Write(p^.Color, SizeOf(p^.Color));
      stream.Write(p^.SelectedColor, SizeOf(p^.SelectedColor));
      stream.Write(p^.Value, SizeOf(p^.Value));
      If assigned(p^.UserData) Then Begin
        b := 1;
        stream.Write(b, SizeOf(b));
        If Not assigned(OnSaveUserData) Then Begin
          Raise exception.create('Error, TChild with defined userdata, but no OnSaveUserData property set.');
          exit;
        End;
        OnSaveUserData(self, Stream, p^.UserData);
      End
      Else Begin
        b := 0;
        stream.Write(b, SizeOf(b));
      End;
      ElementsToStream(p^.Child);
      p := p^.NextSibling;
    End;
    result := true;
  End;

Var
  c: TColor;
Begin
  result := false;
  // Global Settings
  stream.Write(SunBurstChartFileVersion, sizeof(SunBurstChartFileVersion));

  stream.write(FAngleOffset, sizeof(FAngleOffset));
  c := Color;
  stream.write(C, sizeof(C));
  stream.write(fInitialArc, sizeof(fInitialArc));
  stream.write(fPieCenter, sizeof(fPieCenter));
  stream.write(fPieRadius, sizeof(fPieRadius));
  stream.write(fLevelMargin, sizeof(fLevelMargin));

  result := ElementsToStream(fRoot);
  If result Then fChanged := false;
End;

Function TSunburstChart.LoadFromStream(Const Stream: TStream): Boolean;
Var
  LoadedFileVersion: integer;

  Function ElementFromStream(): TSunBurstChartElement;
  Var
    b: uint8;
    i: integer;
  Begin
    result := DefaultSunBurstChartElement();
    result.caption := Stream.ReadAnsiString;
    stream.Read(result.Color, SizeOf(result.Color));
    stream.Read(result.SelectedColor, SizeOf(result.SelectedColor));
    If LoadedFileVersion = 1 Then Begin
      i := 1;
      stream.Read(i, SizeOf(i));
      result.Value := i;
    End
    Else Begin
      stream.Read(result.Value, SizeOf(result.Value));
    End;
    b := 2;
    stream.Read(b, SizeOf(b));
    If b = 1 Then Begin
      If Not assigned(OnLoadUserData) Then Begin
        Raise exception.create('Error, stream with defined userdata, but no OnLoadUserData property set.');
        exit;
      End;
      result.UserData := OnLoadUserData(self, Stream);
    End;
  End;

  Function ElementsFromStream(aParent: PSunBurstChartElement): boolean;
  Var
    Siblings, i: Integer;
    Element: TSunBurstChartElement;
    nParent: PSunBurstChartElement;
  Begin
    result := false;
    Siblings := 0;
    stream.Read(Siblings, SizeOf(Siblings));
    For i := 0 To Siblings - 1 Do Begin
      Element := ElementFromStream();
      nParent := AddChildElement(aParent, Element);
      result := ElementsFromStream(nParent);
      If Not result Then exit;
    End;
    result := true;
  End;

Var
  c: TColor;
Begin
  result := false;
  Clear;
  LoadedFileVersion := 0;
  stream.Read(LoadedFileVersion, sizeof(LoadedFileVersion));
  If (LoadedFileVersion > SunBurstChartFileVersion) Or (LoadedFileVersion <= 0) Then exit; // Da ist offensichtlich was falsch ..

  stream.Read(FAngleOffset, sizeof(FAngleOffset));
  c := Color;
  stream.Read(C, sizeof(C));
  stream.Read(fInitialArc, sizeof(fInitialArc));
  stream.Read(fPieCenter, sizeof(fPieCenter));
  stream.Read(fPieRadius, sizeof(fPieRadius));
  stream.Read(fLevelMargin, sizeof(fLevelMargin));

  result := ElementsFromStream(Nil);
  If result Then fChanged := false;
End;

Function TSunburstChart.SaveToFile(aFilename: String): Boolean;
Var
  fs: TFileStream;
Begin
  result := false;
  Try
    fs := TFileStream.Create(aFilename, fmCreate Or fmOpenWrite);
    result := SaveToStream(fs);
    fs.free;
  Except
    Raise;
    // Nichts, das Result ist ja schon false..
  End;
End;

Function TSunburstChart.LoadFromFile(aFilename: String): Boolean;
Var
  fs: TFileStream;
Begin
  result := false;
  Try
    fs := TFileStream.Create(aFilename, fmOpenRead);
    result := LoadFromStream(fs);
    fs.free;
    Invalidate;
  Except
    Raise;
    // Nichts, das Result ist ja schon false..
  End;
End;

Function TSunburstChart.AddChildElement(Const aParent: PSunBurstChartElement; Child: TSunBurstChartElement
  ): PSunBurstChartElement;
Var
  p: PSunBurstChartElement;
Begin
  result := Nil;
  If aParent = Nil Then Begin
    If assigned(fRoot) Then Begin
      If assigned(fRoot^.NextSibling) Then Begin
        p := fRoot^.NextSibling;
        While assigned(p^.NextSibling) Do Begin
          p := p^.NextSibling;
        End;
        new(p^.NextSibling);
        p^.NextSibling^ := Child;
        p^.NextSibling^.Parent := Nil;
        p^.NextSibling^.PrevSibling := p;
        result := p^.NextSibling;
      End
      Else Begin
        new(fRoot^.NextSibling);
        fRoot^.NextSibling^ := Child;
        fRoot^.NextSibling^.Parent := Nil;
        fRoot^.NextSibling^.PrevSibling := fRoot;
        result := fRoot^.NextSibling;
      End;
    End
    Else Begin
      new(fRoot);
      froot^ := Child;
      froot^.Parent := Nil;
      froot^.PrevSibling := Nil;
      result := fRoot;
    End;
  End
  Else Begin
    If assigned(aParent^.Child) Then Begin
      p := aParent^.Child;
      While assigned(p^.NextSibling) Do Begin
        p := p^.NextSibling;
      End;
      new(p^.NextSibling);
      p^.NextSibling^ := Child;
      p^.NextSibling^.Parent := p^.Parent;
      p^.NextSibling^.PrevSibling := p;
      result := p^.NextSibling;
    End
    Else Begin
      new(aParent^.Child);
      aParent^.Child^ := Child;
      aParent^.Child^.Parent := aParent;
      aParent^.Child^.PrevSibling := Nil;
      result := aParent^.Child;
    End;
  End;
  fChanged := true;
  Invalidate;
End;

Function TSunburstChart.AddSiblingElement(Const aElement: PSunBurstChartElement;
  Sibling: TSunBurstChartElement): PSunBurstChartElement;
Var
  p: PSunBurstChartElement;
Begin
  result := Nil;
  If Not assigned(aElement) Then exit;
  If assigned(aElement^.NextSibling) Then Begin
    p := aElement^.NextSibling;
    While assigned(p^.NextSibling) Do Begin
      p := p^.NextSibling;
    End;
    new(p^.NextSibling);
    p^.NextSibling^ := Sibling;
    p^.NextSibling^.PrevSibling := p;
    p^.NextSibling^.Parent := p^.Parent;
    result := p^.NextSibling;
  End
  Else Begin
    new(aElement^.NextSibling);
    aElement^.NextSibling^ := Sibling;
    aElement^.NextSibling^.PrevSibling := aElement;
    aElement^.NextSibling^.Parent := aElement^.Parent;
    result := aElement^.NextSibling;
  End;
  fChanged := true;
  Invalidate;
End;

Function TSunburstChart.DelElement(Const aElement: PSunBurstChartElement): boolean;
Var
  p: PSunBurstChartElement;
Begin
  result := false;
  If Not assigned(aElement) Then exit;
  If assigned(aElement^.PrevSibling) Then Begin
    // Das Element ist irgendwo mitten in der Siblingkette
    p := aElement;
    // Aushängen des Elementes aus der Siblingkette
    p^.PrevSibling^.NextSibling := p^.NextSibling;
    If assigned(p^.NextSibling) Then Begin
      p^.NextSibling^.PrevSibling := p^.PrevSibling;
    End;
    // Alles Unterhalb fliegt sowieso raus
    FreeChild(p^.Child);
    // Das Eigentliche element Freigeben
    If assigned(p^.UserData) Then Begin
      If assigned(OnDeleteElementsUserData) Then Begin
        OnDeleteElementsUserData(self, p^.UserData);
      End
      Else Begin
        Raise exception.create('Error, freeing PSunBurstChartElement with userdata, but no OnDeleteElementsUserData set.');
      End;
    End;
    dispose(p);
  End
  Else Begin
    // Das Element ist das Erste in der Kette
    If aElement^.Parent = Nil Then Begin
      p := aElement;
      //p^.Parent^.Child := p^.NextSibling; -- Gibt es in der 1. Ebene ja nicht.
      If assigned(p^.NextSibling) Then Begin
        p^.NextSibling^.PrevSibling := Nil;
      End;
      If aElement = fRoot Then froot := froot^.NextSibling;
      FreeChild(p^.Child);
      If assigned(p^.UserData) Then Begin
        If assigned(OnDeleteElementsUserData) Then Begin
          OnDeleteElementsUserData(self, p^.UserData);
        End
        Else Begin
          Raise exception.create('Error, freeing PSunBurstChartElement with userdata, but no OnDeleteElementsUserData set.');
        End;
      End;
      dispose(p);
      Invalidate;
    End
    Else Begin
      p := aElement;
      p^.Parent^.Child := p^.NextSibling;
      If assigned(p^.NextSibling) Then Begin
        p^.NextSibling^.PrevSibling := Nil;
      End;
      FreeChild(p^.Child);
      If assigned(p^.UserData) Then Begin
        If assigned(OnDeleteElementsUserData) Then Begin
          OnDeleteElementsUserData(self, p^.UserData);
        End
        Else Begin
          Raise exception.create('Error, freeing PSunBurstChartElement with userdata, but no OnDeleteElementsUserData set.');
        End;
      End;
      dispose(p);
    End;
  End;
  fChanged := true;
  Invalidate;
End;

Procedure TSunburstChart.DeselectAll;
  Procedure DeSel(Const aElement: PSunBurstChartElement);
  Begin
    If Not assigned(aElement) Then exit;
    aElement^.Selected := false;
    DeSel(aElement^.Child);
    DeSel(aElement^.NextSibling);
  End;

Begin
  Desel(fRoot);
  //fChanged := true; -- Selected flag is not stored so no change !
  Invalidate;
End;

Function TSunburstChart.GetSegmentAtPos(x, y: integer; Out aElement: PSunBurstChartElement
  ): Boolean;

Const
  TwoPi = 2 * Pi;

  Function AngleInRange(Const Angle: Single; StartAngle, EndAngle: Single): boolean;
  Begin
    // Clamp all Angles into the range of 0 .. 2 * pi
    While StartAngle < 0 Do
      StartAngle := StartAngle + TwoPi;
    While StartAngle > TwoPi Do
      StartAngle := StartAngle - TwoPi;
    While EndAngle < 0 Do
      EndAngle := EndAngle + TwoPi;
    While EndAngle > TwoPi + Epsilon Do
      EndAngle := EndAngle - TwoPi;
    If EndAngle < StartAngle Then Begin
      // If the zero crossing is within the Range
      result := (angle >= StartAngle) Or (angle <= EndAngle);
    End
    Else Begin
      result := (Angle >= StartAngle) And (angle <= EndAngle);
    End;
  End;

Var
  Level: Integer;
  angle: Single;

  Function Search(Const asElement: PSunBurstChartElement): Boolean;
  Begin
    result := false;
    If Not assigned(asElement) Then exit;

    result := (asElement^.Level = Level) And AngleInRange(angle, asElement^.AbsStartAngle, asElement^.AbsEndAngle);
    If result Then Begin
      aElement := asElement;
    End
    Else Begin
      If asElement^.Level < Level Then Begin
        result := search(asElement^.Child);
        If result Then exit;
      End;
      If asElement^.Level <= Level Then Begin
        result := search(asElement^.NextSibling);
        If result Then exit;
      End;
    End;
  End;

Var
  tx, ty: integer;
Begin
  result := false;
  aElement := Nil;
  If Not assigned(fRoot) Then exit;
  // 0. Alle Elemente initialisieren, sollten diese noch nie gerendert worden sein
  CalcAllMetaData();
  // 1. Bestimmen der Polarkoordinaten von X,Y
  tx := x - PieCenter.x;
  ty := PieCenter.y - y; // Man Bedenke die Y-Achse ist invertiert damit der Mathematische Drehsinn stimmt ;)
  // TODO: Abfangen Div by 0, wenn fRoot[0].LevelRadius + LevelMargin = 0 !
  Level := trunc(sqrt(sqr(tx) + sqr(ty)) / (fRoot[0].LevelRadius + fLevelMargin)); // Berechnen des Aktuellen Ringes
  angle := ArcTan2(ty, tx); // -pi .. pi
  If angle < 0 Then angle := angle + 2 * pi; // 0 .. 2 * pi (so wie alle internen Koordiaten abgespeichert sind !
  // 2. Suchen ob es ein Segment gibt, welches in diesen Koordinaten liegt !
  result := Search(fRoot);
End;

Function TSunburstChart.Iterator: PSunBurstChartElement;
Begin
  result := fIterator;
End;

Function TSunburstChart.IterFirst: PSunBurstChartElement;
  Procedure Push(Const aElement: PSunBurstChartElement);
  Begin
    If Not Assigned(aElement) Then exit;
    fIteratorFifo.Push(aElement);
    push(aElement^.NextSibling);
    push(aElement^.Child);
  End;

Begin
  // TODO: Not shure if this is the best way of implementing a iteration pattern
  //       as long as nobody deletes a element everything should be fine otherwise
  //       you will get lots of dangling references ..
  fIterator := Nil;
  fIteratorFifo.clear;
  Push(fRoot);
  If fIteratorFifo.Count <> 0 Then Begin
    fIterator := fIteratorFifo.Pop;
  End;
  result := fIterator;
End;

Function TSunburstChart.IterNext: PSunBurstChartElement;
Begin
  If fIteratorFifo.Count <> 0 Then Begin
    fIterator := fIteratorFifo.Pop;
  End
  Else Begin
    fIterator := Nil;
  End;
  result := fIterator;
End;

Procedure TSunburstChart.Paint;
Var
  i: integer;
Begin
  Inherited Paint; // Call the fOnPaint, ifneeded
  // Erase Background
  Canvas.Brush.Color := Color;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(-1, -1, Width + 1, Height + 1);

  If Not assigned(froot) Then exit;
  CalcAllMetaData;

  fSelectedStackCnt := 0;
  RenderElement(froot, True);

  // Re Render all Selected Parts so that their border can overpaint the non selected areas
  For i := 0 To fSelectedStackCnt - 1 Do Begin
    RenderElement(fSelectedStack[i], false);
  End;
  If assigned(fOnAfterPaint) Then Begin
    FonAfterPaint(Self);
  End;
End;

Procedure TSunburstChart.RenderElement(Const aElement: PSunBurstChartElement;
  RenderAll: Boolean);

  Procedure PlotTextAtPos(p: Tpoint);
  Var
    Lines: TStringArray;
    t, i: Integer;
  {$IFDEF Windows}
    s: String;
  {$ENDIF}
  Begin
    If pos(LineEnding, aElement^.Caption) <> 0 Then Begin
{$IFDEF Windows}
      s := aElement^.Caption;
      s := StringReplace(s, #10, '', [rfReplaceAll]);
      lines := s.Split([#13]);
{$ELSE}
      lines := aElement^.Caption.Split([LineEnding]);
{$ENDIF}
      t := p.y - (canvas.TextHeight('8') * length(Lines)) Div 2;
      // TODO: This implements a "center" layout, in theory you could provide left and right as well..
      For i := 0 To high(lines) Do Begin
        canvas.TextOut(
          p.x - (Canvas.TextWidth(lines[i]) Div 2),
          t + i * Canvas.TextHeight('8'),
          lines[i]
          );
      End;
    End
    Else Begin
      canvas.TextOut(
        p.x - Canvas.TextWidth(aElement^.Caption) Div 2,
        p.Y - Canvas.TextHeight(aElement^.Caption) Div 2,
        aElement^.Caption
        );
    End;
  End;

Var
  c, s, a: extended;
  cnt, i: Integer;
  PolyPoints: Array Of TPoint;
  InnerRadius, OuterRadius: Single;
Begin
  If Not assigned(aElement) Then exit;
  If assigned(fOnBeforeSegmentPaint) Then Begin
    fOnBeforeSegmentPaint(self, aElement);
  End;
  PolyPoints := Nil;
  // 1. Rendern des Segmentes
  If aElement^.Selected Then Begin
    Canvas.Brush.Color := aElement^.SelectedColor.BrushColor;
    canvas.Pen.Color := aElement^.SelectedColor.PenColor;
    canvas.Pen.Width := aElement^.SelectedColor.PenWitdh;
    canvas.Font.Color := aElement^.SelectedColor.FontColor;
    If RenderAll Then Begin
      inc(fSelectedStackCnt);
      If fSelectedStackCnt > high(fSelectedStack) Then Begin
        // In case where typically only 1 element is selected
        // This is totally fine. In case with lots and lots selected
        // elements, this is really high cost at the very first rendering ..
        setlength(fSelectedStack, fSelectedStackCnt);
      End;
      fSelectedStack[fSelectedStackCnt - 1] := aElement;
    End;
  End
  Else Begin
    Canvas.Brush.Color := aElement^.Color.BrushColor;
    canvas.Pen.Color := aElement^.Color.PenColor;
    canvas.Pen.Width := aElement^.Color.PenWitdh;
    canvas.Font.Color := aElement^.Color.FontColor;
  End;
  InnerRadius := aElement^.Level * (aElement^.LevelRadius + fLevelMargin);
  OuterRadius := (aElement^.Level + 1) * (aElement^.LevelRadius);
  If InnerRadius = 0 Then Begin
    If abs(aElement^.AbsEndAngle - aElement^.AbsStartAngle - 2 * pi) <= Epsilon Then Begin
      // Ein Vollkreis
      Canvas.Ellipse(
        round(PieCenter.x - OuterRadius),
        round(PieCenter.Y - OuterRadius),
        round(PieCenter.x + OuterRadius),
        round(PieCenter.Y + OuterRadius)
        );
      PlotTextAtPos(PieCenter);
    End
    Else Begin
      // Ein "Torten" Element
      cnt := max(3, round(((aElement^.AbsEndAngle - aElement^.AbsStartAngle) * OuterRadius) / 10));
      setlength(PolyPoints, cnt + 2);
      PolyPoints[0] := PieCenter;
      For i := 0 To cnt Do Begin
        a := (aElement^.AbsEndAngle - aElement^.AbsStartAngle) * i / cnt + aElement^.AbsStartAngle;
        SinCos(a, s, c);
        PolyPoints[i + 1] := point(round(PieCenter.x + OuterRadius * c), round(PieCenter.Y - OuterRadius * s));
      End;
      Canvas.Polygon(PolyPoints);
      a := (aElement^.AbsEndAngle + aElement^.AbsStartAngle) / 2;
      SinCos(a, s, c);
      PlotTextAtPos(point(round(PieCenter.x + OuterRadius * c / 2), round(PieCenter.Y - OuterRadius * s / 2)));
    End;
  End
  Else Begin
    // TODO: Wenn das Segment ein Vollkreis ist solte es als Ring und nicht als Ring mit 0-Linie gemalt werden.
    //       \-> Dieser "Bug" / "glitch" tritt nur auf, wenn BrushColor <> PenColor, sonst stimmt es *g*
    // Ein Segment "außen" also als Ring
    cnt := max(3, round(((aElement^.AbsEndAngle - aElement^.AbsStartAngle) * OuterRadius) / 10));
    setlength(PolyPoints, 2 * (cnt + 1));
    For i := 0 To cnt Do Begin
      a := (aElement^.AbsEndAngle - aElement^.AbsStartAngle) * i / cnt + aElement^.AbsStartAngle;
      SinCos(a, s, c);
      PolyPoints[i] := point(round(PieCenter.x + InnerRadius * c), round(PieCenter.Y - InnerRadius * s));
      PolyPoints[2 * (cnt + 1) - 1 - i] := point(round(PieCenter.x + OuterRadius * c), round(PieCenter.Y - OuterRadius * s));
    End;
    Canvas.Polygon(PolyPoints);
    a := (aElement^.AbsEndAngle + aElement^.AbsStartAngle) / 2;
    SinCos(a, s, c);
    PlotTextAtPos(point(round(PieCenter.x + (InnerRadius + OuterRadius) * c / 2), round(PieCenter.Y - (InnerRadius + OuterRadius) * s / 2)));
  End;
  If assigned(fOnAfterSegmentPaint) Then Begin
    fOnAfterSegmentPaint(self, aElement);
  End;
  // 2. Rekursiver Abstieg
  If RenderAll Then Begin
    RenderElement(aElement^.Child, true);
    RenderElement(aElement^.NextSibling, true);
  End;
End;

Procedure TSunburstChart.SetPieCenter(AValue: Tpoint);
Begin
  If fPieCenter = AValue Then Exit;
  fPieCenter := AValue;
  fChanged := true;
  Invalidate;
End;

Function TSunburstChart.CalcMaxLevelCount: Integer;
  Function GetDepthOf(aElement: PSunBurstChartElement): integer;
  Var
    p: PSunBurstChartElement;
  Begin
    If assigned(aElement) Then Begin
      result := 1 + GetDepthOf(aElement^.Child);
      p := aElement^.NextSibling;
      While assigned(p) Do Begin
        result := max(result, 1 + GetDepthOf(p^.Child));
        p := p^.NextSibling;
      End;
    End
    Else Begin
      result := 0;
    End;
  End;

Begin
  result := GetDepthOf(fRoot);
End;

Procedure TSunburstChart.SetPieRadius(AValue: Single);
Begin
  If fPieRadius = AValue Then Exit;
  fPieRadius := AValue;
  fChanged := true;
  Invalidate;
End;

Procedure TSunburstChart.SetLevelMargin(AValue: integer);
Begin
  If fLevelMargin = AValue Then Exit;
  fLevelMargin := AValue;
  fChanged := true;
  Invalidate;
End;

Procedure TSunburstChart.CalcAllMetaData;
Var
  LevelRadius: Single;

  Function GetSiblingValueSum(Const aElement: PSunBurstChartElement): UInt64;
  Var
    p: PSunBurstChartElement;
  Begin
    result := 0;
    p := aElement;
    While Assigned(p) Do Begin
      result := result + p^.Value;
      p := p^.NextSibling;
    End;
  End;

  Procedure CalcAllMetaDataSub(Const aElement: PSunBurstChartElement; Level: integer; absStartAngle, absEndAngle: Single);
  Var
    ElementSum: UInt64;
    p: PSunBurstChartElement;
    ChildAngleDist, ChildStartAngle, AngleDist: Single;
  Begin
    If Not assigned(aElement) Then exit;

    ElementSum := GetSiblingValueSum(aElement);
    p := aElement;
    AngleDist := absEndAngle - absStartAngle;
    ChildStartAngle := absStartAngle;
    While assigned(p) Do Begin
      ChildAngleDist := AngleDist * p^.Value / max(1, ElementSum);
      p^.AbsStartAngle := ChildStartAngle;
      p^.AbsEndAngle := ChildStartAngle + ChildAngleDist;
      p^.Level := Level;
      p^.LevelRadius := LevelRadius;
      CalcAllMetaDataSub(p^.Child, Level + 1, p^.AbsStartAngle, p^.AbsEndAngle);
      ChildStartAngle := p^.AbsEndAngle;
      p := p^.NextSibling;
    End;
  End;

Var
  LevelCount: integer;
Begin
  LevelCount := CalcMaxlevelCount();
  If LevelCount = 0 Then exit; // Das ist quasi die Prüfung auf fRoot <> Nil

  LevelRadius := (PieRadius - fLevelMargin * (LevelCount - 1)) / LevelCount;

  CalcAllMetaDataSub(fRoot, 0, AngleOffset, InitialArc + AngleOffset);
End;

Procedure TSunburstChart.FreeChild(Var Child: PSunBurstChartElement);
Begin
  If Not assigned(Child) Then exit;
  FreeChild(child^.Child);
  FreeChild(child^.NextSibling);
  If assigned(Child^.UserData) Then Begin
    If assigned(OnDeleteElementsUserData) Then Begin
      OnDeleteElementsUserData(self, Child^.UserData);
    End
    Else Begin
      Raise exception.create('Error, freeing PSunBurstChartElement with userdata, but no OnDeleteElementsUserData set.');
    End;
  End;
  Dispose(Child);
  fChanged := true;
  Invalidate;
End;

Procedure TSunburstChart.SetInitialArc(AValue: Single);
Begin
  If fInitialArc = AValue Then Exit;
  fInitialArc := AValue;
  fChanged := true;
  Invalidate;
End;

Procedure TSunburstChart.setAngleOffset(AValue: Single);
Begin
  If FAngleOffset = AValue Then Exit;
  FAngleOffset := AValue;
  fChanged := true;
  Invalidate;
End;

End.

