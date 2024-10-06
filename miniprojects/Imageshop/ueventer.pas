(******************************************************************************)
(* Eventer                                                         09.05.2019 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : To Create Events for Components that are not derived from    *)
(*               LCL-Components.                                              *)
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
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - IterateAllEventClasses                                *)
(*               0.03 - improve .click detection                              *)
(*                                                                            *)
(* Known Bugs  : none                                                         *)
(*                                                                            *)
(******************************************************************************)
(*
 * Will man Zur Laufzeit Objekte in ein Panel / OpenGLControl oder was auch
 * immer erstellen, und wieder löschen können, dann geht das normale capture
 * designpattern nicht (so wie in TEventerHandler implementiert), da das
 * Freigeben die Aufrufkette zerstört.
 *
 * Diese Unit stellt daher eine Basisklasse (TEventerClass) zur Verfügung
 * mittels derer man virtuelle Methoden zum Überschreiben hat. Diese Klasse
 * kann Beliebig erzeugt und wieder freigegeben werden ohne das die Aufrufkette
 * des Owner beschädigt wird.
 *)

Unit ueventer;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Controls

  (*
   * Kommt hier ein Compilerfehler dann muss die ueventer.inc im
   * Projectverzeichnis angelegt werden. Nachfolgend ein "Maximalbeispiel" für die LCL
   {----------------- Start of content -------------------}

   // Hier wird der Uses Teil voll Fertig gemacht (damit alle notwendigen Units für TOwnerClass zur Verfügung gestellt werden können)
  , ExtCtrls;
   //, OpenGLContext;

// Die Klasse welche die zu Kapernden Methoden bereit stellt

Type
  TOwnerClass = TPaintBox;
  //TOwnerClass = TOpenGLControl;

{$define KeyEvents} // Nicht jede TOwnerClass hat KeyEvents, deswegen sind die Optional => TODO: Manuelles einpflegen der Key Events

    {----------------- End of content -------------------}
    *)

{$I ueventer.inc}

Type

  { TEventerClass }

  TEventerClass = Class
  private
    fHeight: integer;
    fLeft: integer;
    fTop: integer;
    fWidth: integer;
    fVisible: Boolean;
    fEnabled: Boolean; // Visible, alle Auswertungen werden aber Blockiert.
    fOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    fOnMouseMove: TMouseMoveEvent;
    fOnMouseUp: TMouseEvent;
    fOnMouseDown: TMouseEvent;
    fOnKeyPress: TKeyPressEvent;
  protected
    fOwner: TOwnerClass;
    FFocus: Boolean; // Hat das Kontrollelement gerade den Focus ( Achtung wird nur mittels MouseDown bestimmt, ist also nur bedingt zuverlässig )
    FMouseDown: Boolean; // Wurde die Maus gedrückt ( auf dem Kontollelement )
    FMouseHover: Boolean; // Befindet sich die Maus gerade über dem Kontrollelement
    (*
     * Diese Methoden in den Kindklassen überschreiben
     *)
    Function GetClientRect: Trect; virtual;
    Function GetHeight: integer; virtual;
    Function GetWidth: integer; virtual;

    Procedure SetEnabled(AValue: Boolean); virtual;

    Procedure SetHeight(AValue: integer); virtual;
    Procedure SetLeft(AValue: integer); virtual;
    Procedure SetTop(AValue: integer); virtual;
    Procedure SetWidth(AValue: integer); virtual;
    Procedure SetVisible(AValue: Boolean); virtual;

    Procedure Click; virtual;
    Procedure DblClick; virtual;
    Procedure TripleClick; virtual;
    Procedure QuadClick; virtual;

    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    Procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    Procedure KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); virtual;
    Procedure KeyPress(Sender: TObject; Var Key: char); virtual;
    Procedure KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState); virtual;

    (*
     * Iterriert durch alle Registrierten TEventerClass die es im System gibt
     * TODO: Einführen eines "Parent" attributes, damit könnten dann auch so dinge wie TPanel gemacht werden ;)
     *)
    Procedure IterateAllEventClasses(Callback: TNotifyEvent);

    (*
     * Alle Hier Stehenden Properties können in Kindklassen bei Bedarf "sichtbar" gemacht werden.
     *)
    Property OnClick: TNotifyEvent read fOnClick write Fonclick;
    Property OnDblClick: TNotifyEvent read fOnDblClick write FOnDblClick;

    Property OnMouseDown: TMouseEvent read fOnMouseDown write fOnMouseDown;
    Property OnMouseMove: TMouseMoveEvent read fOnMouseMove write fOnMouseMove;
    Property OnMouseUp: TMouseEvent read fOnMouseUp write fOnMouseUp;

    Property OnKeyPress: TKeyPressEvent read fOnKeyPress write FOnKeyPress;

  public
    Property ClientRect: Trect read getClientRect;

    Property Left: integer read fLeft write SetLeft;
    Property Top: integer read fTop write SetTop;
    Property Width: integer read GetWidth write SetWidth;
    Property Height: integer read GetHeight write SetHeight;

    Property Enabled: Boolean read fEnabled write SetEnabled; // Wenn nicht enabled, werden keinerlei Events erzeugt, aber wir sind sichtbat
    Property Visible: Boolean read FVisible write SetVisible; // Wenn nicht sichtbar, werden keinerlei Events erzeugt.

    Procedure SetFocus(); virtual; // Ist mit vorsicht zu geniesen, aber prinzipiell brauchbar ( Das Problem ist das der Focus evtl. noch auf einem Anderen Element sitzen könnte )

    Constructor Create(Owner: TOwnerClass); virtual;
    Destructor Destroy; override;
  End;

  TTransformRoutine = Function(x, y: integer): TPoint;
  (*
   * Befinden sich die Kontrollelemente auf einer Oberfläche die nicht 1:1 ist
   * z.B. ein Scalliertes OpenGL Fenster, dann müssen alle Koordinaten vor der
   * Bearbeitung Transformiert werden. Zu diesem Zweck ist die
   * TransformRoutine zu definieren. Bei einem 1:1 Mapping kann sie NIL bleiben
   *)
Var
  TransformRoutine: TTransformRoutine = Nil;

Function PointInRect(P: TPoint; r: Trect): Boolean;

Implementation

Uses math;

Type

  { TEventerHandler }

  TEventerHandler = Class
  private
    // Todo: Eine Klasse erzeugen, die mehr als nur ein Owner unterstützt.
    FOwner: TOwnerClass;
    fEventer: Array Of TEventerClass;

    fMouseDownEventer: TEventerClass;
    fskipMouseUp: Boolean;
    fleftwasdown: Boolean;
    fOnMouseDownCapture: TMouseEvent;
    fOnMouseMoveCapture: TMouseMoveEvent;
    fOnMouseUpCapture: TMouseEvent;

{$IFDEF KeyEvents}
    fOnKeyDownCapture: TKeyEvent;
    fOnKeyUpCapture: TKeyEvent;
    FOnKeyPressCapture: TKeyPressEvent;
{$ENDIF}
    Procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); virtual;
    Procedure OnKeyPress(Sender: TObject; Var Key: char); virtual;
    Procedure OnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState); virtual;

    Procedure CaptureAllEvents;
    Procedure ReleaseAllEvents;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Procedure RegisterEventer(Owner: TOwnerClass; Eventer: TEventerClass);
    Procedure UnRegisterEventer(Eventer: TEventerClass);
  End;

Var
  EventerHandler: TEventerHandler = Nil;

Function PointInRect(P: TPoint; r: Trect): Boolean;
Begin
  result :=
    (p.x >= min(r.Left, r.Right)) And
    (p.x <= max(r.Left, r.Right)) And
    (p.y >= min(r.Top, r.Bottom)) And
    (p.y <= max(r.Top, r.Bottom));
End;

{ TEventerHandler }

Constructor TEventerHandler.Create;
Begin
  Inherited create;
  FOwner := Nil;
  fEventer := Nil;
  fskipMouseUp := false;
  fleftwasdown := false;
End;

Destructor TEventerHandler.Destroy;
Begin

End;

Procedure TEventerHandler.CaptureAllEvents;
Begin
  fOnMouseDownCapture := FOwner.OnMouseDown;
  FOwner.OnMouseDown := @OnMouseDown;

  fOnMouseUpCapture := FOwner.OnMouseUp;
  FOwner.OnMouseUp := @OnMouseUp;

  fOnMouseMoveCapture := FOwner.OnMouseMove;
  FOwner.OnMouseMove := @OnMouseMove;
{$IFDEF KeyEvents}
  fOnKeyDownCapture := FOwner.OnKeyDown;
  FOwner.OnKeyDown := @OnKeyDown;

  fOnKeyUpCapture := FOwner.OnKeyUp;
  FOwner.OnKeyUp := @OnKeyUp;

  fOnKeyPressCapture := FOwner.OnKeyPress;
  FOwner.OnKeyPress := @OnKeyPress;
{$ENDIF}
End;

Procedure TEventerHandler.ReleaseAllEvents;
Begin
  FOwner.OnMouseDown := fOnMouseDownCapture;
  FOwner.OnMouseUp := fOnMouseUpCapture;
  FOwner.OnMouseMove := fOnMouseMoveCapture;
{$IFDEF KeyEvents}
  FOwner.OnKeyDown := fOnKeyDownCapture;
  FOwner.OnKeyUp := fOnKeyUpCapture;
  FOwner.OnKeyPress := FOnKeyPressCapture;
{$ENDIF}
  FOwner := Nil;
End;

Procedure TEventerHandler.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  ox, oy, i: Integer;
  p: Tpoint;
  handled: Boolean;
Begin
  ox := x;
  oy := y;
  handled := false;
  If Assigned(TransformRoutine) Then Begin
    p := TransformRoutine(x, y);
    x := p.x;
    y := p.y;
  End;
  For i := 0 To high(fEventer) Do Begin
    fEventer[i].FMouseDown := false;
  End;
  For i := 0 To high(fEventer) Do Begin
    If i > high(fEventer) Then break; // Da im Event das Eventer element auch freigegeben werden darf, braucht es dieses if
    If PointInRect(point(x, y), fEventer[i].ClientRect) And fEventer[i].fVisible And fEventer[i].fEnabled Then Begin
      handled := true;
      If fMouseDownEventer = fEventer[i] Then fMouseDownEventer := Nil;
      fEventer[i].MouseUp(button, shift, x - fEventer[i].Left, y - fEventer[i].Top);
      If i > high(fEventer) Then break; // Da im Event das Eventer element auch freigegeben werden darf, braucht es dieses if
      If Not fskipMouseUp Then Begin
        If fleftwasdown Then Begin
          If fEventer[i].fVisible And fEventer[i].fEnabled Then Begin
            fEventer[i].Click;
          End;
        End;
      End;
      break; // Der Event wurde behandelt, also raus .. (aber kein Exit, damit das evtl MouseUp des DownEventer noch gesetzt werden kann !)
    End;
  End;
  If assigned(fMouseDownEventer) And fMouseDownEventer.Visible And fMouseDownEventer.Enabled Then Begin
    fMouseDownEventer.MouseUp(button, shift, x - fMouseDownEventer.Left, y - fMouseDownEventer.Top);
  End;
  If assigned(fOnMouseUpCapture) And (Not handled) Then Begin
    fOnMouseUpCapture(sender, button, shift, ox, oy);
  End;
End;

Procedure TEventerHandler.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  i, ox, oy: Integer;
  p: Tpoint;
Begin
  ox := x;
  oy := y;
  fskipMouseUp := false;
  fMouseDownEventer := Nil;
  fleftwasdown := ssleft In shift;
  If Assigned(TransformRoutine) Then Begin
    p := TransformRoutine(x, y);
    x := p.x;
    y := p.y;
  End;
  For i := 0 To high(fEventer) Do Begin
    fEventer[i].FFocus := false;
  End;
  // TODO: Die Laufrichtung von i sollte Rückwärts sein, dann kann man sich diese If's sparen
  //       Das Problem ist aber dennoch das gleiche, wenn ein Eventer Freigegeben wird steht i auf dem "danach" d.h. alle nachfoldenden Events werden dann beim Falschen Element aufgerufen !
  For i := 0 To high(fEventer) Do Begin
    If i > high(fEventer) Then break; // Da im Event das Eventer element auch freigegeben werden darf, baucht es dieses if
    If PointInRect(point(x, y), fEventer[i].ClientRect) And fEventer[i].Visible And fEventer[i].fEnabled Then Begin
      fMouseDownEventer := fEventer[i];
      fEventer[i].FMouseDown := true;
      fEventer[i].FFocus := true;
      fEventer[i].MouseDown(button, shift, x - fEventer[i].Left, y - fEventer[i].Top);
      If i > high(fEventer) Then exit; // Da im Event das Eventer element auch freigegeben werden darf, baucht es dieses if
      If (ssDouble In Shift) And fEventer[i].Visible And fEventer[i].fEnabled Then Begin
        fEventer[i].DblClick;
        fskipMouseUp := true;
      End;
      If i > high(fEventer) Then exit; // Da im Event das Eventer element auch freigegeben werden darf, baucht es dieses if
      If (ssTriple In Shift) And fEventer[i].Visible And fEventer[i].fEnabled Then Begin
        fEventer[i].TripleClick;
        fskipMouseUp := true;
      End;
      If i > high(fEventer) Then exit; // Da im Event das Eventer element auch freigegeben werden darf, baucht es dieses if
      If (ssQuad In Shift) And fEventer[i].Visible And fEventer[i].fEnabled Then Begin
        fEventer[i].QuadClick;
        fskipMouseUp := true;
      End;
      exit;
    End;
  End;
  If assigned(fOnMouseDownCapture) Then Begin
    fOnMouseDownCapture(sender, button, shift, ox, oy);
  End;
End;

Procedure TEventerHandler.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  i, ox, oy: Integer;
  capfound, found: Boolean;
  p: TPoint;
Begin
  ox := x;
  oy := y;
  // If ssleft In shift Then fskipMouseUp := true; -- Deactivated, makes click event much more intuitive
  If Assigned(TransformRoutine) Then Begin
    p := TransformRoutine(x, y);
    x := p.x;
    y := p.y;
  End;
  found := false;
  capfound := false;
  For i := 0 To high(fEventer) Do Begin
    If PointInRect(point(x, y), fEventer[i].ClientRect) And fEventer[i].Visible And fEventer[i].fEnabled Then Begin
      fEventer[i].FMouseHover := true;
      fEventer[i].MouseMove(Shift, x - fEventer[i].Left, y - fEventer[i].Top);
      found := true;
      If fEventer[i] = fMouseDownEventer Then capfound := true;
    End
    Else Begin
      fEventer[i].FMouseHover := false;
    End;
  End;
  If assigned(fMouseDownEventer) And Not capfound Then Begin
    fMouseDownEventer.MouseMove(Shift, x - fMouseDownEventer.Left, y - fMouseDownEventer.Top);
  End;
  If Not found And assigned(fOnMouseMoveCapture) Then Begin
    fOnMouseMoveCapture(sender, shift, ox, oy);
  End;
End;

Procedure TEventerHandler.OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  i: integer;
Begin
  For i := 0 To high(fEventer) Do Begin
    fEventer[i].KeyDown(sender, key, Shift);
  End;
{$IFDEF KeyEvents}
  If assigned(fOnKeyDownCapture) Then Begin
    fOnKeyDownCapture(sender, key, Shift);
  End;
{$ENDIF}
End;

Procedure TEventerHandler.OnKeyPress(Sender: TObject; Var Key: char);
Var
  i: integer;
Begin
  For i := 0 To high(fEventer) Do Begin
    fEventer[i].KeyPress(sender, key);
  End;
{$IFDEF KeyEvents}
  If assigned(FOnKeyPressCapture) Then Begin
    FOnKeyPressCapture(sender, key);
  End;
{$ENDIF}
End;

Procedure TEventerHandler.OnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  i: integer;
Begin
  For i := 0 To high(fEventer) Do Begin
    fEventer[i].KeyUp(sender, key, Shift);
  End;
{$IFDEF KeyEvents}
  If assigned(fOnKeyUpCapture) Then Begin
    fOnKeyUpCapture(sender, key, Shift);
  End;
{$ENDIF}
End;

Procedure TEventerHandler.RegisterEventer(Owner: TOwnerClass;
  Eventer: TEventerClass);
Var
  i: integer;
Begin
  If FOwner = Nil Then Begin
    FOwner := Owner;
    CaptureAllEvents();
  End;
  If FOwner <> Owner Then Begin
    Raise Exception.create('Aktuell wird nur ein Owner unterstützt. Bitte multi Owner Support implementieren.');
  End;
  (*
   * Ein und der Selbe Eventer dürfen nicht 2 mal Registriert werden !
   *)
  For i := 0 To high(fEventer) Do Begin
    If fEventer[i] = Eventer Then Begin
      (*
       * Beim Freigeben würde die Instanz nur 1 mal frei gegeben,
       * Sämtlichen Events würden mehrfach generiert.
       *)
      Raise Exception.create('It is not allowed to add a eventer multiple times.');
    End;
  End;
  setlength(fEventer, high(fEventer) + 2);
  fEventer[high(fEventer)] := Eventer;
End;

Procedure TEventerHandler.UnRegisterEventer(Eventer: TEventerClass);
Var
  i, j: Integer;
Begin
  (*
   * Das Objekt in dessen OnMouseDown wir uns gerade befinden, soll frei gegeben werden
   * Also darf sein fMouseDownEventer nicht mehr genutzt werden.
   *)
  If Eventer = fMouseDownEventer Then Begin
    fMouseDownEventer := Nil;
  End;
  For i := 0 To high(fEventer) Do Begin
    If fEventer[i] = Eventer Then Begin
      For j := i To High(fEventer) - 1 Do Begin
        fEventer[j] := fEventer[j + 1];
      End;
      setlength(fEventer, high(fEventer));
      If high(fEventer) = -1 Then Begin
        ReleaseAllEvents;
      End;
      exit;
    End;
  End;
End;

{ TEventerClass }

Constructor TEventerClass.Create(Owner: TOwnerClass);
Begin
  Inherited create;
  fOwner := Owner;
  fVisible := true;
  fEnabled := true;
  EventerHandler.RegisterEventer(owner, self);
  fMouseDown := false;
  fOnMouseUp := Nil;
  fOnMouseMove := Nil;
  fOnMouseDown := Nil;
  fOnClick := Nil;
  FOnDblClick := Nil;
  fOnKeyPress := Nil;
End;

Destructor TEventerClass.Destroy;
Begin
  EventerHandler.UnRegisterEventer(self);
End;

Procedure TEventerClass.SetHeight(AValue: integer);
Begin
  If fHeight = AValue Then Exit;
  fHeight := AValue;
End;

Function TEventerClass.GetClientRect: Trect;
Begin
  result := rect(fLeft, fTop, fLeft + fWidth, fTop + fHeight);
End;

Function TEventerClass.GetWidth: integer;
Begin
  result := fWidth;
End;

Function TEventerClass.GetHeight: integer;
Begin
  result := fHeight;
End;

Procedure TEventerClass.SetEnabled(AValue: Boolean);
Begin
  If fEnabled = AValue Then Exit;
  fEnabled := AValue;
  If Not fEnabled Then Begin
    FFocus := false;
    FMouseDown := false;
    FMouseHover := false;
  End;
End;

Procedure TEventerClass.SetLeft(AValue: integer);
Begin
  If fLeft = AValue Then Exit;
  fLeft := AValue;
End;

Procedure TEventerClass.SetTop(AValue: integer);
Begin
  If fTop = AValue Then Exit;
  fTop := AValue;
End;

Procedure TEventerClass.SetWidth(AValue: integer);
Begin
  If fWidth = AValue Then Exit;
  fWidth := AValue;
End;

Procedure TEventerClass.SetVisible(AValue: Boolean);
Begin
  If fVisible = AValue Then exit;
  fVisible := AValue;
End;

Procedure TEventerClass.Click;
Begin
  If assigned(fOnClick) Then Begin
    fOnClick(Self);
  End;
End;

Procedure TEventerClass.DblClick;
Begin
  If assigned(FOnDblClick) Then Begin
    FOnDblClick(Self);
  End;
End;

Procedure TEventerClass.TripleClick;
Begin
  // TODO: Wie Click implementieren
End;

Procedure TEventerClass.QuadClick;
Begin
  // TODO: Wie Click implementieren
End;

Procedure TEventerClass.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  If assigned(fOnMouseDown) Then Begin
    fOnMouseDown(self, Button, shift, x, y);
  End;
End;

Procedure TEventerClass.MouseMove(Shift: TShiftState; X, Y: Integer);
Begin
  If assigned(fOnMouseMove) Then Begin
    fOnMouseMove(self, shift, x, y);
  End;
End;

Procedure TEventerClass.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  If assigned(fOnMouseUp) Then Begin
    fOnMouseUp(self, Button, shift, x, y);
  End;
End;

Procedure TEventerClass.KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin

End;

Procedure TEventerClass.KeyPress(Sender: TObject; Var Key: char);
Begin
  If Visible And assigned(OnKeyPress) Then Begin
    OnKeyPress(Self, Key);
  End;
End;

Procedure TEventerClass.KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin

End;

Procedure TEventerClass.IterateAllEventClasses(Callback: TNotifyEvent);
Var
  i: Integer;
Begin
  For i := 0 To high(EventerHandler.fEventer) Do Begin
    Callback(EventerHandler.fEventer[i]);
  End;
End;

Procedure TEventerClass.SetFocus;
Begin
  FFocus := True;
End;

Initialization

  EventerHandler := TEventerHandler.Create;

Finalization;
  EventerHandler.free;

End.

