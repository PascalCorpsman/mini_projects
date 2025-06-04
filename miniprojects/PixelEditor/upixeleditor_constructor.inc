(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of PixelEditor                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)

// This code es extracted from the upixeleditor.MakeCurrent function in order
// to not disturb the read flow of the user, in a typical LCL-Application all
// this content would be defined in the .lfm file

AktColorInfoLabel := TOpenGl_Label.Create(owner, '');
AktColorInfoLabel.Left := 147;
AktColorInfoLabel.Top := 463;
AktColorInfoLabel.Caption := '';
AktColorInfoLabel.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
AddElement(AktColorInfoLabel);

NewButton := TOpenGL_Bevel.Create(Owner);
Newbutton.Left := 3;
Newbutton.Top := 3;
image := LoadAlphaColorGraphik('New.bmp');
If image = 0 Then exit;
Newbutton.SetImage(image);
Newbutton.Transparent := true;
Newbutton.Width := 32;
Newbutton.Height := 32;
NewButton.OnClick := @OnNewButtonClick;
NewButton.Hint := 'Create new image (CTRL+N)';
AddElement(NewButton);

OpenButton := TOpenGL_Bevel.Create(Owner);
OpenButton.Left := 3 + 32 + 3;
OpenButton.Top := 3;
image := LoadAlphaColorGraphik('Open.bmp');
If image = 0 Then exit;
OpenButton.SetImage(image);
OpenButton.Transparent := true;
OpenButton.Width := 32;
OpenButton.Height := 32;
OpenButton.OnClick := @OnOpenButtonClick;
OpenButton.Hint := 'Open image (CTRL+L)';
AddElement(OpenButton);

SaveButton := TOpenGL_Bevel.Create(Owner);
SaveButton.Left := 3 + 32 + 3 + 32 + 3;
SaveButton.Top := 3;
image := LoadAlphaColorGraphik('Save.bmp');
If image = 0 Then exit;
SaveButton.SetImage(image);
SaveButton.Transparent := true;
SaveButton.Width := 32;
SaveButton.Height := 32;
SaveButton.OnClick := @OnSaveButtonClick;
SaveButton.Hint := 'Save image (CTRL+S)';
AddElement(SaveButton);

SaveAsButton := TOpenGL_Bevel.Create(Owner);
SaveAsButton.Left := 3 + 32 + 3 + 32 + 3 + 32 + 3;
SaveAsButton.Top := 3;
image := LoadAlphaColorGraphik('SaveAs.bmp');
If image = 0 Then exit;
SaveAsButton.SetImage(image);
SaveAsButton.Transparent := true;
SaveAsButton.Width := 32;
SaveAsButton.Height := 32;
SaveAsButton.OnClick := @OnSaveAsButtonClick;
SaveAsButton.Hint := 'Save image as';
AddElement(SaveAsButton);

ExitButton := TOpenGL_Bevel.Create(Owner);
Exitbutton.Left := 143;
Exitbutton.Top := 3;
image := LoadAlphaColorGraphik('Exit.bmp');
If image = 0 Then exit;
Exitbutton.SetImage(image);
Exitbutton.Transparent := true;
Exitbutton.Width := 32;
Exitbutton.Height := 32;
ExitButton.OnClick := @OnExitButtonClick;
ExitButton.Hint := 'Close pixeleditor (ALT+F4)';
AddElement(ExitButton);

GridButton := TOpenGL_Bevel.Create(Owner);
GridButton.Left := 213;
GridButton.Top := 3;
image := LoadAlphaColorGraphik('Grid.bmp');
If image = 0 Then exit;
GridButton.SetImage(image);
GridButton.Transparent := true;
GridButton.Width := 32;
GridButton.Height := 32;
GridButton.OnClick := @OnGridButtonClick;
GridButton.Hint := 'Toggle grid (CTRL+G)';
AddElement(GridButton);

ZoomOutButton := TOpenGL_Bevel.Create(Owner);
ZoomOutButton.Left := 262;
ZoomOutButton.Top := 10;
image := LoadAlphaColorGraphik('Minus.bmp');
If image = 0 Then exit;
ZoomOutButton.SetImage(image);
ZoomOutButton.Transparent := true;
ZoomOutButton.Width := 16;
ZoomOutButton.Height := 16;
ZoomOutButton.OnClick := @OnZoomOutButtonClick;
ZoomOutButton.hint := 'Zoom out (-)';
AddElement(ZoomOutButton);

ZoomInfoTextbox := TOpenGL_Textbox.Create(Owner, '');
ZoomInfoTextbox.Left := 283;
ZoomInfoTextbox.Top := 3;
ZoomInfoTextbox.Width := 67;
ZoomInfoTextbox.Height := 32;
ZoomInfoTextbox.Layout := tlCenter;
ZoomInfoTextbox.Alignment := taCenter;
ZoomInfoTextbox.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
AddElement(ZoomInfoTextbox);

ZoomInButton := TOpenGL_Bevel.Create(Owner);
ZoomInButton.Left := 353;
ZoomInButton.Top := 10;
image := LoadAlphaColorGraphik('Plus.bmp');
If image = 0 Then exit;
ZoomInButton.SetImage(image);
ZoomInButton.Transparent := true;
ZoomInButton.Width := 16;
ZoomInButton.Height := 16;
ZoomInButton.OnClick := @OnZoomInButtonClick;
ZoomInButton.Hint := 'Zoom in (+)';
AddElement(ZoomInButton);

ColorMonochronButton := TOpenGL_ToggleButton.Create(Owner);
ColorMonochronButton.Left := 386;
ColorMonochronButton.Top := 3;
image := LoadAlphaColorGraphik('Monochrome_vs_Colored_2.bmp');
If image = 0 Then exit;
ColorMonochronButton.SetImage(image);
image := LoadAlphaColorGraphik('Monochrome_vs_Colored_1.bmp');
If image = 0 Then exit;
ColorMonochronButton.SetDownImage(image);
ColorMonochronButton.Transparent := true;
ColorMonochronButton.Width := 32;
ColorMonochronButton.Height := 32;
ColorMonochronButton.Hint := 'Switch colored / monochrone view (X)';
//ColorMonochronButton.OnClick := @OnSelectModeButtonClick; -- Braucht es nicht, man kann ja den Style abfragen ;)
AddElement(ColorMonochronButton);

OptionsButton := TOpenGL_Bevel.Create(Owner);
OptionsButton.Left := 570;
OptionsButton.Top := 3;
image := LoadAlphaColorGraphik('Settings.bmp');
If image = 0 Then exit;
OptionsButton.SetImage(image);
OptionsButton.Transparent := true;
OptionsButton.Width := 32;
OptionsButton.Height := 32;
OptionsButton.OnClick := @OnOptionsButtonClick;
OptionsButton.Hint := 'Settings (CTRL+O)';
AddElement(OptionsButton);

UndoButton := TOpenGL_Bevel.Create(Owner);
UndoButton.Left := 605;
UndoButton.Top := 3;
image := LoadAlphaColorGraphik('Undo.bmp');
If image = 0 Then exit;
UndoButton.SetImage(image);
UndoButton.Transparent := true;
UndoButton.Width := 32;
UndoButton.Height := 32;
UndoButton.OnClick := @OnUndoButtonClick;
UndoButton.OnDblClick := @OnUndoButtonClick;
UndoButton.Hint := 'Undo (CTRL+Z)';
AddElement(UndoButton);

SelectButton := TOpenGL_Bevel.Create(Owner);
SelectButton.Left := 3;
SelectButton.Top := 41;
image := LoadAlphaColorGraphik('Select.bmp');
If image = 0 Then exit;
SelectButton.SetImage(image);
SelectButton.Transparent := true;
SelectButton.Width := 32;
SelectButton.Height := 32;
SelectButton.OnClick := @OnSelectButtonClick;
SelectButton.hint := 'Select pixel area (S)';
AddElement(SelectButton);

SelectModeButton := TOpenGL_ToggleButton.Create(Owner);
SelectModeButton.Left := 41;
SelectModeButton.Top := 41;
image := LoadAlphaColorGraphik('SelModeLowered.bmp');
If image = 0 Then exit;
SelectModeButton.SetImage(image);
image := LoadAlphaColorGraphik('SelModeRaised.bmp');
If image = 0 Then exit;
SelectModeButton.SetDownImage(image);
SelectModeButton.Transparent := true;
SelectModeButton.Width := 32;
SelectModeButton.Height := 32;
SelectModeButton.Hint := 'Switch paste mode';
//SelectModeButton.OnClick := @OnSelectModeButtonClick; -- Braucht es nicht, man kann ja den Style abfragen ;)
AddElement(SelectModeButton);

SelectRotateCounterClockwise90 := TOpenGL_Bevel.Create(Owner);
SelectRotateCounterClockwise90.Left := 3;
SelectRotateCounterClockwise90.Top := 76;
image := LoadAlphaColorGraphik('Rotate90.bmp');
If image = 0 Then exit;
SelectRotateCounterClockwise90.SetImage(image);
SelectRotateCounterClockwise90.Transparent := true;
SelectRotateCounterClockwise90.Width := 16;
SelectRotateCounterClockwise90.Height := 16;
SelectRotateCounterClockwise90.OnClick := @OnSelectRotateCounterClockwise90ButtonClick;
SelectRotateCounterClockwise90.Hint := 'Rotate 90' + chr($C3) + chr($B8) + ' (SHIFT+R)'; // das ° Zeichen
AddElement(SelectRotateCounterClockwise90);

SelectMirrorHorButton := TOpenGL_Bevel.Create(Owner);
SelectMirrorHorButton.Left := 21;
SelectMirrorHorButton.Top := 76;
image := LoadAlphaColorGraphik('Mirror_Hor.bmp');
If image = 0 Then exit;
SelectMirrorHorButton.SetImage(image);
SelectMirrorHorButton.Transparent := true;
SelectMirrorHorButton.Width := 16;
SelectMirrorHorButton.Height := 16;
SelectMirrorHorButton.OnClick := @OnSelectMirrorHorButtonClick;
SelectMirrorHorButton.hint := 'Mirror horizontally (SHIFT+H)';
AddElement(SelectMirrorHorButton);

SelectMirrorVerButton := TOpenGL_Bevel.Create(Owner);
SelectMirrorVerButton.Left := 21 + 18;
SelectMirrorVerButton.Top := 76;
image := LoadAlphaColorGraphik('Mirror_Ver.bmp');
If image = 0 Then exit;
SelectMirrorVerButton.SetImage(image);
SelectMirrorVerButton.Transparent := true;
SelectMirrorVerButton.Width := 16;
SelectMirrorVerButton.Height := 16;
SelectMirrorVerButton.OnClick := @OnSelectMirrorVerButtonClick;
SelectMirrorVerButton.Hint := 'Mirror vertically (SHIFT+V)';
AddElement(SelectMirrorVerButton);

SelectRotateAngle := TOpenGL_Bevel.Create(Owner);
SelectRotateAngle.Left := 21 + 18 + 18;
SelectRotateAngle.Top := 76;
image := LoadAlphaColorGraphik('Angle.bmp');
If image = 0 Then exit;
SelectRotateAngle.SetImage(image);
SelectRotateAngle.Transparent := true;
SelectRotateAngle.Width := 16;
SelectRotateAngle.Height := 16;
SelectRotateAngle.OnClick := @OnSelectRotateAngleButtonClick;
SelectRotateAngle.Hint := 'Rotate by angle (SHIFT+A)';
AddElement(SelectRotateAngle);

BrightenButton := TOpenGL_Bevel.Create(Owner);
BrightenButton.Left := 3;
BrightenButton.Top := 95;
image := LoadAlphaColorGraphik('Brighten.bmp');
If image = 0 Then exit;
BrightenButton.SetImage(image);
BrightenButton.Transparent := true;
BrightenButton.Width := 32;
BrightenButton.Height := 32;
BrightenButton.OnClick := @OnBrightenButtonClick;
BrightenButton.OnDblClick := @OnBrightenButtonClick;
BrightenButton.Hint := 'Brighten (B)';
AddElement(BrightenButton);

DarkenButton := TOpenGL_Bevel.Create(Owner);
DarkenButton.Left := 41;
DarkenButton.Top := 95;
image := LoadAlphaColorGraphik('Darken.bmp');
If image = 0 Then exit;
DarkenButton.SetImage(image);
DarkenButton.Transparent := true;
DarkenButton.Width := 32;
DarkenButton.Height := 32;
DarkenButton.OnClick := @OnDarkenButtonClick;
DarkenButton.OnDblClick := @OnDarkenButtonClick;
DarkenButton.Hint := 'Darken (D)';
AddElement(DarkenButton);

CurserSize1 := TOpenGL_Bevel.Create(Owner);
CurserSize1.Left := 3;
CurserSize1.Top := 130;
image := LoadAlphaColorGraphik('Size1.bmp');
If image = 0 Then exit;
CurserSize1.SetImage(image);
CurserSize1.Transparent := true;
CurserSize1.Width := 16;
CurserSize1.Height := 16;
CurserSize1.OnClick := @OnCurserSizeButtonClick;
CurserSize1.Hint := 'Small (1)';
AddElement(CurserSize1);

CurserSize2 := TOpenGL_Bevel.Create(Owner);
CurserSize2.Left := 21;
CurserSize2.Top := 130;
image := LoadAlphaColorGraphik('Size2.bmp');
If image = 0 Then exit;
CurserSize2.SetImage(image);
CurserSize2.Transparent := true;
CurserSize2.Width := 16;
CurserSize2.Height := 16;
CurserSize2.OnClick := @OnCurserSizeButtonClick;
CurserSize2.Hint := 'Medium (2)';
AddElement(CurserSize2);

CurserSize3 := TOpenGL_Bevel.Create(Owner);
CurserSize3.Left := 21 + 18;
CurserSize3.Top := 130;
image := LoadAlphaColorGraphik('Size3.bmp');
If image = 0 Then exit;
CurserSize3.SetImage(image);
CurserSize3.Transparent := true;
CurserSize3.Width := 16;
CurserSize3.Height := 16;
CurserSize3.OnClick := @OnCurserSizeButtonClick;
CurserSize3.Hint := 'Large (3)';
AddElement(CurserSize3);

CurserSize4 := TOpenGL_Bevel.Create(Owner);
CurserSize4.Left := 21 + 18 + 18;
CurserSize4.Top := 130;
image := LoadAlphaColorGraphik('Size4.bmp');
If image = 0 Then exit;
CurserSize4.SetImage(image);
CurserSize4.Transparent := true;
CurserSize4.Width := 16;
CurserSize4.Height := 16;
CurserSize4.OnClick := @OnCurserSizeButtonClick;
CurserSize4.Hint := 'Extra large (4)';
AddElement(CurserSize4);

EraserButton := TOpenGL_Bevel.Create(Owner);
EraserButton.Left := 3;
EraserButton.Top := 149;
image := LoadAlphaColorGraphik('Eraser.bmp');
If image = 0 Then exit;
EraserButton.SetImage(image);
EraserButton.Transparent := true;
EraserButton.Width := 32;
EraserButton.Height := 32;
EraserButton.OnClick := @OnEraserButtonClick;
EraserButton.Hint := 'Eraser tool (DEL/BACK)';
AddElement(EraserButton);

PencilButton := TOpenGL_Bevel.Create(Owner);
PencilButton.Left := 3;
PencilButton.Top := 184;
image := LoadAlphaColorGraphik('Pencil.bmp');
If image = 0 Then exit;
PencilButton.SetImage(image);
PencilButton.Transparent := true;
PencilButton.Width := 32;
PencilButton.Height := 32;
PencilButton.OnClick := @OnPencilButtonClick;
PencilButton.Hint := 'Pencil tool (P)';
AddElement(PencilButton);

CursorRoundShape1 := TOpenGL_Bevel.Create(Owner);
CursorRoundShape1.Left := 40;
CursorRoundShape1.Top := 174;
image := LoadAlphaColorGraphik('RoundShape1.bmp');
If image = 0 Then exit;
CursorRoundShape1.SetImage(image);
CursorRoundShape1.Transparent := true;
CursorRoundShape1.Width := 16;
CursorRoundShape1.Height := 16;
CursorRoundShape1.OnClick := @OnCursorShapeClick;
CursorRoundShape1.hint := 'Dot (SHIFT+1)';
AddElement(CursorRoundShape1);

CursorRoundShape2 := TOpenGL_Bevel.Create(Owner);
CursorRoundShape2.Left := 40;
CursorRoundShape2.Top := 192;
image := LoadAlphaColorGraphik('RoundShape2.bmp');
If image = 0 Then exit;
CursorRoundShape2.SetImage(image);
CursorRoundShape2.Transparent := true;
CursorRoundShape2.Width := 16;
CursorRoundShape2.Height := 16;
CursorRoundShape2.OnClick := @OnCursorShapeClick;
CursorRoundShape2.hint := 'Small disc (SHIFT+3)';
AddElement(CursorRoundShape2);

CursorRoundShape3 := TOpenGL_Bevel.Create(Owner);
CursorRoundShape3.Left := 40;
CursorRoundShape3.Top := 210;
image := LoadAlphaColorGraphik('RoundShape3.bmp');
If image = 0 Then exit;
CursorRoundShape3.SetImage(image);
CursorRoundShape3.Transparent := true;
CursorRoundShape3.Width := 16;
CursorRoundShape3.Height := 16;
CursorRoundShape3.OnClick := @OnCursorShapeClick;
CursorRoundShape3.hint := 'Big disc (SHIFT+5)';
AddElement(CursorRoundShape3);

CursorSquareShape1 := TOpenGL_Bevel.Create(Owner);
CursorSquareShape1.Left := 58;
CursorSquareShape1.Top := 174;
image := LoadAlphaColorGraphik('SquareShape1.bmp');
If image = 0 Then exit;
CursorSquareShape1.SetImage(image);
CursorSquareShape1.Transparent := true;
CursorSquareShape1.Width := 16;
CursorSquareShape1.Height := 16;
CursorSquareShape1.OnClick := @OnCursorShapeClick;
CursorSquareShape1.Hint := 'Small rectangle (SHIFT+2)';
AddElement(CursorSquareShape1);

CursorSquareShape2 := TOpenGL_Bevel.Create(Owner);
CursorSquareShape2.Left := 58;
CursorSquareShape2.Top := 192;
image := LoadAlphaColorGraphik('SquareShape2.bmp');
If image = 0 Then exit;
CursorSquareShape2.SetImage(image);
CursorSquareShape2.Transparent := true;
CursorSquareShape2.Width := 16;
CursorSquareShape2.Height := 16;
CursorSquareShape2.OnClick := @OnCursorShapeClick;
CursorSquareShape2.Hint := 'Medium rectangle (SHIFT+4)';
AddElement(CursorSquareShape2);

CursorSquareShape3 := TOpenGL_Bevel.Create(Owner);
CursorSquareShape3.Left := 58;
CursorSquareShape3.Top := 210;
image := LoadAlphaColorGraphik('SquareShape3.bmp');
If image = 0 Then exit;
CursorSquareShape3.SetImage(image);
CursorSquareShape3.Transparent := true;
CursorSquareShape3.Width := 16;
CursorSquareShape3.Height := 16;
CursorSquareShape3.OnClick := @OnCursorShapeClick;
CursorSquareShape3.Hint := 'Big rectangle (SHIFT+6)';
AddElement(CursorSquareShape3);

LineButton := TOpenGL_Bevel.Create(Owner);
LineButton.Left := 3;
LineButton.Top := 219;
image := LoadAlphaColorGraphik('Line.bmp');
If image = 0 Then exit;
LineButton.SetImage(image);
LineButton.Transparent := true;
LineButton.Width := 32;
LineButton.Height := 32;
LineButton.OnClick := @OnLineButtonClick;
LineButton.Hint := 'Line tool (L)';
AddElement(LineButton);

CircleButton := TOpenGL_Bevel.Create(Owner);
CircleButton.Left := 3;
CircleButton.Top := 254;
image := LoadAlphaColorGraphik('Ellipse.bmp');
If image = 0 Then exit;
CircleButton.SetImage(image);
CircleButton.Transparent := true;
CircleButton.Width := 32;
CircleButton.Height := 32;
CircleButton.OnClick := @OnCircleButtonClick;
CircleButton.Hint := 'Ellipse tool (E)';
AddElement(CircleButton);

SquareButton := TOpenGL_Bevel.Create(Owner);
SquareButton.Left := 3;
SquareButton.Top := 289;
image := LoadAlphaColorGraphik('Rectangle.bmp');
If image = 0 Then exit;
SquareButton.SetImage(image);
SquareButton.Transparent := true;
SquareButton.Width := 32;
SquareButton.Height := 32;
SquareButton.OnClick := @OnSquareButtonClick;
SquareButton.Hint := 'Rectangle tool (R)';
AddElement(SquareButton);

OutlineButton := TOpenGL_Bevel.Create(Owner);
OutlineButton.Left := 40;
OutlineButton.Top := 279;
image := LoadAlphaColorGraphik('Outline.bmp');
If image = 0 Then exit;
OutlineButton.SetImage(image);
OutlineButton.Transparent := true;
OutlineButton.Width := 16;
OutlineButton.Height := 16;
OutlineButton.OnClick := @OnOutlineButtonClick;
OutlineButton.Hint := 'Outline (O)';
AddElement(OutlineButton);

FilledButton := TOpenGL_Bevel.Create(Owner);
FilledButton.Left := 58;
FilledButton.Top := 279;
image := LoadAlphaColorGraphik('Filled.bmp');
If image = 0 Then exit;
FilledButton.SetImage(image);
FilledButton.Transparent := true;
FilledButton.Width := 16;
FilledButton.Height := 16;
FilledButton.OnClick := @OnOutlineButtonClick;
FilledButton.Hint := 'Filled (F)';
AddElement(FilledButton);

MirrorButton := TOpenGL_Bevel.Create(Owner);
MirrorButton.Left := 3;
MirrorButton.Top := 324;
image := LoadAlphaColorGraphik('Mirror.bmp');
If image = 0 Then exit;
MirrorButton.SetImage(image);
MirrorButton.Transparent := true;
MirrorButton.Width := 32;
MirrorButton.Height := 32;
MirrorButton.OnClick := @OnMirrorButtonClick;
MirrorButton.Hint := 'Mirror tool (M / SHIFT+M)';
AddElement(MirrorButton);

MirrorCenterButton := TOpenGL_ToggleButton.create(Owner);
MirrorCenterButton.Left := 58;
MirrorCenterButton.Top := 322;
image := LoadAlphaColorGraphik('Mirror_Center.bmp');
If image = 0 Then exit;
MirrorCenterButton.SetImage(image);
image := LoadAlphaColorGraphik('Mirror_Axis.bmp');
If image = 0 Then exit;
MirrorCenterButton.SetDownImage(image);
MirrorCenterButton.Transparent := true;
MirrorCenterButton.Width := 16;
MirrorCenterButton.Height := 16;
MirrorCenterButton.Hint := 'Toggle mirror center (C)';
//MirrorCenterButton.OnClick := @OnMirrorCenterButtonClick; -- Braucht es nicht man kann ja den Style abfragen
AddElement(MirrorCenterButton);

Mirror4Button := TOpenGL_Bevel.Create(Owner);
Mirror4Button.Left := 40;
Mirror4Button.Top := 322;
image := LoadAlphaColorGraphik('Mirror4.bmp');
If image = 0 Then exit;
Mirror4Button.SetImage(image);
Mirror4Button.Transparent := true;
Mirror4Button.Width := 16;
Mirror4Button.Height := 16;
Mirror4Button.OnClick := @OnMirrorModeButtonClick;
Mirror4Button.Hint := 'Mirror horizontally and vertically (M)';
AddElement(Mirror4Button);

MirrorVertButton := TOpenGL_Bevel.Create(Owner);
MirrorVertButton.Left := 40;
MirrorVertButton.Top := 340;
image := LoadAlphaColorGraphik('Mirror_Ver.bmp');
If image = 0 Then exit;
MirrorVertButton.SetImage(image);
MirrorVertButton.Transparent := true;
MirrorVertButton.Width := 16;
MirrorVertButton.Height := 16;
MirrorVertButton.OnClick := @OnMirrorModeButtonClick;
MirrorVertButton.Hint := 'Mirror vertically (M)';
AddElement(MirrorVertButton);

MirrorHorButton := TOpenGL_Bevel.Create(Owner);
MirrorHorButton.Left := 58;
MirrorHorButton.Top := 340;
image := LoadAlphaColorGraphik('Mirror_Hor.bmp');
If image = 0 Then exit;
MirrorHorButton.SetImage(image);
MirrorHorButton.Transparent := true;
MirrorHorButton.Width := 16;
MirrorHorButton.Height := 16;
MirrorHorButton.OnClick := @OnMirrorModeButtonClick;
MirrorHorButton.hint := 'Mirror horizontally (M)';
AddElement(MirrorHorButton);

FloodFillButton := TOpenGL_Bevel.Create(Owner);
FloodFillButton.Left := 3;
FloodFillButton.Top := 359;
image := LoadAlphaColorGraphik('Bucket.bmp');
If image = 0 Then exit;
FloodFillButton.SetImage(image);
FloodFillButton.Transparent := true;
FloodFillButton.Width := 32;
FloodFillButton.Height := 32;
FloodFillButton.OnClick := @OnFloodFillButtonClick;
FloodFillButton.Hint := 'Floodfill tool (SHIFT+B)';
AddElement(FloodFillButton);

FloodFillModeButton := TOpenGL_Bevel.Create(Owner);
FloodFillModeButton.Left := 41;
FloodFillModeButton.Top := 359;
image := LoadAlphaColorGraphik('Matchmode.bmp');
If image = 0 Then exit;
FloodFillModeButton.SetImage(image);
FloodFillModeButton.Transparent := true;
FloodFillModeButton.Width := 32;
FloodFillModeButton.Height := 32;
FloodFillModeButton.OnClick := @OnFloodFillModeButtonClick;
FloodFillModeButton.Hint := 'Set color match conditions (T)';
AddElement(FloodFillModeButton);

PipetteButton := TOpenGL_Bevel.Create(Owner);
PipetteButton.Left := 3;
PipetteButton.Top := 394;
image := LoadAlphaColorGraphik('Pipette.bmp');
If image = 0 Then exit;
PipetteButton.SetImage(image);
PipetteButton.Transparent := true;
PipetteButton.Width := 32;
PipetteButton.Height := 32;
PipetteButton.OnClick := @OnPipetteButtonClick;
PipetteButton.Hint := 'Color pick tool (I)';
AddElement(PipetteButton);

ColorCurveButton := TOpenGL_Bevel.Create(Owner);
ColorCurveButton.Left := 3;
ColorCurveButton.Top := 429 + 10;
image := LoadAlphaColorGraphik('ColorCurve.bmp');
If image = 0 Then exit;
ColorCurveButton.SetImage(image);
ColorCurveButton.Transparent := true;
ColorCurveButton.Width := 32;
ColorCurveButton.Height := 32;
ColorCurveButton.OnClick := @OnColorCurveButton;
ColorCurveButton.Hint := 'Color curve tool (U)';
AddElement(ColorCurveButton);

ConvoluteButton := TOpenGL_Bevel.Create(Owner);
ConvoluteButton.Left := 41;
ConvoluteButton.Top := 429 + 10;
image := LoadAlphaColorGraphik('Convolute.bmp');
If image = 0 Then exit;
ConvoluteButton.SetImage(image);
ConvoluteButton.Transparent := true;
ConvoluteButton.Width := 32;
ConvoluteButton.Height := 32;
ConvoluteButton.OnClick := @OnConvoluteButton;
ConvoluteButton.Hint := 'Convolute tool';
AddElement(ConvoluteButton);

ColorPicDialog := TOpenGL_ColorPicDialog.Create(Owner);
If ColorPicDialog.CriticalError <> '' Then Begin
  fCriticalError := ColorPicDialog.CriticalError;
  exit;
End;
ColorPicDialog.left := 147 + 14;
ColorPicDialog.Top := 131;
ColorPicDialog.OnSetColor := @SetLeftColor;
ColorPicDialog.OnLoadColorPalette := @OnLoadColorPaletteButtonClick;
ColorPicDialog.OnSaveColorPalette := @OnSaveColorPaletteButtonClick;
AddElement(ColorPicDialog);

ColorPreview := TOpenGL_ForeBackGroundColorBox.Create(Owner);
ColorPreview.left := 76;
ColorPreview.Top := 428;
ColorPreview.Width := 49;
ColorPreview.Height := 49;
AddElement(ColorPreview);

ColorTransparent := TOpenGL_Bevel.Create(Owner);
ColorTransparent.Left := 147 - 29 + 12;
ColorTransparent.Top := 428;
image := LoadAlphaColorGraphik('Transparent.bmp');
If image = 0 Then exit;
ColorTransparent.SetImage(image);
ColorTransparent.Transparent := true;
ColorTransparent.Width := 28;
ColorTransparent.Height := 28;
ColorTransparent.OnMouseDown := @OnTransparentMouseDown;
ColorTransparent.Hint := 'Transparent';
AddElement(ColorTransparent);

Color1 := TOpenGL_ColorBox.Create(Owner);
Color1.Left := 147 + 14;
Color1.Top := 428;
Color1.Width := 32;
Color1.Height := 32;
Color1.OnClick := @OnColorClick;
Color1.OnDblClick := @OnColorDblClick;
Color1.Color := rgba(0, 102, 153, AlphaOpaque);
Color1.DefaultColor := rgba(0, 102, 153, AlphaOpaque);
Color1.LoweredColor := rgba(0, 0, 0, AlphaOpaque);
Color1.RaisedColor := rgba(0, 0, 0, AlphaOpaque);
Color1.OnMouseUp := @OnColorMouseUp;
Color1.Hint := 'Color 1 (CTRL+1)';
AddElement(Color1);

Color2 := TOpenGL_ColorBox.Create(Owner);
Color2.Left := 182 + 14;
Color2.Top := 428;
Color2.Width := 32;
Color2.Height := 32;
Color2.OnClick := @OnColorClick;
Color2.OnDblClick := @OnColorDblClick;
Color2.Color := rgba(51, 51, 255, AlphaOpaque);
Color2.DefaultColor := rgba(51, 51, 255, AlphaOpaque);
Color2.LoweredColor := rgba(0, 0, 0, AlphaOpaque);
Color2.RaisedColor := rgba(0, 0, 0, AlphaOpaque);
Color2.OnMouseUp := @OnColorMouseUp;
Color2.Hint := 'Color 2 (CTRL+2)';
AddElement(Color2);

Color3 := TOpenGL_ColorBox.Create(Owner);
Color3.Left := 217 + 14;
Color3.Top := 428;
Color3.Width := 32;
Color3.Height := 32;
Color3.OnClick := @OnColorClick;
Color3.OnDblClick := @OnColorDblClick;
Color3.Color := rgba(255, 0, 255, AlphaOpaque);
Color3.DefaultColor := rgba(255, 0, 255, AlphaOpaque);
Color3.LoweredColor := rgba(0, 0, 0, AlphaOpaque);
Color3.RaisedColor := rgba(0, 0, 0, AlphaOpaque);
Color3.OnMouseUp := @OnColorMouseUp;
Color3.Hint := 'Color 3 (CTRL+3)';
AddElement(Color3);

Color4 := TOpenGL_ColorBox.Create(Owner);
Color4.Left := 252 + 14;
Color4.Top := 428;
Color4.Width := 32;
Color4.Height := 32;
Color4.OnClick := @OnColorClick;
Color4.OnDblClick := @OnColorDblClick;
Color4.Color := rgba(204, 0, 0, AlphaOpaque);
Color4.DefaultColor := rgba(204, 0, 0, AlphaOpaque);
Color4.LoweredColor := rgba(0, 0, 0, AlphaOpaque);
Color4.RaisedColor := rgba(0, 0, 0, AlphaOpaque);
Color4.OnMouseUp := @OnColorMouseUp;
Color4.Hint := 'Color 4 (CTRL+4)';
AddElement(Color4);

Color5 := TOpenGL_ColorBox.Create(Owner);
Color5.Left := 287 + 14;
Color5.Top := 428;
Color5.Width := 32;
Color5.Height := 32;
Color5.OnClick := @OnColorClick;
Color5.OnDblClick := @OnColorDblClick;
Color5.Color := rgba(204, 153, 0, AlphaOpaque);
Color5.DefaultColor := rgba(204, 153, 0, AlphaOpaque);
Color5.LoweredColor := rgba(0, 0, 0, AlphaOpaque);
Color5.RaisedColor := rgba(0, 0, 0, AlphaOpaque);
Color5.OnMouseUp := @OnColorMouseUp;
Color5.Hint := 'Color 5 (CTRL+5)';
AddElement(Color5);

Color6 := TOpenGL_ColorBox.Create(Owner);
Color6.Left := 322 + 14;
Color6.Top := 428;
Color6.Width := 32;
Color6.Height := 32;
Color6.OnClick := @OnColorClick;
Color6.OnDblClick := @OnColorDblClick;
Color6.Color := rgba(0, 153, 51, AlphaOpaque);
Color6.DefaultColor := rgba(0, 153, 51, AlphaOpaque);
Color6.LoweredColor := rgba(0, 0, 0, AlphaOpaque);
Color6.RaisedColor := rgba(0, 0, 0, AlphaOpaque);
Color6.OnMouseUp := @OnColorMouseUp;
Color6.Hint := 'Color 6 (CTRL+6)';
AddElement(Color6);

Color7 := TOpenGL_ColorBox.Create(Owner);
Color7.Left := 357 + 14;
Color7.Top := 428;
Color7.Width := 32;
Color7.Height := 32;
Color7.OnClick := @OnColorClick;
Color7.OnDblClick := @OnColorDblClick;
Color7.Color := rgba(255, 255, 255, AlphaOpaque);
Color7.DefaultColor := rgba(255, 255, 255, AlphaOpaque);
Color7.LoweredColor := rgba(0, 0, 0, AlphaOpaque);
Color7.RaisedColor := rgba(0, 0, 0, AlphaOpaque);
Color7.OnMouseUp := @OnColorMouseUp;
Color7.Hint := 'Color 7 (CTRL+7)';
AddElement(Color7);

Color8 := TOpenGL_ColorBox.Create(Owner);
Color8.Left := 392 + 14;
Color8.Top := 428;
Color8.Width := 32;
Color8.Height := 32;
Color8.OnClick := @OnColorClick;
Color8.OnDblClick := @OnColorDblClick;
Color8.Color := rgba(0, 0, 0, AlphaOpaque);
Color8.DefaultColor := rgba(0, 0, 0, AlphaOpaque);
Color8.LoweredColor := rgba(0, 0, 0, AlphaOpaque);
Color8.RaisedColor := rgba(0, 0, 0, AlphaOpaque);
Color8.OnMouseUp := @OnColorMouseUp;
Color8.Hint := 'Color 8 (CTRL+8)';
AddElement(Color8);

InfoLabel := TOpenGl_Label.Create(owner, '');
InfoLabel.Left := 427 + 14;
InfoLabel.Top := 445;
InfoLabel.Caption := '';
InfoLabel.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
AddElement(InfoLabel);

InfoDetailLabel := TOpenGl_Label.Create(owner, '');
InfoDetailLabel.Left := 523 + 14;
InfoDetailLabel.Top := 445;
InfoDetailLabel.Caption := '';
InfoDetailLabel.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
AddElement(InfoDetailLabel);

