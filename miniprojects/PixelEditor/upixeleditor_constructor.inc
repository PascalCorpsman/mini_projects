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

NewButton := TOpenGL_Bevel.Create(Owner);
Newbutton.Left := 3;
Newbutton.Top := 3;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'New.bmp', Fuchsia, smClamp);
//Newbutton.SetImage(image);
Newbutton.Transparent := true;
Newbutton.Width := 32;
Newbutton.Height := 32;
NewButton.OnClick := @OnNewButtonClick;
AddElement(NewButton);

OpenButton := TOpenGL_Bevel.Create(Owner);
OpenButton.Left := 3 + 32 + 3;
OpenButton.Top := 3;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'New.bmp', Fuchsia, smClamp);
//OpenButton.SetImage(image);
OpenButton.Transparent := true;
OpenButton.Width := 32;
OpenButton.Height := 32;
OpenButton.OnClick := @OnOpenButtonClick;
AddElement(OpenButton);

SaveButton := TOpenGL_Bevel.Create(Owner);
SaveButton.Left := 3 + 32 + 3 + 32 + 3;
SaveButton.Top := 3;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'New.bmp', Fuchsia, smClamp);
//SaveButton.SetImage(image);
SaveButton.Transparent := true;
SaveButton.Width := 32;
SaveButton.Height := 32;
SaveButton.OnClick := @OnSaveButtonClick;
AddElement(SaveButton);

SaveAsButton := TOpenGL_Bevel.Create(Owner);
SaveAsButton.Left := 3 + 32 + 3 + 32 + 3 + 32 + 3;
SaveAsButton.Top := 3;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'New.bmp', Fuchsia, smClamp);
//SaveAsButton.SetImage(image);
SaveAsButton.Transparent := true;
SaveAsButton.Width := 32;
SaveAsButton.Height := 32;
SaveAsButton.OnClick := @OnSaveAsButtonClick;
AddElement(SaveAsButton);

ExitButton := TOpenGL_Bevel.Create(Owner);
Exitbutton.Left := 143;
Exitbutton.Top := 3;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
Exitbutton.SetImage(image);
Exitbutton.Transparent := true;
Exitbutton.Width := 32;
Exitbutton.Height := 32;
ExitButton.OnClick := @OnExitButtonClick;
AddElement(ExitButton);

GridButton := TOpenGL_Bevel.Create(Owner);
GridButton.Left := 213;
GridButton.Top := 3;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Grid.bmp', Fuchsia, smClamp);
GridButton.SetImage(image);
GridButton.Transparent := true;
GridButton.Width := 32;
GridButton.Height := 32;
GridButton.OnClick := @OnGridButtonClick;
AddElement(GridButton);


ZoomOutButton := TOpenGL_Bevel.Create(Owner);
ZoomOutButton.Left := 262;
ZoomOutButton.Top := 10;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
//ZoomOutButton.SetImage(image);
ZoomOutButton.Transparent := true;
ZoomOutButton.Width := 16;
ZoomOutButton.Height := 16;
ZoomOutButton.OnClick := @OnZoomOutButtonClick;
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
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
//ZoomInButton.SetImage(image);
ZoomInButton.Transparent := true;
ZoomInButton.Width := 16;
ZoomInButton.Height := 16;
ZoomInButton.OnClick := @OnZoomInButtonClick;
AddElement(ZoomInButton);

OptionsButton := TOpenGL_Bevel.Create(Owner);
OptionsButton.Left := 570;
OptionsButton.Top := 3;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Settings.bmp', Fuchsia, smClamp);
OptionsButton.SetImage(image);
OptionsButton.Transparent := true;
OptionsButton.Width := 32;
OptionsButton.Height := 32;
OptionsButton.OnClick := @OnOptionsButtonClick;
AddElement(OptionsButton);

UndoButton := TOpenGL_Bevel.Create(Owner);
UndoButton.Left := 605;
UndoButton.Top := 3;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Undo.bmp', Fuchsia, smClamp);
UndoButton.SetImage(image);
UndoButton.Transparent := true;
UndoButton.Width := 32;
UndoButton.Height := 32;
UndoButton.OnClick := @OnUndoButtonClick;
AddElement(UndoButton);

SelectButton := TOpenGL_Bevel.Create(Owner);
SelectButton.Left := 3;
SelectButton.Top := 41;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
//SelectButton.SetImage(image);
SelectButton.Transparent := true;
SelectButton.Width := 32;
SelectButton.Height := 32;
SelectButton.OnClick := @OnSelectButtonClick;
AddElement(SelectButton);

SelectModeButton := TOpenGL_ToggleButton.Create(Owner);
SelectModeButton.Left := 41;
SelectModeButton.Top := 41;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'SelModeLowered.bmp', Fuchsia, smClamp);
SelectModeButton.SetImage(image);
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'SelModeRaised.bmp', Fuchsia, smClamp);
SelectModeButton.SetDownImage(image);
SelectModeButton.Transparent := true;
SelectModeButton.Width := 32;
SelectModeButton.Height := 32;
//SelectModeButton.OnClick := @OnSelectModeButtonClick; -- Braucht es nicht, man kann ja den Style abfragen ;)
AddElement(SelectModeButton);

RotateCounterClockwise90 := TOpenGL_Bevel.Create(Owner);
RotateCounterClockwise90.Left := 3;
RotateCounterClockwise90.Top := 76;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
//RotateCounterClockwise90.SetImage(image);
RotateCounterClockwise90.Transparent := true;
RotateCounterClockwise90.Width := 16;
RotateCounterClockwise90.Height := 16;
RotateCounterClockwise90.OnClick := @OnRotateCounterClockwise90ButtonClick;
AddElement(RotateCounterClockwise90);

RotateClockwise90 := TOpenGL_Bevel.Create(Owner);
RotateClockwise90.Left := 21;
RotateClockwise90.Top := 76;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
//RotateClockwise90.SetImage(image);
RotateClockwise90.Transparent := true;
RotateClockwise90.Width := 16;
RotateClockwise90.Height := 16;
RotateClockwise90.OnClick := @OnRotateClockwise90ButtonClick;
AddElement(RotateClockwise90);

Rotate180 := TOpenGL_Bevel.Create(Owner);
Rotate180.Left := 21 + 18;
Rotate180.Top := 76;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
//Rotate180.SetImage(image);
Rotate180.Transparent := true;
Rotate180.Width := 16;
Rotate180.Height := 16;
Rotate180.OnClick := @OnRotate180ButtonClick;
AddElement(Rotate180);

RotateAngle := TOpenGL_Bevel.Create(Owner);
RotateAngle.Left := 21 + 18 + 18;
RotateAngle.Top := 76;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
//RotateAngle.SetImage(image);
RotateAngle.Transparent := true;
RotateAngle.Width := 16;
RotateAngle.Height := 16;
RotateAngle.OnClick := @OnRotateAngleButtonClick;
AddElement(RotateAngle);

BrightenButton := TOpenGL_Bevel.Create(Owner);
BrightenButton.Left := 3;
BrightenButton.Top := 95;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Brighten.bmp', Fuchsia, smClamp);
BrightenButton.SetImage(image);
BrightenButton.Transparent := true;
BrightenButton.Width := 32;
BrightenButton.Height := 32;
BrightenButton.OnClick := @OnBrightenButtonClick;
AddElement(BrightenButton);

DarkenButton := TOpenGL_Bevel.Create(Owner);
DarkenButton.Left := 41;
DarkenButton.Top := 95;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Darken.bmp', Fuchsia, smClamp);
DarkenButton.SetImage(image);
DarkenButton.Transparent := true;
DarkenButton.Width := 32;
DarkenButton.Height := 32;
DarkenButton.OnClick := @OnDarkenButtonClick;
AddElement(DarkenButton);

CurserSize1 := TOpenGL_Bevel.Create(Owner);
CurserSize1.Left := 3;
CurserSize1.Top := 130;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Size1.bmp', Fuchsia, smClamp);
CurserSize1.SetImage(image);
CurserSize1.Transparent := true;
CurserSize1.Width := 16;
CurserSize1.Height := 16;
CurserSize1.OnClick := @OnCurserSizeButtonClick;
AddElement(CurserSize1);

CurserSize2 := TOpenGL_Bevel.Create(Owner);
CurserSize2.Left := 21;
CurserSize2.Top := 130;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Size2.bmp', Fuchsia, smClamp);
CurserSize2.SetImage(image);
CurserSize2.Transparent := true;
CurserSize2.Width := 16;
CurserSize2.Height := 16;
CurserSize2.OnClick := @OnCurserSizeButtonClick;
AddElement(CurserSize2);

CurserSize3 := TOpenGL_Bevel.Create(Owner);
CurserSize3.Left := 21 + 18;
CurserSize3.Top := 130;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Size3.bmp', Fuchsia, smClamp);
CurserSize3.SetImage(image);
CurserSize3.Transparent := true;
CurserSize3.Width := 16;
CurserSize3.Height := 16;
CurserSize3.OnClick := @OnCurserSizeButtonClick;
AddElement(CurserSize3);

CurserSize4 := TOpenGL_Bevel.Create(Owner);
CurserSize4.Left := 21 + 18 + 18;
CurserSize4.Top := 130;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Size4.bmp', Fuchsia, smClamp);
CurserSize4.SetImage(image);
CurserSize4.Transparent := true;
CurserSize4.Width := 16;
CurserSize4.Height := 16;
CurserSize4.OnClick := @OnCurserSizeButtonClick;
AddElement(CurserSize4);

EraserButton := TOpenGL_Bevel.Create(Owner);
EraserButton.Left := 3;
EraserButton.Top := 149;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Eraser.bmp', Fuchsia, smClamp);
EraserButton.SetImage(image);
EraserButton.Transparent := true;
EraserButton.Width := 32;
EraserButton.Height := 32;
EraserButton.OnClick := @OnEraserButtonClick;
AddElement(EraserButton);

PencilButton := TOpenGL_Bevel.Create(Owner);
PencilButton.Left := 3;
PencilButton.Top := 184;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Penciltool.bmp', Fuchsia, smClamp);
PencilButton.SetImage(image);
PencilButton.Transparent := true;
PencilButton.Width := 32;
PencilButton.Height := 32;
PencilButton.OnClick := @OnPencilButtonClick;
AddElement(PencilButton);

CursorRoundShape1 := TOpenGL_Bevel.Create(Owner);
CursorRoundShape1.Left := 40;
CursorRoundShape1.Top := 174;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'RoundShape1.bmp', Fuchsia, smClamp);
CursorRoundShape1.SetImage(image);
CursorRoundShape1.Transparent := true;
CursorRoundShape1.Width := 16;
CursorRoundShape1.Height := 16;
CursorRoundShape1.OnClick := @OnCursorShapeClick;
AddElement(CursorRoundShape1);

CursorRoundShape2 := TOpenGL_Bevel.Create(Owner);
CursorRoundShape2.Left := 40;
CursorRoundShape2.Top := 192;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'RoundShape2.bmp', Fuchsia, smClamp);
CursorRoundShape2.SetImage(image);
CursorRoundShape2.Transparent := true;
CursorRoundShape2.Width := 16;
CursorRoundShape2.Height := 16;
CursorRoundShape2.OnClick := @OnCursorShapeClick;
AddElement(CursorRoundShape2);

CursorRoundShape3 := TOpenGL_Bevel.Create(Owner);
CursorRoundShape3.Left := 40;
CursorRoundShape3.Top := 210;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'RoundShape3.bmp', Fuchsia, smClamp);
CursorRoundShape3.SetImage(image);
CursorRoundShape3.Transparent := true;
CursorRoundShape3.Width := 16;
CursorRoundShape3.Height := 16;
CursorRoundShape3.OnClick := @OnCursorShapeClick;
AddElement(CursorRoundShape3);

CursorSquareShape1 := TOpenGL_Bevel.Create(Owner);
CursorSquareShape1.Left := 58;
CursorSquareShape1.Top := 174;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'SquareShape1.bmp', Fuchsia, smClamp);
CursorSquareShape1.SetImage(image);
CursorSquareShape1.Transparent := true;
CursorSquareShape1.Width := 16;
CursorSquareShape1.Height := 16;
CursorSquareShape1.OnClick := @OnCursorShapeClick;
AddElement(CursorSquareShape1);

CursorSquareShape2 := TOpenGL_Bevel.Create(Owner);
CursorSquareShape2.Left := 58;
CursorSquareShape2.Top := 192;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'SquareShape2.bmp', Fuchsia, smClamp);
CursorSquareShape2.SetImage(image);
CursorSquareShape2.Transparent := true;
CursorSquareShape2.Width := 16;
CursorSquareShape2.Height := 16;
CursorSquareShape2.OnClick := @OnCursorShapeClick;
AddElement(CursorSquareShape2);

CursorSquareShape3 := TOpenGL_Bevel.Create(Owner);
CursorSquareShape3.Left := 58;
CursorSquareShape3.Top := 210;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'SquareShape3.bmp', Fuchsia, smClamp);
CursorSquareShape3.SetImage(image);
CursorSquareShape3.Transparent := true;
CursorSquareShape3.Width := 16;
CursorSquareShape3.Height := 16;
CursorSquareShape3.OnClick := @OnCursorShapeClick;
AddElement(CursorSquareShape3);

LineButton := TOpenGL_Bevel.Create(Owner);
LineButton.Left := 3;
LineButton.Top := 219;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Linetool.bmp', Fuchsia, smClamp);
LineButton.SetImage(image);
LineButton.Transparent := true;
LineButton.Width := 32;
LineButton.Height := 32;
LineButton.OnClick := @OnLineButtonClick;
AddElement(LineButton);

CircleButton := TOpenGL_Bevel.Create(Owner);
CircleButton.Left := 3;
CircleButton.Top := 254;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Ellipse.bmp', Fuchsia, smClamp);
CircleButton.SetImage(image);
CircleButton.Transparent := true;
CircleButton.Width := 32;
CircleButton.Height := 32;
CircleButton.OnClick := @OnCircleButtonClick;
AddElement(CircleButton);

SquareButton := TOpenGL_Bevel.Create(Owner);
SquareButton.Left := 3;
SquareButton.Top := 289;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Rectangle.bmp', Fuchsia, smClamp);
SquareButton.SetImage(image);
SquareButton.Transparent := true;
SquareButton.Width := 32;
SquareButton.Height := 32;
SquareButton.OnClick := @OnSquareButtonClick;
AddElement(SquareButton);

OutlineButton := TOpenGL_Bevel.Create(Owner);
OutlineButton.Left := 40;
OutlineButton.Top := 279;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Outline.bmp', Fuchsia, smClamp);
OutlineButton.SetImage(image);
OutlineButton.Transparent := true;
OutlineButton.Width := 16;
OutlineButton.Height := 16;
OutlineButton.OnClick := @OnOutlineButtonClick;
AddElement(OutlineButton);

FilledButton := TOpenGL_Bevel.Create(Owner);
FilledButton.Left := 58;
FilledButton.Top := 279;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Filled.bmp', Fuchsia, smClamp);
FilledButton.SetImage(image);
FilledButton.Transparent := true;
FilledButton.Width := 16;
FilledButton.Height := 16;
FilledButton.OnClick := @OnOutlineButtonClick;
AddElement(FilledButton);

MirrorButton := TOpenGL_Bevel.Create(Owner);
MirrorButton.Left := 3;
MirrorButton.Top := 324;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Mirror.bmp', Fuchsia, smClamp);
MirrorButton.SetImage(image);
MirrorButton.Transparent := true;
MirrorButton.Width := 32;
MirrorButton.Height := 32;
MirrorButton.OnClick := @OnMirrorButtonClick;
AddElement(MirrorButton);

MirrorCenterButton := TOpenGL_ToggleButton.create(Owner);
MirrorCenterButton.Left := 58;
MirrorCenterButton.Top := 322;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Mirror_Center.bmp', Fuchsia, smClamp);
MirrorCenterButton.SetImage(image);
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Mirror_Axis.bmp', Fuchsia, smClamp);
MirrorCenterButton.SetDownImage(image);
MirrorCenterButton.Transparent := true;
MirrorCenterButton.Width := 16;
MirrorCenterButton.Height := 16;
//MirrorCenterButton.OnClick := @OnMirrorCenterButtonClick; -- Braucht es nicht man kann ja den Style abfragen
AddElement(MirrorCenterButton);

Mirror4Button := TOpenGL_Bevel.Create(Owner);
Mirror4Button.Left := 40;
Mirror4Button.Top := 322;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Mirror4.bmp', Fuchsia, smClamp);
Mirror4Button.SetImage(image);
Mirror4Button.Transparent := true;
Mirror4Button.Width := 16;
Mirror4Button.Height := 16;
Mirror4Button.OnClick := @OnMirrorModeButtonClick;
AddElement(Mirror4Button);

MirrorVertButton := TOpenGL_Bevel.Create(Owner);
MirrorVertButton.Left := 40;
MirrorVertButton.Top := 340;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Mirror_Ver.bmp', Fuchsia, smClamp);
MirrorVertButton.SetImage(image);
MirrorVertButton.Transparent := true;
MirrorVertButton.Width := 16;
MirrorVertButton.Height := 16;
MirrorVertButton.OnClick := @OnMirrorModeButtonClick;
AddElement(MirrorVertButton);

MirrorHorButton := TOpenGL_Bevel.Create(Owner);
MirrorHorButton.Left := 58;
MirrorHorButton.Top := 340;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Mirror_Hor.bmp', Fuchsia, smClamp);
MirrorHorButton.SetImage(image);
MirrorHorButton.Transparent := true;
MirrorHorButton.Width := 16;
MirrorHorButton.Height := 16;
MirrorHorButton.OnClick := @OnMirrorModeButtonClick;
AddElement(MirrorHorButton);

FloodFillButton := TOpenGL_Bevel.Create(Owner);
FloodFillButton.Left := 3;
FloodFillButton.Top := 359;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Bucket.bmp', Fuchsia, smClamp);
FloodFillButton.SetImage(image);
FloodFillButton.Transparent := true;
FloodFillButton.Width := 32;
FloodFillButton.Height := 32;
FloodFillButton.OnClick := @OnFloodFillButtonClick;
AddElement(FloodFillButton);

FloodFillModeButton := TOpenGL_Bevel.Create(Owner);
FloodFillModeButton.Left := 41;
FloodFillModeButton.Top := 359;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'FloodfillMode.bmp', Fuchsia, smClamp);
FloodFillModeButton.SetImage(image);
FloodFillModeButton.Transparent := true;
FloodFillModeButton.Width := 32;
FloodFillModeButton.Height := 32;
FloodFillModeButton.OnClick := @OnFloodFillModeButtonClick;
AddElement(FloodFillModeButton);

ColorPickButton := TOpenGL_Bevel.Create(Owner);
ColorPickButton.Left := 3;
ColorPickButton.Top := 394;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Pincette.bmp', Fuchsia, smClamp);
ColorPickButton.SetImage(image);
ColorPickButton.Transparent := true;
ColorPickButton.Width := 32;
ColorPickButton.Height := 32;
ColorPickButton.OnClick := @OnColorPickButtonClick;
AddElement(ColorPickButton);

ColorPicDialog := TOpenGL_ColorPicDialog.Create(Owner);
ColorPicDialog.left := 147;
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

ColorTransparent := TOpenGL_TransparentColorBox.Create(Owner);
ColorTransparent.Left := 128;
ColorTransparent.Top := 428;
image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Transparent.bmp', Fuchsia, smClamp);
ColorTransparent.SetImage(image);
ColorTransparent.Transparent := true;
ColorTransparent.Width := 16;
ColorTransparent.Height := 16;
ColorTransparent.OnClick := @OnSelectTransparentColorClick;
ColorTransparent.LoweredColor := rgba(0, 0, 0, 0);
ColorTransparent.RaisedColor := rgba(0, 0, 0, 0);
ColorTransparent.OnMouseUp := @OnColorMouseUp;
AddElement(ColorTransparent);

Color1 := TOpenGL_ColorBox.Create(Owner);
Color1.Left := 147;
Color1.Top := 428;
Color1.Width := 32;
Color1.Height := 32;
Color1.OnClick := @OnColorClick;
Color1.OnDblClick := @OnColorDblClick;
Color1.Color := rgba(0, 102, 153, 0);
Color1.DefaultColor := rgba(0, 102, 153, 0);
Color1.LoweredColor := rgba(0, 0, 0, 0);
Color1.RaisedColor := rgba(0, 0, 0, 0);
Color1.OnMouseUp := @OnColorMouseUp;
AddElement(Color1);

Color2 := TOpenGL_ColorBox.Create(Owner);
Color2.Left := 182;
Color2.Top := 428;
Color2.Width := 32;
Color2.Height := 32;
Color2.OnClick := @OnColorClick;
Color2.OnDblClick := @OnColorDblClick;
Color2.Color := rgba(51, 51, 255, 0);
Color2.DefaultColor := rgba(51, 51, 255, 0);
Color2.LoweredColor := rgba(0, 0, 0, 0);
Color2.RaisedColor := rgba(0, 0, 0, 0);
Color2.OnMouseUp := @OnColorMouseUp;
AddElement(Color2);

Color3 := TOpenGL_ColorBox.Create(Owner);
Color3.Left := 217;
Color3.Top := 428;
Color3.Width := 32;
Color3.Height := 32;
Color3.OnClick := @OnColorClick;
Color3.OnDblClick := @OnColorDblClick;
Color3.Color := rgba(204, 0, 204, 0);
Color3.DefaultColor := rgba(204, 0, 204, 0);
Color3.LoweredColor := rgba(0, 0, 0, 0);
Color3.RaisedColor := rgba(0, 0, 0, 0);
Color3.OnMouseUp := @OnColorMouseUp;
AddElement(Color3);

Color4 := TOpenGL_ColorBox.Create(Owner);
Color4.Left := 252;
Color4.Top := 428;
Color4.Width := 32;
Color4.Height := 32;
Color4.OnClick := @OnColorClick;
Color4.OnDblClick := @OnColorDblClick;
Color4.Color := rgba(204, 0, 0, 0);
Color4.DefaultColor := rgba(204, 0, 0, 0);
Color4.LoweredColor := rgba(0, 0, 0, 0);
Color4.RaisedColor := rgba(0, 0, 0, 0);
Color4.OnMouseUp := @OnColorMouseUp;
AddElement(Color4);

Color5 := TOpenGL_ColorBox.Create(Owner);
Color5.Left := 287;
Color5.Top := 428;
Color5.Width := 32;
Color5.Height := 32;
Color5.OnClick := @OnColorClick;
Color5.OnDblClick := @OnColorDblClick;
Color5.Color := rgba(204, 153, 0, 0);
Color5.DefaultColor := rgba(204, 153, 0, 0);
Color5.LoweredColor := rgba(0, 0, 0, 0);
Color5.RaisedColor := rgba(0, 0, 0, 0);
Color5.OnMouseUp := @OnColorMouseUp;
AddElement(Color5);

Color6 := TOpenGL_ColorBox.Create(Owner);
Color6.Left := 322;
Color6.Top := 428;
Color6.Width := 32;
Color6.Height := 32;
Color6.OnClick := @OnColorClick;
Color6.OnDblClick := @OnColorDblClick;
Color6.Color := rgba(0, 153, 51, 0);
Color6.DefaultColor := rgba(0, 153, 51, 0);
Color6.LoweredColor := rgba(0, 0, 0, 0);
Color6.RaisedColor := rgba(0, 0, 0, 0);
Color6.OnMouseUp := @OnColorMouseUp;
AddElement(Color6);

Color7 := TOpenGL_ColorBox.Create(Owner);
Color7.Left := 357;
Color7.Top := 428;
Color7.Width := 32;
Color7.Height := 32;
Color7.OnClick := @OnColorClick;
Color7.OnDblClick := @OnColorDblClick;
Color7.Color := rgba(255, 255, 255, 0);
Color7.DefaultColor := rgba(255, 255, 255, 0);
Color7.LoweredColor := rgba(0, 0, 0, 0);
Color7.RaisedColor := rgba(0, 0, 0, 0);
Color7.OnMouseUp := @OnColorMouseUp;
AddElement(Color7);

Color8 := TOpenGL_ColorBox.Create(Owner);
Color8.Left := 392;
Color8.Top := 428;
Color8.Width := 32;
Color8.Height := 32;
Color8.OnClick := @OnColorClick;
Color8.OnDblClick := @OnColorDblClick;
Color8.Color := rgba(0, 0, 0, 0);
Color8.DefaultColor := rgba(0, 0, 0, 0);
Color8.LoweredColor := rgba(0, 0, 0, 0);
Color8.RaisedColor := rgba(0, 0, 0, 0);
Color8.OnMouseUp := @OnColorMouseUp;
AddElement(Color8);

AktColorInfoLabel := TOpenGl_Label.Create(owner, '');
AktColorInfoLabel.Left := 147;
AktColorInfoLabel.Top := 463;
AktColorInfoLabel.Caption := '';
AktColorInfoLabel.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
AddElement(AktColorInfoLabel);

InfoLabel := TOpenGl_Label.Create(owner, '');
InfoLabel.Left := 427;
InfoLabel.Top := 445;
InfoLabel.Caption := '';
InfoLabel.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
AddElement(InfoLabel);

InfoDetailLabel := TOpenGl_Label.Create(owner, '');
InfoDetailLabel.Left := 523;
InfoDetailLabel.Top := 445;
InfoDetailLabel.Caption := '';
InfoDetailLabel.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
AddElement(InfoDetailLabel);

// Den Verstehen wir noch nicht -> Raus
//SelectLayerButton := TOpenGL_Bevel.Create(Owner);
//SelectLayerButton.Left := 640 - 35;
//SelectLayerButton.Top := 480 - 35;
////image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
////SelectLayerButton.SetImage(image);
//SelectLayerButton.Transparent := true;
//SelectLayerButton.Width := 32;
//SelectLayerButton.Height := 32;
//SelectLayerButton.OnClick := @OnSelectLayerButtonClick;
//AddElement(SelectLayerButton);




