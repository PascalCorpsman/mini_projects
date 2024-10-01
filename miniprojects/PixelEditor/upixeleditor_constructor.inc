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

FElements := Nil;

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


//         RotateClockwise90: TOpenGL_Bevel;
//    Rotate180: TOpenGL_Bevel;
//    RotateAngle: TOpenGL_Bevel;







SelectLayerButton := TOpenGL_Bevel.Create(Owner);
SelectLayerButton.Left := 640 - 35;
SelectLayerButton.Top := 480 - 35;
//image := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Exit.bmp', Fuchsia, smClamp);
//SelectLayerButton.SetImage(image);
SelectLayerButton.Transparent := true;
SelectLayerButton.Width := 32;
SelectLayerButton.Height := 32;
SelectLayerButton.OnClick := @OnSelectLayerButtonClick;
AddElement(SelectLayerButton);

