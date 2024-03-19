(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of SimpleRay                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

Type
  TForm4 = Class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    TreeView1: TTreeView;
    Procedure Button1Click(Sender: TObject);
    Procedure TreeView1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure TreeView1KeyPress(Sender: TObject; Var Key: Char);
    Procedure Memo1KeyPress(Sender: TObject; Var Key: Char);
    Procedure Button1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form4: TForm4;

Implementation

Uses unit1;

{$R *.lfm}

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm4.TreeView1Click(Sender: TObject);
Var
  s: String;
Begin
  memo1.Lines.clear;
  s := TreeView1.Selected.text;
  If s = 'Allgemein' Then Begin
    memo1.Lines.Text := 'Simple Ray ver. ' + unit1.version + LineEnding + LineEnding + 'Erstellt von Corpsman' + LineEnding + 'Support : www.Corpsman.de' + LineEnding + LineEnding +
      'Dieses Programm ist mein 1. Versuch zum Thema Raytracing, nebenbei wollte ich noch die Themen Parcing und Copmpilerbau ausprobieren.' + LineEnding + LineEnding +
      'Die Bedingung ist recht einfach mittels des Editors im Hauptformular eine Codevervollständigung ist mittels STRG+Leertaste aktivierbar.' + LineEnding +
      'Ein Klick mit der Rechten Maustaste in das Editorfeld zeigt welche weiteren Optionen der Editor bietet.' + LineEnding + LineEnding +
      'Um alle weiteren Möglichkeiten die der Raytracer bietet zu sehen einfach die Datei Sample.scene laden.';
  End;
  If s = 'Raytracer' Then Begin
    memo1.Lines.Text := 'Im Raytracer Tag können generelle Einstellungen für den Raytracer vorgenommen werden.';
  End;
  If s = 'Imagewidth' Then Begin
    memo1.Lines.Text := 'Imagewidth' + LineEnding + LineEnding +
      'x = 400 = Integer > 0 ' + LineEnding + LineEnding +
      'Gibt die Breite des resultierenden Bildes in Pixeln an.';
  End;
  If s = 'Imageheight' Then Begin
    memo1.Lines.Text := 'Imageheight' + LineEnding + LineEnding +
      'x = 400 = Integer > 0 ' + LineEnding + LineEnding +
      'Gibt die Höhe des resultierenden Bildes in Pixeln an.';
  End;
  If s = 'Mindistance' Then Begin
    memo1.Lines.Text := 'Mindistance' + LineEnding + LineEnding +
      'x = 0.00001 = Single > 0' + LineEnding + LineEnding +
      'Gibt an ab welcher Minimalen Toleranz zwei Float Werte als unterschiedlich' + LineEnding +
      'erkannt werden.' + LineEnding + LineEnding +
      'ACHTUNG dieser Wert sollte nur von sehr Erfahrenen Usern geändert werden, da' + LineEnding +
      'er unter umständen alle Berechnungen des Raytracers zunichte machen kann und' + LineEnding +
      'oder zu Programmabstürzen führen kann.';
  End;
  If s = 'Maxdistance' Then Begin
    memo1.Lines.Text := 'Maxdistance' + LineEnding + LineEnding +
      'x = 10000000000.0 = Single > 0' + LineEnding + LineEnding +
      'Gibt an welchen Maximalen Wert ein Float erreichen kann.' + LineEnding + LineEnding +
      'ACHTUNG dieser Wert sollte nur von sehr Erfahrenen Usern geändert werden, da' + LineEnding +
      'er unter umständen alle Berechnungen des Raytracers zunichte machen kann und' + LineEnding +
      'oder zu Programmabstürzen führen kann.';
  End;
  If s = 'Lightoverflowmode' Then Begin
    memo1.Lines.Text := 'Lightoverflowmode' + LineEnding + LineEnding +
      'x = Clamp = ( Clamp, Scale )'#13#13 +
      'Wird während den Berechnungen ein Farbwert erzeugt der einen Farbanteil in R,G,B über 1 hat so wird ' +
      'so mus dieser Wert bei der ausgabe wieder in das Intervall [0 .. 1] zurückskalliert werden. ' +
      'Dies kann mittels Clamp = Abschneiden, oder Scale geschehen.' + LineEnding + LineEnding +
      'Bei scale bleibt die eigentlich errechnete Farbe erhalten. Bei Clamp kann es durch das Abschneiden ' +
      'der Farbwerte zu Farbverschiebungen kommen.';
  End;
  If s = 'Orthoprojection' Then Begin
    memo1.Lines.Text := 'Orthoprojection' + LineEnding + LineEnding + 'x = off = ( on, off )' + LineEnding + LineEnding +
      'Die Option Orthoprojection erlaubt es die Perspektivische Verzerrung ab zu schalten, in kombination mit ' +
      'dieser Option sollte stetzt auch der Befehl "Ortho" benutzt werden.';
  End;
  If s = 'Ortho' Then Begin
    memo1.Lines.Text := 'Ortho' + LineEnding + LineEnding + 'x = ((0.0,0.0),(1.0,1.0),1.0) = ((x,y),(z,a),b) = ( BottomLeft,TopRight,Z-Direction) = x,y,z,a,b = Single' + LineEnding + LineEnding +
      'ist die Option Orthoprojection an so rendert der Raytracer ein Bild ' +
      'innerhalb von BottomLeft und TopRight in der XY-Ebene, z gibt dabei die ' +
      'Blickrichtung an.' + LineEnding + LineEnding +
      'z >= 0 = Blickrichtung in Positive Z-Achse' + LineEnding +
      'z < 0 = Blickrichtung in Negative Z- Achse';
  End;
  If s = 'Cullfaceing' Then Begin
    memo1.lines.text := 'Cullfaceing' + LineEnding + LineEnding + 'x = on, off'#13#13 +
      'Ist die Option Cullfaceing angeschaltet so werden je nach Cullface Mode Sichtbar oder nicht.';
  End;
  If s = 'Cullfacemode' Then Begin
    memo1.lines.text := 'Cullfacemode' + LineEnding + LineEnding + 'x = front, back' + LineEnding + LineEnding +
      'Ist die Option Cullfaceing angeschaltet und werden die Primitive Korreckt übergeben.' + LineEnding +
      'Die Punkte von Dreiecken und Quads müssen im Gegenuhrzeigersinn angegeben werden.' + LineEnding + LineEnding +
      'Dann werden im Mode Front die Ausenseiten gerendert, und im Mode Back nur die Innenseiten.' + LineEnding +
      'Blickt man von der Falschen Seite auf ein entsprechendes Primitiv dann ist dieses Durchsichtig.';
  End;
  If s = 'Backgroundcolor' Then Begin
    memo1.lines.text := 'Backgroundcolor' + LineEnding + LineEnding + 'x = (x,y,z) mit x,y,z = Single >= 0' + LineEnding + LineEnding +
      'Wenn der Ray-Strahl kein Primitive trifft so wird ihm entweder die Hintergrundfarbe oder bei eingeschaltetem Nebel die Nebelfarbe zugewiesen.';
  End;
  If s = 'Rekursiondepth' Then Begin
    memo1.lines.text := 'Rekursiondepth' + LineEnding + LineEnding + 'x = Integer >= 0'#13#13 +
      'Die Rekursionstiefe legt fest ob ein Strahl bei reflektion weitergeleitet wird oder nicht.'#13 +
      'Ist die Rekursionstiefe = 0 eingestellt so deaktiviert diese automatisch alle Reflexionen und Transparenzen.';
  End;
  If s = 'Eye' Then Begin
    memo1.lines.text := 'Eye' + LineEnding + LineEnding +
      '((x,y,z),(a,b,c),(d,e,f)) = (Position,Up,Direction) mit x,y,z,a,b,c,d,e,f = single'#13#13 +
      'Der Befehl Eye legt die Position, Upvector und Direction des Auges fest. Die eingegebenen Werte werden ' +
      'Automatisch normiert.' + LineEnding + LineEnding +
      'Alternativ zu Eye kann auch Look At benutzt werden.';
  End;
  If s = 'Lookat' Then Begin
    memo1.lines.text := 'Lookat' + LineEnding + LineEnding +
      '((x,y,z),(a,b,c),(d,e,f)) = (Position,Center,Up) mit x,y,z,a,b,c,d,e,f = single'#13#13 +
      'Als Alternative zu Eye kann hier die Position und der Upvektor angegeben werden.'#13 +
      'Center gibt die Koordinate an die in der Mitte des gerenderten Bildes liegen soll.';
  End;
  If s = 'Fog' Then Begin
    memo1.lines.text := 'Fog' + LineEnding + LineEnding +
      'x = on, off' + LineEnding + LineEnding +
      'In verbindung mit den Einstellungen Fogmin, Fogmax, Fogcolor und Fogmode kann mittels Fog = on ein Nebeleffeckt eingeschaltet werden.';
  End;
  If s = 'FogMin' Then Begin
    memo1.lines.text := 'FogMin' + LineEnding + LineEnding +
      'x = Single >= 0' + LineEnding + LineEnding +
      'Fogmin legt die Grenze fest ab der als Abstand vom Auge gemessen der Nebel beginnt.';
  End;
  If s = 'FogMax' Then Begin
    memo1.lines.text := 'FogMax' + LineEnding + LineEnding +
      'x = Single > 0' + LineEnding + LineEnding +
      'Fogmax legt die Grenze fest ab der als Abstand vom Auge gemessen der Nebel seine Volle Dichte erreicht hat.';
  End;
  If s = 'FogColor' Then Begin
    memo1.lines.text := 'FogColor' + LineEnding + LineEnding +
      'x = (x,y,z) mit x,y,z = Single >= 0' + LineEnding + LineEnding +
      'Legt die Farbe des Nebels Fest.';
  End;
  If s = 'FogMode' Then Begin
    memo1.lines.text := 'FogMode' + LineEnding + LineEnding +
      'x = linear, cos' + LineEnding + LineEnding +
      'Legt fest wie im Bereich Fogmin zu Fogmax in den Nebel übergeblendet wird.';
  End;
  If s = 'Variables' Then Begin
    memo1.lines.text := 'Variablen'#13#13 +
      'Der Rayparser unterstützt teilweise das Verwenden von Variablen hierbei müssen Variablen dem Folgenden Aufbau entsprechen :'#13#13 +
      'YX = Z;'#13#13 +
      'Hierbei steht X für ein Zeichen aus der Menge ["1","2","3","4","S","B","M"] diese Zeichen geben den Typ der Variable an.'#13#13 +
      'Y steht für eine Beliebige Zeichenkette aus der Menge ["a".."z","A".."Z","_","0".."9"] die mindestens 1 Zeichen Lang sein mus.'#13#13 +
      'Z ist der zugewiesene Wert je nach Variablentyp.'#13#13 +
      'Folgende Typen sind Verfügbar :'#13#13 +
      '          1 = Single'#13 +
      '          2 = 2D-Vector'#13 +
      '          3 = 3D-Vector'#13 +
      '          4 = 4D-Vector'#13 +
      '          S = String'#13 +
      '          B = Boolscher Wert'#13 +
      '          M = Texturinterpolationsmode'#13#13 +
      'Der Raytracer betrachtet alle Variablen als Global, und überschreibt vorhandene Variablen eventuell mit neuen Werten. Auch Variablen von eingebundenen Dateien ' +
      'können andere Variablen überschreiben und sind ebenfalls Global !'#13#13 +
      'Einige Beispiele :'#13#13 +
      'A1 = 0.0;'#13 +
      'B1 = -A1;'#13 +
      'BildnameS = "@Scenepath@Test.bmp";'#13 +
      'Lichter_1_bis 9_an_ausB = on;'#13 +
      'A3 = (1.0,0.0,0.0);'#13#13 +
      'ACHTUNG !!!!'#13 +
      'Der Aufruf "A1 = A1;" ist zwar Zulässig und wird vom Compiler Akzeptiert, jedoch bleibt der Wert der Variablen undefiniert, wenn er nicht vorher schon definiert wurde.'#13;
  End;
  If s = 'Light' Then Begin
    memo1.lines.text := 'Light'#13#13 +
      'Jede Scene benötigt mindestens eine Lichtquelle oder ein Material das Licht Emmitiert ( Wobei die Lichtemmitierenden Flächen ' +
      'keine anderen Objekte beleuchten können).';
  End;
  If s = 'Position' Then Begin
    memo1.lines.text := 'Position'#13#13 +
      'x = (x,y,z) mit x,y,z = Single'#13#13 +
      'Legt die Position eines Primitives fest, die Position ist bei einem Primitive nur verfügbar wenn dieses nicht über Points definiert wird.';
  End;
  If s = 'Active' Then Begin
    memo1.lines.text := 'Active'#13#13 +
      'x =  on, off'#13#13 +
      'Jede Lichtquelle kann aus und Angeschaltet werden, Standart = on';
  End;
  If s = 'Ambientcolor' Then Begin
    memo1.lines.text := 'Ambientcolor'#13#13 +
      '(x,y,z) mit x,y,z = Single >= 0'#13#13 +
      'Die Ambientcolor gibt bei Lichtquellen an mit welcher Farbe Ambient beleuchtet wird.'#13#13 +
      'Ist die Ambiente Farbe bei einem Primitive gesetzt so wird diese Farbe Komponentenweise mit dem Ambienten Lichtanteil der jeweiligen Lichtquelle ' +
      'multipliziert und trägt dann zur Gesamtfarbe bei. Ist für das Primitive eine Textur Definiert so wird zusätzlich noch der Farbwert der Textur ' +
      'Komponentenweise in den Ambienten Farbanteil mit einbezogen.'#13#13 +
      'Die Ambiente Farbe eignet sich gut zu Aufhellung des Primitives.';
  End;
  If s = 'Diffusecolor' Then Begin
    memo1.lines.text := 'Diffusecolor'#13#13 +
      '(x,y,z) mit x,y,z = Single >= 0'#13#13 +
      'Die Diffusecolor gibt bei Lichtquellen an mit welcher Farbe Diffus beleuchtet wird.'#13#13 +
      'Ist die Difusse Farbe bei einem Primitive gesetzt so wird diese Farbe Komponentenweise mit dem Diffusen Lichtanteil der jeweiligen Lichtquelle ' +
      'multipliziert und trägt dann zur Gesamtfarbe bei. Ist für das Primitive eine Textur Definiert so wird zusätzlich noch der Farbwert der Textur ' +
      'Komponentenweise in den Diffusen Farbanteil mit einbezogen.'#13#13 +
      'Die Diffuse Farbe eignet sich gut zum erzeugen eines Räumlichen Effecktes.';
  End;
  If s = 'Specularcolor' Then Begin
    memo1.lines.text := 'Specularcolor'#13#13 +
      '(x,y,z) mit x,y,z = Single >= 0'#13#13 +
      'Die Specularcolor gibt bei Lichtquellen an mit welcher Farbe Specular beleuchtet wird.'#13#13 +
      'Ist die Speculare Farbe bei einem Primitive gesetzt so wird diese Farbe Komponentenweise mit dem Specularen Lichtanteil der jeweiligen Lichtquelle ' +
      'multipliziert und trägt dann zur Gesamtfarbe bei. Ist für das Primitive eine Textur Definiert so wird zusätzlich noch der Farbwert der Textur ' +
      'Komponentenweise in den Specularen Farbanteil mit einbezogen.'#13#13 +
      'Die Speculare Farbe Erzeugt ein Highlight auf dem Primitiv, in Verbindung mit dem Highlightexponent kann die Größe des Highlights bestimmt werden.';
  End;
  If s = 'Material' Then Begin
    memo1.lines.text := 'Material'#13#13 +
      'Jedes Primitive hat seine Eigenen Materialeigenschaften'#13 +
      'Im Großen Ganzen sind die aber bis auf die Anzahl der'#13 +
      'Texturkoordinaten immer gleich.';
  End;
  If s = 'Texture' Then Begin
    memo1.lines.text := 'Texture'#13#13 +
      'Vom Raytracer Akzeptierte Bildformate'#13 +
      '*.bmp'#13 +
      '*.jpg'#13 +
      'Ein Bild wird entweder Absolut oder indirekt angeben'#13 +
      'z.B. :'#13 +
      'Texture = "C:\Test.bmp"; // Absolut'#13 +
      'oder'#13 +
      'der Präfix @Local@ steht hierbei für den Pfad von dem aus der Parser gestartet wurde.'#13 +
      'Texture = "@Local@Test.bmp"; // Hierbei wird die Biddatei im Verzeichnis von'#13 +
      'dem aus der Parser gestartet wurde geöffnet.'#13 +
      'Der Präfix @Scenepath@ steht für den Pfas in dem die *.scene Datei gespeichert wurde'#13 +
      'Das Aufrufen weiterer Verzeichnisse ist ebenfalls möglich in etwa wie'#13 +
      'Texture = "@Scenepath@BilderVerzeichnis\Test.bmp"';
  End;
  If s = 'Transparencetexture' Then Begin
    memo1.lines.text := 'Transparencetexture'#13#13 +
      'Vom Raytracer Akzeptierte Bildformate'#13 +
      '*.bmp'#13 +
      '*.jpg'#13 +
      'Ein Bild wird entweder Absolut oder indirekt angeben'#13 +
      'z.B. :'#13 +
      'Transparencetexture = "C:\Test.bmp"; // Absolut'#13 +
      'oder'#13 +
      'der Präfix @Local@ steht hierbei für den Pfad von dem aus der Parser gestartet wurde.'#13 +
      'Transparencetexture = "@Local@Test.bmp"; // Hierbei wird die Biddatei im Verzeichnis von'#13 +
      'dem aus der Parser gestartet wurde geöffnet.'#13 +
      'Der Präfix @Scenepath@ steht für den Pfas in dem die *.scene Datei gespeichert wurde'#13 +
      'Das Aufrufen weiterer Verzeichnisse ist ebenfalls möglich in etwa wie'#13 +
      'Transparencetexture = "@Scenepath@BilderVerzeichnis\Test.bmp"'#13#13 +
      'Transparencetexture überschreibt den Parameter Transparence des Materials und setzt für'#13 +
      'jeden Lookup den interpolierten Luminance wert aus der Transparencetexture als'#13 +
      'Transparence wert ein.'#13#13 +
      'Hierbei ist Schwarz ( RGB = 0,0,0 ) = Keine Transparenz'#13 +
      'Weis ( RGB = 255,255,255 ) = Volle Transparenz';
  End;
  If s = 'Shadowborder' Then Begin
    memo1.lines.text := 'Shadowborder'#13#13 +
      'x = Single >= 0'#13#13 +
      'Die Kugeln bieten die Möglichkeit der Weichen Schatten auf der Kugel'#13 +
      'Diese können zur besseren Darstellung aktiviert werden.'#13 +
      'X = 0 = aus'#13 +
      'X > 0 = eingeschaltet. Shadowborder wird auf den Radius der Kugel'#13 +
      'scaliert X = 1 = Kugelradius';
  End;
  If s = 'Shadowbordermode' Then Begin
    memo1.lines.text := 'Shadowbordermode'#13#13 +
      'x = linear, Cos'#13#13 +
      'ist die Shadowborder aktiviert dann kann hier ausgewählt werden ob'#13 +
      'der Schatten Liniar oder über die Cosinusfunction angenähert werden soll.';
  End;
  If s = 'Smoothshadow' Then Begin
    memo1.lines.text := 'Smoothshadow'#13#13 +
      'x = Single >= 0'#13#13 +
      'ist diese Option eingeschaltet dann wird der Schatten der duch das'#13 +
      'Primitive entsteht je nach SmoothshadowMode übergeblendet.'#13 +
      'X = 0 = aus'#13 +
      'X > 0 = eingeschaltet. Smoothshadow wird auf den Durchmesser des Primitives'#13 +
      'scaliert X = 1 = Primitivedurchmesser';
  End;
  If s = 'Smoothshadowmode' Then Begin
    memo1.lines.text := 'Smoothshadowmode'#13#13 +
      'x = linear, Cos'#13#13 +
      'ist der Smoothshadow aktiviert dann kann hier ausgewählt werden ob'#13 +
      'der Schatten Liniar oder über die Cosinusfunction angenähert werden soll.';
  End;
  If s = 'Textureparameter' Then Begin
    memo1.lines.text := 'Textureparameter'#13#13 +
      'x = Clamp, Repeat'#13#13 +
      'Die Textureparameter geben an was geschehen soll wenn die U, V parameter aus dem Intervall [0 .. 1] herauslaufen ' +
      'Bei Clamp wird der Wert auf die entsprechende Grenze zurückgesetzt ( > 1 => 1, < 0 => 0 ). '#13#13 +
      'Bei Repeat wird jeweils nur mit dem Nachkommateil gerechnet, dadurch kann eine Texture mehrfach nebeneinander auf ' +
      'ein Primitive gelegt werden.';
  End;
  If s = 'Textureinterpolation' Then Begin
    memo1.lines.text := 'Textureinterpolation'#13#13 +
      'x = Nearest, Biliniar, Cos, Triliniar'#13#13 +
      'Mit Textureinterpolation wird festgelegt wie der TextureLookup geschieht'#13 +
      'Nearest = Der Pixel der den Texturkoordinaten am nächsten ist wird gewählt ( am schnellsten )'#13 +
      'Biliniar = OpenGL Standard der Farbwert wird anhand der 4 umliegenden Pixel Interpoloert'#13 +
      'Cos = Ähnlich wie Biliniar werden hier anhand der Cosinusfunction die 4 umliegenden Pixel Interpoliert'#13 +
      'Triliniar = Interpolation unter zuhilfenahme einer Mippmapingstufe'#13#13 +
      'Triliniar wird zur Zeit noch nicht unterstützt.';
  End;

  (*

      Treeview1.items.AddChild(n, 'Reflectance');
      Treeview1.items.AddChild(n, 'Transparence');
      Treeview1.items.AddChild(n, 'Ambientcolor');
      Treeview1.items.AddChild(n, 'Emissioncolor');
      Treeview1.items.AddChild(n, 'Diffusecolor');
      Treeview1.items.AddChild(n, 'Specularcolor');
      Treeview1.items.AddChild(n, 'Highlightexponent');
      Treeview1.items.AddChild(n, 'Texturecoordinates');
      n := Treeview1.items.AddChild(Nil, 'Triangle');
      Treeview1.items.AddChild(n, 'Points');
      Treeview1.items.AddChild(n, 'Rotate');
      Treeview1.items.AddChild(n, 'Translate');
      n := Treeview1.items.AddChild(Nil, 'Quad');
      Treeview1.items.AddChild(n, 'Points');
      Treeview1.items.AddChild(n, 'Rotate');
      Treeview1.items.AddChild(n, 'Translate');
      n := Treeview1.items.AddChild(Nil, 'Sphere');
      Treeview1.items.AddChild(n, 'Radius');
      Treeview1.items.AddChild(n, 'Rotate');
      Treeview1.items.AddChild(n, 'Translate');
      n := Treeview1.items.AddChild(Nil, 'External');  Unterstützt als einziges das einlesen im TAG !!!!
      Treeview1.items.AddChild(n, 'Filename');
      Treeview1.items.AddChild(n, 'Rotate');
      Treeview1.items.AddChild(n, 'Translate');
    *)

End;

Procedure TForm4.FormCreate(Sender: TObject);
Var
  n: TTreeNode;
Begin
  Treeview1.items.clear;
  n := Treeview1.items.AddChild(Nil, 'Allgemein');
  n := Treeview1.items.AddChild(Nil, 'Raytracer');
  Treeview1.items.AddChild(n, 'Imagewidth');
  Treeview1.items.AddChild(n, 'Imageheight');
  Treeview1.items.AddChild(n, 'Mindistance');
  Treeview1.items.AddChild(n, 'Maxdistance');
  Treeview1.items.AddChild(n, 'Lightoverflowmode');
  Treeview1.items.AddChild(n, 'Orthoprojection');
  Treeview1.items.AddChild(n, 'Ortho');
  Treeview1.items.AddChild(n, 'Cullfaceing');
  Treeview1.items.AddChild(n, 'Cullfacemode');
  Treeview1.items.AddChild(n, 'Backgroundcolor');
  Treeview1.items.AddChild(n, 'Rekursiondepth');
  Treeview1.items.AddChild(n, 'Eye');
  Treeview1.items.AddChild(n, 'Lookat');
  Treeview1.items.AddChild(n, 'Fog');
  Treeview1.items.AddChild(n, 'FogMin');
  Treeview1.items.AddChild(n, 'FogMax');
  Treeview1.items.AddChild(n, 'FogColor');
  Treeview1.items.AddChild(n, 'FogMode');
  Treeview1.items.AddChild(n, 'Variables');
  n := Treeview1.items.AddChild(Nil, 'Light');
  Treeview1.items.AddChild(n, 'Position');
  Treeview1.items.AddChild(n, 'Active');
  Treeview1.items.AddChild(n, 'Ambientcolor');
  Treeview1.items.AddChild(n, 'Diffusecolor');
  Treeview1.items.AddChild(n, 'Specularcolor');
  n := Treeview1.items.AddChild(Nil, 'Material');
  Treeview1.items.AddChild(n, 'Texture');
  Treeview1.items.AddChild(n, 'Transparencetexture');
  Treeview1.items.AddChild(n, 'Shadowborder');
  Treeview1.items.AddChild(n, 'Shadowbordermode');
  Treeview1.items.AddChild(n, 'Smoothshadow');
  Treeview1.items.AddChild(n, 'Smoothshadowmode');
  Treeview1.items.AddChild(n, 'Textureparameter');
  Treeview1.items.AddChild(n, 'Textureinterpolation');
  Treeview1.items.AddChild(n, 'Reflectance');
  Treeview1.items.AddChild(n, 'Transparence');
  Treeview1.items.AddChild(n, 'Ambientcolor');
  Treeview1.items.AddChild(n, 'Emissioncolor');
  Treeview1.items.AddChild(n, 'Diffusecolor');
  Treeview1.items.AddChild(n, 'Specularcolor');
  Treeview1.items.AddChild(n, 'Highlightexponent');
  Treeview1.items.AddChild(n, 'Texturecoordinates');
  n := Treeview1.items.AddChild(Nil, 'Triangle');
  Treeview1.items.AddChild(n, 'Points');
  Treeview1.items.AddChild(n, 'Rotate');
  Treeview1.items.AddChild(n, 'Translate');
  n := Treeview1.items.AddChild(Nil, 'Quad');
  Treeview1.items.AddChild(n, 'Points');
  Treeview1.items.AddChild(n, 'Rotate');
  Treeview1.items.AddChild(n, 'Translate');
  n := Treeview1.items.AddChild(Nil, 'Sphere');
  Treeview1.items.AddChild(n, 'Position');
  Treeview1.items.AddChild(n, 'Radius');
  Treeview1.items.AddChild(n, 'Rotate');
  Treeview1.items.AddChild(n, 'Translate');
  n := Treeview1.items.AddChild(Nil, 'External');
  Treeview1.items.AddChild(n, 'Filename');
  Treeview1.items.AddChild(n, 'Rotate');
  Treeview1.items.AddChild(n, 'Translate');
End;

Procedure TForm4.TreeView1KeyPress(Sender: TObject; Var Key: Char);
Begin
  TreeView1Click(Nil);
End;

Procedure TForm4.Memo1KeyPress(Sender: TObject; Var Key: Char);
Begin
  If key = #27 Then Close;
End;

Procedure TForm4.Button1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = 27 Then close;
End;

End.

