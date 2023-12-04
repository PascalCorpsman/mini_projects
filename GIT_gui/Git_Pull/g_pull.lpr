Program g_pull;

{$MODE objfpc}{$H+}

Uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
{$IFDEF HASAMIGA}
  athreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, uGITOptions, ugitprogress
  { you can add units after this };

{$R *.res}

Begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TGitOptions, GitOptions);
  Application.CreateForm(TGitProgress, GitProgress);
  Application.Run;
End.

