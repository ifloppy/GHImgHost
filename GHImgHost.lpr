program GHImgHost;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, settings, Common, opensslsockets, openssl
  { you can add units after this }
  {$IfDef WINDOWS}, uMetaDarkStyle, uDarkStyleParams, uDarkStyleSchemes{$EndIf};

{$R *.res}

begin
  {$IfDef WINDOWS}
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  PreferredAppMode:=pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$EndIf}
  Application.Initialize;
  InitSSLInterface;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.Run;
end.

