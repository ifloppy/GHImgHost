unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IniFiles, settings;

type

  { TFormMain }

  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;
  Config: TIniFile;

implementation

{$R *.lfm}

{ TFormMain }

procedure ShowSettingsForm();
var
  SettingsForm: TFormSettings;
begin
  SettingsForm := TFormSettings.Create(nil);
  SettingsForm.ShowModal;
  SettingsForm.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Config := TIniFile.Create('config.ini');
  if Config.ReadString('github', 'token', '') = '' then
  begin
    ShowSettingsForm();
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Config.Free;
end;




end.
