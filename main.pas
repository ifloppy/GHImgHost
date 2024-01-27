unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, IniFiles, settings;

type

  { TFormMain }

  TFormMain = class(TForm)
    btnSettings: TButton;
    btnFromClipboard: TButton;
    btnUpload: TButton;
    Button1: TButton;
    Image1: TImage;
    StatusBar1: TStatusBar;

    procedure btnFromClipboardClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnUploadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  private

  public
    isUploading: Boolean;
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

end;

procedure TFormMain.btnSettingsClick(Sender: TObject);
begin
  ShowSettingsForm();
end;

procedure TFormMain.btnUploadClick(Sender: TObject);
begin
  if isUploading then begin
    btnUpload.Enabled:=false;
    Exit;
  end;

  if (
  Config.ReadString('Github', 'User', '').IsEmpty or
  Config.ReadString('Github', 'Repo', '').IsEmpty or
  Config.ReadString('Github', 'Token', '').IsEmpty
  ) then begin
    MessageDlg('信息错误', 'Github信息存在漏填', mtError, [mbOK], 0);
    Exit;
  end;
end;

procedure TFormMain.btnFromClipboardClick(Sender: TObject);
begin
  //Document: https://wiki.lazarus.freepascal.org/Clipboard
  if isUploading then begin
    btnFromClipboard.Enabled:=false;
    Exit;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Config.Free;
end;

procedure TFormMain.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  FileName: string;
begin
  if isUploading then begin
    Exit;
  end;

  if Length(FileNames) > 0 then FileName:=FileNames[0];

  Image1.Picture.LoadFromFile(FileName);
  //TODO: upload image file
  //Document: https://wiki.lazarus.freepascal.org/Drag_and_Drop_sample#Files
end;





end.
