unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, IniFiles, settings, Clipbrd, LCLIntf, LCLType, base64, fphttpclient, common;

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
var
  outputPNG: TPortableNetworkGraphic;
  Encoder: TBase64EncodingStream;
  Base64OutputStream: TStringStream;
  PNGStream: TStream;
  Client: TFPHTTPClient;
  UploadData: TStringStream;
  resp, url, filename: string;
begin
  if isUploading then begin
    btnUpload.Enabled:=false;
    Exit;
  end;

  if (
  Config.ReadString('Github', 'User', '').IsEmpty or
  Config.ReadString('Github', 'Repo', '').IsEmpty or
  Config.ReadString('Github', 'Token', '').IsEmpty or
  Config.ReadString('Github', 'Email', '').isEmpty
  ) then begin
    MessageDlg('信息错误', 'Github信息存在漏填', mtError, [mbOK], 0);
    Exit;
  end;

  StatusBar1.Panels[0].Text:='等待转码图片';

  //Prepare PNG
  outputPNG:=Image1.Picture.PNG;
  Base64OutputStream:=TStringStream.Create;
  Encoder:=TBase64EncodingStream.Create(Base64OutputStream);
  PNGStream:=TMemoryStream.Create;
  outputPNG.SaveToStream(PNGStream);
  PNGStream.Position:=0;
  Encoder.CopyFrom(PNGStream, PNGStream.Size);
  Encoder.Flush;


  PNGStream.Free;
  Encoder.Free;

  StatusBar1.Panels[0].Text:='等待上传图片';
  //Upload
  Client:=TFPHTTPClient.Create(nil);
  Client.AddHeader('Accept', 'application/json');
  Client.AddHeader('user-agent', User_Agent);
  Client.AddHeader('authorization', 'Bearer '+Config.ReadString('Github', 'Token', ''));

  UploadData:=TStringStream(Format('{"message":"Uploaded by GHImgHost","committer":{"name":"%s","email":"%s"},"content":"%s"}', [
  Config.ReadString('Github', 'User', ''),
  Config.ReadString('Github', 'Email', ''),
  Base64OutputStream.AnsiDataString
  ]));
  Base64OutputStream.Free;

  Client.RequestBody:=UploadData;

  filename:=GeneratePNGFileName() ;
  url:=Format(BaseURL+'/repos/%s/%s/contents/%s', [
  Config.ReadString('Github', 'User', ''),
  Config.ReadString('Github', 'Repo', '')
  filename
  ]);

  StatusBar1.Panels[0].Text:='正在上传图片';
  try
    resp:=Client.Put(url);
  finally
  end;

  if Client.ResponseStatusCode = 201 then begin
    StatusBar1.Panels[0].Text:='图片上传成功';
  end else begin
    StatusBar1.Panels[0].Text:='图片上传失败：'+Client.ResponseStatusText;
  end;


  UploadData.Free;
  Client.Free;
end;

procedure TFormMain.btnFromClipboardClick(Sender: TObject);
begin
  //Document: https://wiki.lazarus.freepascal.org/Clipboard
  if isUploading then begin
    btnFromClipboard.Enabled:=false;
    Exit;
  end;

  if Clipboard.HasFormat(CF_Picture) then begin
    Image1.Picture.LoadFromClipboardFormat(CF_Bitmap);
    exit;
  end;

  StatusBar1.Panels[0].Text:='剪切板中没有找到图片';
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
