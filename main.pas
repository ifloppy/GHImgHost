unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, IniFiles, settings, Clipbrd, LCLIntf, LCLType, ActnList, base64,
  fphttpclient, common;

type

  { TFormMain }

  TFormMain = class(TForm)
    btnCopy2: TButton;
    btnSettings: TButton;
    btnFromClipboard: TButton;
    btnUpload: TButton;
    btnCopy1: TButton;
    btnClear: TButton;
    Image1: TImage;
    StatusBar1: TStatusBar;

    procedure btnClearClick(Sender: TObject);
    procedure btnCopy2Click(Sender: TObject);
    procedure btnFromClipboardClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnUploadClick(Sender: TObject);
    procedure btnCopy1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public
    isUploading: Boolean;
    UploadedFileName: string;
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
    MessageDlg('信息错误', 'Github信息存在漏填，请打开设置界面进行填写并保存', mtError, [mbOK], 0);
    Exit;
  end;

  isUploading:=true;

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
  Client.AddHeader('Accept', 'application/vnd.github+json');
  Client.AddHeader('user-agent', User_Agent);
  Client.AddHeader('authorization', 'Bearer '+Config.ReadString('Github', 'Token', ''));

  UploadData:=TStringStream.Create(Format('{"message":"Uploaded by GHImgHost","committer":{"name":"%s","email":"%s"},"content":"%s"}', [
  Config.ReadString('Github', 'User', ''),
  Config.ReadString('Github', 'Email', ''),
  Base64OutputStream.AnsiDataString
  ]));
  Base64OutputStream.Free;

  Client.RequestBody:=UploadData;

  filename:=GeneratePNGFileName() ;
  url:=Format(BaseURL+'/repos/%s/%s/contents/%s', [
  Config.ReadString('Github', 'User', ''),
  Config.ReadString('Github', 'Repo', ''),
  filename
  ]);

  StatusBar1.Panels[0].Text:='正在上传图片';
  try
    resp:=Client.Put(url);
  finally
  end;

  if Client.ResponseStatusCode = 201 then begin
    StatusBar1.Panels[0].Text:='图片上传成功';
    //imgToCopy:=Format(Config.ReadString('Formatter', 'Copy', ''), [filename]);
    UploadedFileName:=filename;
  end else begin
    StatusBar1.Panels[0].Text:='图片上传失败：'+Client.ResponseStatusText;
    if MessageDlg('Upload failed', '上传失败，是否要展示服务器返回的内容？', mtWarning, mbYesNo, 0) = mrYes then
    ShowMessage(resp);
  end;


  UploadData.Free;
  Client.Free;
  isUploading:=false;
end;

procedure TFormMain.btnCopy1Click(Sender: TObject);
begin
  if isUploading then begin
    btnFromClipboard.Enabled:=false;
    Exit;
  end;
  if UploadedFileName.IsEmpty then begin
    StatusBar1.Panels[0].Text:='你还没有上传图片!';
    exit;
  end;
  Clipboard.AsText:=Format(Config.ReadString('Formatter', 'Copy', '%s'), [UploadedFileName]);
  StatusBar1.Panels[0].Text:='已复制格式化后的图片名称（1）';
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

procedure TFormMain.btnClearClick(Sender: TObject);
begin
  Image1.Picture.Clear;
  UploadedFileName:='';
end;

procedure TFormMain.btnCopy2Click(Sender: TObject);
begin
  if isUploading then begin
    btnFromClipboard.Enabled:=false;
    Exit;
  end;
  if UploadedFileName.IsEmpty then begin
    StatusBar1.Panels[0].Text:='你还没有上传图片!';
    exit;
  end;
  Clipboard.AsText:=Format(Config.ReadString('Formatter', 'Copy2', '%s'), [UploadedFileName]);
  StatusBar1.Panels[0].Text:='已复制格式化后的图片名称（2）';
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

  if Length(FileNames) > 0 then begin
    FileName:=FileNames[0];
    Image1.Picture.LoadFromFile(FileName);

  end;



  //Document: https://wiki.lazarus.freepascal.org/Drag_and_Drop_sample#Files
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if (Key = LCLType.VK_V) and (ssCtrl in Shift) then
      btnFromClipboardClick(self);
end;






end.
