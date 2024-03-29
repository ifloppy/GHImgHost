unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, IniFiles, settings, Clipbrd, LCLIntf, LCLType, ActnList, ExtDlgs,
  base64, fphttpclient, common;

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
    OpenPictureDialog1: TOpenPictureDialog;
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
    procedure Image1Click(Sender: TObject);
  private

  public
    isUploading, isSelectedFile: Boolean;
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
  resp, url, filename, processed: string;
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
    MessageDlg(INFO_ERR, INFO_ERR_DETAIL, mtError, [mbOK], 0);
    Exit;
  end;

  if not isSelectedFile then begin
    StatusBar1.Panels[0].Text:=NO_CHOSEN_PIC;
    exit;
  end;

  isUploading:=true;

  StatusBar1.Panels[0].Text:=WAITING_ENCODING_PIC;

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

  StatusBar1.Panels[0].Text:=WAITING_UPLOADING_PIC;
  //Upload
  Client:=TFPHTTPClient.Create(nil);
  Client.AddHeader('Accept', 'application/vnd.github+json');
  Client.AddHeader('user-agent', User_Agent);
  Client.AddHeader('authorization', 'Bearer '+Config.ReadString('Github', 'Token', ''));

  processed := TStringStream(Base64OutputStream).DataString;
  UploadData:=TStringStream.Create(Format('{"message":"Uploaded by GHImgHost","committer":{"name":"%s","email":"%s"},"content":"%s"}', [
  Config.ReadString('Github', 'User', ''),
  Config.ReadString('Github', 'Email', ''),
  processed
  ]));
  Base64OutputStream.Free;

  Client.RequestBody:=UploadData;

  filename:=GeneratePNGFileName() ;
  url:=Format(BaseURL+'/repos/%s/%s/contents/%s', [
  Config.ReadString('Github', 'User', ''),
  Config.ReadString('Github', 'Repo', ''),
  filename
  ]);

  StatusBar1.Panels[0].Text:=UPLOADING_PIC;
  try
    resp:=Client.Put(url);
  finally
  end;

  if Client.ResponseStatusCode = 201 then begin
    StatusBar1.Panels[0].Text:=SUCCESS_UPLOADING_PIC;
    //imgToCopy:=Format(Config.ReadString('Formatter', 'Copy', ''), [filename]);
    UploadedFileName:=filename;
  end else begin
    StatusBar1.Panels[0].Text:=FAILED_UPLOADING_PIC+Client.ResponseStatusText;
    if MessageDlg('Upload failed', FAILED_UPLOADING_IF_DISPLAY_DETAIL, mtWarning, mbYesNo, 0) = mrYes then
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
    StatusBar1.Panels[0].Text:=NO_UPLOADED_PIC;
    exit;
  end;
  Clipboard.AsText:=Format(Config.ReadString('Formatter', 'Copy', '%s'), [UploadedFileName]);
  StatusBar1.Panels[0].Text:=COPIED_FORMATTED_PIC+'（1）';
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
    isSelectedFile:=true;
    exit;
  end;

  StatusBar1.Panels[0].Text:=NO_UPLOADED_PIC;
end;

procedure TFormMain.btnClearClick(Sender: TObject);
begin
  Image1.Picture.Clear;
  UploadedFileName:='';
  isSelectedFile:=false;
end;

procedure TFormMain.btnCopy2Click(Sender: TObject);
begin
  if isUploading then begin
    btnFromClipboard.Enabled:=false;
    Exit;
  end;
  if UploadedFileName.IsEmpty then begin
    StatusBar1.Panels[0].Text:=NO_UPLOADED_PIC;
    exit;
  end;
  Clipboard.AsText:=Format(Config.ReadString('Formatter', 'Copy2', '%s'), [UploadedFileName]);
  StatusBar1.Panels[0].Text:=COPIED_FORMATTED_PIC+'（2）';
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

procedure TFormMain.Image1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then begin Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    isSelectedFile:=true;
  end;
end;






end.
