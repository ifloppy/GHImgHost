unit settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, fphttpclient, Common;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    btnGetToken: TButton;
    btnTest: TButton;
    btnOK: TButton;
    inputUserName: TLabeledEdit;
    inputRepoName: TLabeledEdit;
    inputToken: TLabeledEdit;
    procedure btnTestClick(Sender: TObject);
  private

  public

  end;

var
  FormSettings: TFormSettings;

implementation

uses main;

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.btnTestClick(Sender: TObject);
var
  Client: TFPHTTPClient;
begin
  Client:=TFPHTTPClient.Create(nil);
  try
    Client.Get(BaseURL+'/repos/'+inputUserName.Text+'/'+inputRepoName.Text);
  finally
  end;

  if Client.ResponseStatusCode = 200 then begin
    MessageDlg('发送成功', '该配置能够与Github的API进行通讯', mtInformation, [mbOK], 0);
  end else
  begin
    MessageDlg('发送失败', '无法与Github的API进行通信，请尝试修改配置，错误代码：'+Client.ResponseStatusText, mtError, [mbOK], 0);
  end;

end;

end.

