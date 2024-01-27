unit settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fphttpclient, Common, LCLIntf, ComCtrls;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    btnGetToken: TButton;
    btnOK: TButton;
    btnTest: TButton;
    inputRepoName: TLabeledEdit;
    inputToken: TLabeledEdit;
    inputUserName: TLabeledEdit;
    inputCopyFormat: TLabeledEdit;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnGetTokenClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  Client.AddHeader('Authorization', 'Bearer '+inputToken.Text);
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

  Client.Free;
end;

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  inputUserName.Text:=Config.ReadString('Github', 'User', '');
  inputRepoName.Text:=Config.ReadString('Github', 'Repo', '');
  inputToken.Text:=Config.ReadString('Github', 'Token', '');
  inputCopyFormat.Text:=Config.ReadString('Copy', 'Formatter', '%s');
end;

procedure TFormSettings.btnGetTokenClick(Sender: TObject);
begin
  MessageDlg('提示', '在创建Token时，需要启用repo类的权限', mtInformation, [mbOK], 0);
  OpenURL('https://github.com/settings/tokens');
end;

procedure TFormSettings.btnOKClick(Sender: TObject);
begin
  Config.WriteString('Github', 'User', inputUserName.Text);
  Config.WriteString('Github', 'Repo', inputRepoName.Text);
  Config.WriteString('Github', 'Token', inputToken.Text);

  if Pos('%s', inputCopyFormat.Text) = 0 then begin
    MessageDlg('无效参数', '你需要在复制格式中填写含有%s的文本', mtWarning, [mbOK], 0);
    Exit;
  end;
  Config.WriteString('Copy', 'Formatter', inputCopyFormat.Text);

  Close;
end;

end.

