unit settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fphttpclient, Common, LCLIntf, ComCtrls, ActnList;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    ActionList1: TActionList;
    btnGetToken: TButton;
    btnOK: TButton;
    btnTest: TButton;
    btnCancel: TButton;
    inputCopyFormat2: TLabeledEdit;
    selectCopyFormatterTemplate: TComboBox;
    inputEmail: TLabeledEdit;
    selectIdentStr: TComboBox;
    inputRepoName: TLabeledEdit;
    inputToken: TLabeledEdit;
    inputUserName: TLabeledEdit;
    inputCopyFormat1: TLabeledEdit;
    inputFIleNamePrefix: TLabeledEdit;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnCancelClick(Sender: TObject);
    procedure btnGetTokenClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure selectCopyFormatterTemplateChange(Sender: TObject);
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
  resp: String;
begin
  Client:=TFPHTTPClient.Create(nil);
  Client.AddHeader('Authorization', 'Bearer '+inputToken.Text);
  Client.AddHeader('user-agent', User_Agent);
  Client.AddHeader('accept', 'application/json');
  try
    resp:=Client.Get(BaseURL+'/repos/'+inputUserName.Text+'/'+inputRepoName.Text);
  finally
  end;

  if Client.ResponseStatusCode = 200 then begin
    MessageDlg('发送成功', '该配置能够与Github的API进行通讯', mtInformation, [mbOK], 0);
  end else
  begin
    MessageDlg('发送失败', '无法与Github的API进行通信，请尝试修改配置，错误代码：'+Client.ResponseStatusText, mtError, [mbOK], 0);
    ShowMessage(resp);
  end;

  Client.Free;
end;

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  inputUserName.Text:=Config.ReadString('Github', 'User', '');
  inputRepoName.Text:=Config.ReadString('Github', 'Repo', '');
  inputToken.Text:=Config.ReadString('Github', 'Token', '');
  inputEmail.Text:=Config.ReadString('Github', 'Email', '');
  inputCopyFormat1.Text:=Config.ReadString('Formatter', 'Copy', '%s');
  inputCopyFormat2.Text:=Config.ReadString('Formatter', 'Copy2', '%s');
  inputFIleNamePrefix.Text:=Config.ReadString('Formatter', 'FileNamePrefix', '');
  selectIdentStr.ItemIndex:=Config.ReadInteger('Formatter', 'IndentType', 0);
end;


procedure TFormSettings.selectCopyFormatterTemplateChange(Sender: TObject);
begin
  case selectCopyFormatterTemplate.ItemIndex of
    0: inputCopyFormat1.Text:='https://raw.githubusercontent.com/'+Config.ReadString('Github', 'User', '%USER%')+'/'+Config.ReadString('Github', 'Repo', '%REPO%')+'/main/%s';
    1: inputCopyFormat1.Text:='https://cdn.jsdelivr.net/gh/'+Config.ReadString('Github', 'User', '%USER%')+'/'+Config.ReadString('Github', 'Repo', '%REPO%')+'/%s';
  end;
  selectCopyFormatterTemplate.ItemIndex:=-1;
  selectCopyFormatterTemplate.Text:='可以在此选择复制格式模板（1）';
end;

procedure TFormSettings.btnGetTokenClick(Sender: TObject);
begin
  MessageDlg('提示', '在创建Token时，需要启用repo类的权限', mtInformation, [mbOK], 0);
  OpenURL('https://github.com/settings/tokens');
end;

procedure TFormSettings.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormSettings.btnOKClick(Sender: TObject);
begin
  Config.WriteString('Github', 'User', inputUserName.Text);
  Config.WriteString('Github', 'Repo', inputRepoName.Text);
  Config.WriteString('Github', 'Token', inputToken.Text);
  Config.WriteString('Github', 'Email', inputEmail.Text);

  if Pos('%s', inputCopyFormat1.Text) = 0 then begin
    MessageDlg('无效参数', '你需要在复制格式中填写含有%s的文本', mtWarning, [mbOK], 0);
    Exit;
  end;
  Config.WriteString('Formatter', 'Copy', inputCopyFormat1.Text);
  Config.WriteString('Formatter', 'Copy2', inputCopyFormat2.Text);

  Config.WriteString('Formatter', 'FileNamePrefix', inputFIleNamePrefix.Text);
  Config.WriteInteger('Formatter', 'IndentType', selectIdentStr.ItemIndex);

  Close;
end;

end.

