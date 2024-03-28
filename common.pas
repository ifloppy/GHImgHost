unit Common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

const
  BaseURL = 'https://api.github.com';
  User_Agent = 'Image Client DEV';

resourcestring
  INFO_ERR = '信息错误';
  INFO_ERR_DETAIL = 'Github信息存在漏填，请打开设置界面进行填写并保存';

  NO_CHOSEN_PIC = '你还没有选择图片！';
  WAITING_ENCODING_PIC = '等待转码图片';
  WAITING_UPLOADING_PIC = '等待上传图片';
  UPLOADING_PIC = '正在上传图片';
  SUCCESS_UPLOADING_PIC = '图片上传成功';
  FAILED_UPLOADING_PIC = '图片上传失败：';
  FAILED_UPLOADING_IF_DISPLAY_DETAIL = '上传失败，是否要展示服务器返回的内容？';
  NO_UPLOADED_PIC = '你还没有上传图片!';

  COPIED_FORMATTED_PIC = '已复制格式化后的图片名称';

function GeneratePNGFileName(): string;

implementation

uses main;

function GUIDToStringModified(const GUID: TGUID): string;
begin
  SetLength(Result, 36);
  StrLFmt(PChar(Result), 36,'%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',
    [
     Longint(GUID.D1), GUID.D2, GUID.D3,
     GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
     GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]
    ]);
end;

function GeneratePNGFileName(Prefix: string; IdentType: integer): string;
var
  dt: TDateTime;
  ts: TTimeStamp;
  guid: TGuid;
begin
  {
  yyyymmddhhmmss
  当前的毫秒时间戳
  随机生成UUID
  }
  case IdentType of
    0: begin
      dt := Now;
      Result := Prefix + FormatDateTime('yyyymmddhhnnss', dt) + '.png';
    end;
    1: begin
      ts := DateTimeToTimeStamp(Now);
      Result := Prefix + FloatToStr(TimeStampToMSecs(ts)) + '.png';
    end;
    2: begin
      CreateGUID(guid);
      Result := Prefix + GUIDToStringModified(guid) + '.png';
    end;
  end;
end;

function GeneratePNGFileName(): string;
begin
  Result := GeneratePNGFileName(Config.ReadString('Formatter', 'FileNamePrefix', ''),
    Config.ReadInteger('Formatter', 'IndentType', 0));
end;

end.
