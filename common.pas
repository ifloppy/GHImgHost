unit Common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

const
  BaseURL = 'https://api.github.com';
  User_Agent = 'Image Client DEV';

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
