unit dUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8, LazFileUtils;

type
  TProxyType = (ptHTTP, ptHTTPS);

type
  TdmUtils = class(TDataModule)
  private
    function GetProxyEnvValue(const ProxyType: TProxyType): string;
  public
    function GetAppConfigFileName: string;

    procedure GetProxyParams(const ProxyType: TProxyType; var ProxyHost: string; var ProxyPort: integer; var ProxyUser, ProxyPass: string);
  end;

var
  dmUtils: TdmUtils;

implementation

{$R *.lfm}

function TdmUtils.GetAppConfigFileName: string;
begin
  {$IFDEF DARWIN}
  Result := ExtractFilePath(ExpandFileName('~/Library/Preferences/cqrprop')) + 'cqrprop' + DirectorySeparator + 'cqrprop.cfg';
  {$ELSE}
  Result := ExtractFilePath(GetAppConfigFile(False)) + 'cqrprop' + DirectorySeparator + 'cqrprop.cfg';
  {$ENDIF}
end;

function TdmUtils.GetProxyEnvValue(const ProxyType: TProxyType): string;
begin
  Result := '';
  case ProxyType of
    ptHTTP: Result := GetEnvironmentVariableUTF8('http_proxy');
    ptHTTPS: Result := GetEnvironmentVariableUTF8('https_proxy')
  end;
end;

procedure TdmUtils.GetProxyParams(const ProxyType: TProxyType; var ProxyHost: string; var ProxyPort: integer; var ProxyUser, ProxyPass: string);
var
  ProxyValue: string = '';
  HasAuth: boolean = False;
  ServerPort: string = '';
  Port: string = '';
  UserPass: string = '';
begin
  ProxyHost := '';
  ProxyPort := 0;
  ProxyUser := '';
  ProxyPass := '';

  ProxyValue := GetProxyEnvValue(ProxyType);

  if (ProxyValue = '') then
    exit; //no proxy settings, exiting

  HasAuth := Pos('@', ProxyValue) > 0;

  if (ProxyValue[Length(ProxyValue)] = '/') then
    ProxyValue := copy(ProxyValue, 1, Length(ProxyValue) - 1); //remove last "/"

  if HasAuth then
  begin
    ServerPort := copy(ProxyValue, Pos('@', ProxyValue) + 1, Length(ProxyValue) - Pos('@', ProxyValue));  //USERNAME:PASSWORD@SERVER:8080
    Port := copy(ServerPort, Pos(':', ServerPort) + 1, Length(ServerPort) - Pos(':', ServerPort));
    if not TryStrToInt(Port, ProxyPort) then
    begin
      Writeln('Cannot read the proxy port property!');
      Writeln(ProxyValue);
      exit;
    end;

    UserPass := copy(ProxyValue, Pos('://', ProxyValue) + 3, Pos('@', ProxyValue) - Pos('://', ProxyValue) - 3);  //USERNAME:PASSWORD

    ProxyHost := copy(ServerPort, 1, Pos(':', ServerPort) - 1);
    ProxyUser := copy(UserPass, 1, Pos(':', UserPass) - 1);
    ProxyPass := copy(UserPass, Pos(':', UserPass) + 1, Length(UserPass));
  end
  else begin
    ServerPort := copy(ProxyValue, Pos('://', ProxyValue) + 3, Length(ProxyValue) - Pos('://', ProxyValue) - 2);  //SERVER:8080
    Port := copy(ServerPort, Pos(':', ServerPort) + 1, Length(ServerPort) - Pos(':', ServerPort));
    if not TryStrToInt(Port, ProxyPort) then
    begin
      Writeln('Cannot read the proxy port property!');
      Writeln(ProxyValue);
      exit;
    end;
    ProxyHost := copy(ServerPort, 1, Pos(':', ServerPort) - 1);
  end;
end;

end.


