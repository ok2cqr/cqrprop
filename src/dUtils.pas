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
    function  GetProxyEnvValue(const ProxyType : TProxyType) : String;
    function  FindLib(const Path,LibName : String) : String;
  public
    function  GetAppConfigFileName : String;
    function  GetSSLLib(LibName : String) : String;

    procedure GetProxyParams(const ProxyType : TProxyType; var ProxyHost : String; var ProxyPort : Integer; var ProxyUser, ProxyPass : String);
  end;

var
  dmUtils : TdmUtils;

implementation

{$R *.lfm}

function TdmUtils.GetAppConfigFileName : String;
begin
  Result := ExtractFilePath(GetAppConfigFile(False))+'cqrprop'+DirectorySeparator+'cqrprop.cfg'
end;

function TdmUtils.GetProxyEnvValue(const ProxyType : TProxyType) : String;
begin
  Result := '';
  case ProxyType of
    ptHTTP  : Result := GetEnvironmentVariableUTF8('http_proxy');
    ptHTTPS : Result := GetEnvironmentVariableUTF8('https_proxy')
  end
end;

procedure TdmUtils.GetProxyParams(const ProxyType : TProxyType; var ProxyHost : String; var ProxyPort : Integer; var ProxyUser, ProxyPass : String);
var
  ProxyValue : String = '';
  HasAuth    : Boolean = False;
  ServerPort : String = '';
  Port       : String = '';
  UserPass   : String = '';
begin
  ProxyHost := '';
  ProxyPort := 0;
  ProxyUser := '';
  ProxyPass := '';

  ProxyValue := GetProxyEnvValue(ProxyType);

  if (ProxyValue = '') then
    exit; //no proxy settings, exiting

  HasAuth := Pos('@',ProxyValue) > 0;

  if (ProxyValue[Length(ProxyValue)] = '/') then
    ProxyValue := copy(ProxyValue, 1, Length(ProxyValue)-1); //remove last "/"

  if HasAuth then
  begin
    ServerPort := copy(ProxyValue, Pos('@',ProxyValue)+1, Length(ProxyValue)-Pos('@',ProxyValue));  //USERNAME:PASSWORD@SERVER:8080
    Port       := copy(ServerPort, Pos(':',ServerPort)+1, Length(ServerPort)-Pos(':',ServerPort));
    if not TryStrToInt(Port, ProxyPort) then
    begin
      Writeln('Cannot read the proxy port property!');
      Writeln(ProxyValue);
      exit
    end;

    UserPass  := copy(ProxyValue, Pos('://', ProxyValue)+3, Pos('@', ProxyValue)-Pos('://', ProxyValue)-3);  //USERNAME:PASSWORD

    ProxyHost := copy(ServerPort, 1, Pos(':',ServerPort)-1);
    ProxyUser := copy(UserPass, 1, Pos(':', UserPass)-1);
    ProxyPass := copy(UserPass, Pos(':', UserPass)+1, Length(UserPass))
  end
  else begin
    ServerPort := copy(ProxyValue, Pos('://', ProxyValue)+3, Length(ProxyValue)-Pos('://', ProxyValue)-2);  //SERVER:8080
    Port := copy(ServerPort, Pos(':',ServerPort)+1, Length(ServerPort)-Pos(':',ServerPort));
    if not TryStrToInt(Port, ProxyPort) then
    begin
      Writeln('Cannot read the proxy port property!');
      Writeln(ProxyValue);
      exit
    end;
    ProxyHost := copy(ServerPort, 1, Pos(':', ServerPort)-1)
  end
end;

function TdmUtils.FindLib(const Path,LibName : String) : String;
var
  res       : Byte;
  SearchRec : TSearchRec;
begin
  Result := '';
  res := FindFirst(Path + LibName, faAnyFile, SearchRec);
  try
    while Res = 0 do
    begin
      if FileExistsUTF8(Path + SearchRec.Name) then
      begin
        Result := (Path + SearchRec.Name);
        Break
      end;
      Res := FindNext(SearchRec)
    end
  finally
    FindClose(SearchRec)
  end
end;


function TdmUtils.GetSSLLib(LibName : String) : String;
var
  lib : String;
begin
  lib :=  FindLib('/usr/lib64/',LibName+'.so*');
  if (lib = '') then
    lib := FindLib('/lib64/',LibName+'.so*');
  if (lib='') then
    lib := FindLib('/usr/lib/x86_64-linux-gnu/',LibName+'.so*');
  if (lib='') then
    lib := FindLib('/usr/lib/i386-linux-gnu/',LibName+'.so*');
  if (lib = '') then
    lib :=  FindLib('/usr/lib/',LibName+'.so*');
  if (lib = '') then
    lib := FindLib('/lib/',LibName+'.so*');

  Result := Lib
end;

end.

