unit dUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type
  TdmUtils = class(TDataModule)
  private
    { private declarations }
  public
    function GetAppConfigFileName : String;
  end;

var
  dmUtils : TdmUtils;

implementation

{$R *.lfm}

function TdmUtils.GetAppConfigFileName : String;
begin
  Result := ExtractFilePath(GetAppConfigFile(False))+'cqrprop'+DirectorySeparator+'cqrprop.cfg'
end;

end.

