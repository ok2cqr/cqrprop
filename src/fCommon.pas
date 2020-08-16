unit fCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, iniFiles, LazFileUtils;

const
  APP_NAME = 'cqrprop';

type

  { TfrmCommon }

  TfrmCommon = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    function GetLocalConfigFile: string;
  public
    procedure SaveWindowPos;
    procedure LoadWindowPos;

    property ConfigFile: string read GetLocalConfigFile;
  end;

var
  frmCommon: TfrmCommon;

implementation

{$R *.lfm}

procedure TfrmCommon.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveWindowPos;
end;

procedure TfrmCommon.FormShow(Sender: TObject);
begin
  LoadWindowPos;
end;

function TfrmCommon.GetLocalConfigFile: string;

  procedure CreateEmptyFile(emFile: string);
  var
    f: TextFile;
  begin
    AssignFile(f, emFile);
    Rewrite(f);
    WriteLn(f, '');
    CloseFile(f);
  end;

var
  dir: string;
begin
  dir := ExtractFilePath(GetAppConfigFile(False)) + APP_NAME + DirectorySeparator;
  if DirectoryExistsUTF8(dir) then
  begin
    if (not FileExistsUTF8(dir + APP_NAME + '.local.cfg')) then
      CreateEmptyFile(dir + APP_NAME + '.local.cfg');
  end
  else begin
    CreateDir(dir);
    CreateEmptyFile(dir + APP_NAME + '.local.cfg');
  end;
  Result := dir + APP_NAME + '.local.cfg';
end;

procedure TfrmCommon.SaveWindowPos;
var
  iniLocal: TIniFile;
begin
  iniLocal := TIniFile.Create(GetLocalConfigFile);
  try
    if (WindowState = wsMaximized) then
      iniLocal.WriteBool(Name, 'Max', True)
    else begin
      iniLocal.WriteInteger(Name, 'Height', Height);
      iniLocal.WriteInteger(Name, 'Width', Width);
      iniLocal.WriteInteger(Name, 'Top', Top);
      iniLocal.WriteInteger(Name, 'Left', Left);
    end
  finally
    iniLocal.UpdateFile;
    FreeAndNil(iniLocal)
  end;
end;

procedure TfrmCommon.LoadWindowPos;
var
  iniLocal: TIniFile;
begin
  iniLocal := TIniFile.Create(GetLocalConfigFile);
  try
    if iniLocal.ReadBool(Name, 'Max', False) then
      WindowState := wsMaximized
    else begin
      if (BorderStyle <> bsDialog) then
      begin
        Height := iniLocal.ReadInteger(Name, 'Height', Height);
        Width := iniLocal.ReadInteger(Name, 'Width', Width);
      end;
      Top := iniLocal.ReadInteger(Name, 'Top', Top);
      Left := iniLocal.ReadInteger(Name, 'Left', Left);
    end
  finally
    FreeAndNil(iniLocal)
  end;
end;


end.

