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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    iniLocal : TIniFile;

    procedure SaveWindowPos;
    procedure LoadWindowPos;

    function GetLocalConfigFile : String;
  public
    property ConfigFile : String read GetLocalConfigFile;
  end; 

var
  frmCommon: TfrmCommon;

implementation

{$R *.lfm}

procedure TfrmCommon.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveWindowPos;
  iniLocal.UpdateFile;
  FreeAndNil(iniLocal)
end;

procedure TfrmCommon.FormCreate(Sender: TObject);
begin
  iniLocal := TIniFile.Create(GetLocalConfigFile)
end;

procedure TfrmCommon.FormShow(Sender: TObject);
begin
  LoadWindowPos
end;

function TfrmCommon.GetLocalConfigFile : String;

   procedure CreateEmptyFile(emFile : String);
   var
     f : TextFile;
   begin
     AssignFile(f,emFile);
     Rewrite(f);
     WriteLn(f,'');
     CloseFile(f)
   end;

var
  dir : String;
begin
  dir := ExtractFilePath(GetAppConfigFile(False))+APP_NAME+DirectorySeparator;
  if DirectoryExistsUTF8(dir) then
  begin
    if (not FileExistsUTF8(dir+APP_NAME+'.local.cfg')) then
      CreateEmptyFile(dir+APP_NAME+'.local.cfg')
  end
  else begin
    CreateDir(dir);
    CreateEmptyFile(dir+APP_NAME+'.local.cfg')
  end;
  Result := dir+APP_NAME+'.local.cfg'
end;

procedure TfrmCommon.SaveWindowPos;
begin
  if (WindowState = wsMaximized) then
     iniLocal.WriteBool(name,'Max',True)
  else begin
    iniLocal.WriteInteger(name,'Height',Height);
    iniLocal.WriteInteger(name,'Width',Width);
    iniLocal.WriteInteger(name,'Top',Top);
    iniLocal.WriteInteger(name,'Left',Left)
  end
end;

procedure TfrmCommon.LoadWindowPos;
begin
  if iniLocal.ReadBool(name,'Max',False) then
    WindowState := wsMaximized
  else begin
    if (BorderStyle <> bsDialog) then
    begin
      Height := iniLocal.ReadInteger(name,'Height',Height);
      Width  := iniLocal.ReadInteger(name,'Width',Width)
    end;
    Top  := iniLocal.ReadInteger(name,'Top',Top);
    Left := iniLocal.ReadInteger(name,'Left',Left)
  end
end;


end.

