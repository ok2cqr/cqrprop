unit fOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, fCommon, IniFiles, process;

type

  { TfrmOptions }

  TfrmOptions = class(TfrmCommon)
    btnOpenWeb : TButton;
    btnOK : TButton;
    btnCancel : TButton;
    edtDownloadLink : TEdit;
    edtRefreshTime : TSpinEdit;
    edtWaitTime : TSpinEdit;
    GroupBox1 : TGroupBox;
    GroupBox2 : TGroupBox;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    procedure btnOKClick(Sender : TObject);
    procedure btnOpenWebClick(Sender : TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmOptions : TfrmOptions;

implementation

{$R *.lfm}

uses dUtils;

procedure TfrmOptions.FormShow(Sender: TObject);
var
  ini : TIniFile;
begin
  inherited;
  ini := TIniFile.Create(dmUtils.GetAppConfigFileName);
  try
    edtDownloadLink.Text := ini.ReadString('App', 'DownloadLink', 'http://www.hamqsl.com/solar2.php');
    edtRefreshTime.Value := ini.ReadInteger('App', 'RefreshTime', 5);
    edtWaitTime.Value    := ini.ReadInteger('App', 'WaitTime', 2)
  finally
    FreeAndNil(ini)
  end
end;

procedure TfrmOptions.btnOKClick(Sender : TObject);
var
  ini : TIniFile;
begin
  ini := TIniFile.Create(dmUtils.GetAppConfigFileName);
  try
    ini.CacheUpdates := False;
    ini.WriteString('App', 'DownloadLink', edtDownloadLink.Text);
    ini.WriteInteger('App', 'RefreshTime', edtRefreshTime.Value);
    ini.WriteInteger('App', 'WaitTime', edtWaitTime.Value)
  finally
    ini.UpdateFile;
    FreeAndNil(ini)
  end;

  ModalResult := mrOK
end;

procedure TfrmOptions.btnOpenWebClick(Sender : TObject);
var
  Process : TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'xdg-open';
    Process.Parameters.Add('http://www.hamqsl.com/solar.html');
    Process.Execute
  finally
    FreeAndNil(Process)
  end
end;

end.

