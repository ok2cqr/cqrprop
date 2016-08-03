unit fShowPropForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ActnList, fCommon, httpsend, IniFiles, LazFileUtils, ssl_openssl_lib;

type

  { TfrmShowPropForm }

  TfrmShowPropForm = class(TfrmCommon)
    acProp : TActionList;
    acOptions : TAction;
    acAbout : TAction;
    acRefresh : TAction;
    acClose : TAction;
    imgProp : TImage;
    MenuItem1 : TMenuItem;
    MenuItem2 : TMenuItem;
    MenuItem3 : TMenuItem;
    MenuItem5 : TMenuItem;
    MenuItem6 : TMenuItem;
    MenuItem7 : TMenuItem;
    pnlInfo : TPanel;
    popMenu : TPopupMenu;
    tmrImageDownload : TTimer;
    procedure acAboutExecute(Sender : TObject);
    procedure acCloseExecute(Sender : TObject);
    procedure acOptionsExecute(Sender : TObject);
    procedure acRefreshExecute(Sender : TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure imgPropMouseDown(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer);
    procedure imgPropMouseMove(Sender : TObject; Shift : TShiftState; X,
      Y : Integer);
    procedure imgPropMouseUp(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer);
    procedure tmrImageDownloadTimer(Sender : TObject);
  private
    OldX, OldY : Integer;
    FormMoving : Boolean;

    function  GetTimerInterval : Integer;
  public
    ImageFileName    : String;
    ImageFile        : TFileStream;
    OldImageFileName : String;

    procedure SynShowImage;
  end;

type
  TImageDownloadThread = class(TThread)
  private
    function  DownloadToTempFile(const Url,FileName : String) : Boolean;
  protected
    procedure Execute; override;
end;

var
  frmShowPropForm : TfrmShowPropForm;

implementation

{$R *.lfm}

uses fAbout, fOptions, dUtils;

procedure TImageDownloadThread.Execute;
var
  ini : TIniFile;
  Url : String;
begin
  ini := TIniFile.Create(dmUtils.GetAppConfigFileName);
  try
    Url := ini.ReadString('App', 'DownloadLink', 'http://www.hamqsl.com/solar2.php');

    frmShowPropForm.ImageFileName := ExtractFilePath(dmUtils.GetAppConfigFileName) + 'solar' + IntToStr(Random(10000));

    if DownloadToTempFile(Url, frmShowPropForm.ImageFileName) then
    begin

      if (FileExistsUTF8(frmShowPropForm.OldImageFileName)) then
        DeleteFileUTF8(frmShowPropForm.OldImageFileName);
      frmShowPropForm.OldImageFileName := frmShowPropForm.ImageFileName;

      Synchronize(@frmShowPropForm.SynShowImage)
    end
  finally
    FreeAndNil(ini)
  end
end;

function TImageDownloadThread.DownloadToTempFile(const Url,FileName : String) : Boolean;
var
  Http : THTTPSend;
  Mem  : TFileStream;

  ProxyHost : String;
  ProxyPort : Integer = 0;
  ProxyUser : String;
  ProxyPass : String;
begin
  Http := THTTPSend.Create;
  Mem  := TFileStream.Create(FileName,fmCreate);
  try
    if (Pos('https://', Url) = 1) then
      dmUtils.GetProxyParams(ptHTTPS, ProxyHost, ProxyPort, ProxyUser, ProxyPass)
    else
      dmUtils.GetProxyParams(ptHTTP, ProxyHost, ProxyPort, ProxyUser, ProxyPass);

    Http.ProxyHost := ProxyHost;
    Http.ProxyPort := IntToStr(ProxyPort);
    Http.ProxyUser := ProxyUser;
    Http.ProxyPass := ProxyPass;

    Result := Http.HTTPMethod('GET', Url);
    if Result then
    begin
      Mem.Seek(0, soFromBeginning);
      Mem.CopyFrom(Http.Document, 0)
    end
  finally
    FreeAndNil(Http);
    FreeAndNil(Mem)
  end
end;

procedure TfrmShowPropForm.acOptionsExecute(Sender : TObject);
begin
  frmOptions := TfrmOptions.Create(frmShowPropForm);
  try
    frmOptions.ShowModal
  finally
    FreeAndNil(frmOptions)
  end
end;

procedure TfrmShowPropForm.acAboutExecute(Sender : TObject);
begin
  frmAbout := TfrmAbout.Create(frmShowPropForm);
  try
    frmAbout.ShowModal
  finally
    FreeAndNil(frmAbout)
  end
end;

procedure TfrmShowPropForm.acCloseExecute(Sender : TObject);
begin
  Close()
end;

procedure TfrmShowPropForm.acRefreshExecute(Sender : TObject);
var
  ImageDownloadThread : TImageDownloadThread;
begin
  imgProp.Visible := False;

  ImageDownloadThread := TImageDownloadThread.Create(True);
  ImageDownloadThread.FreeOnTerminate := True;
  ImageDownloadThread.Start
end;

procedure TfrmShowPropForm.FormShow(Sender: TObject);
begin
  inherited;
  DLLSSLName  := dmUtils.GetSSLLib('libssl');;
  DLLUtilName := dmUtils.GetSSLLib('libcrypto');
  //this will hide the window from a Cinnamon panel
  ShowInTaskBar := stNever;
  ImageFile     := nil;

  tmrImageDownload.Interval := GetTimerInterval;
  tmrImageDownload.Enabled  := True;
  acRefresh.Execute
end;

procedure TfrmShowPropForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  tmrImageDownload.Enabled := False;
  FreeAndNil(ImageFile);
  if (FileExistsUTF8(ImageFileName)) then
    DeleteFileUTF8(ImageFileName);
  Application.Terminate
end;

procedure TfrmShowPropForm.imgPropMouseDown(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  if (Button = mbLeft) then
  begin
    FormMoving := True;
    OldX := X;
    OldY := Y
  end
end;

procedure TfrmShowPropForm.imgPropMouseMove(Sender : TObject;
  Shift : TShiftState; X, Y : Integer);
begin
  if FormMoving then
  begin
    Left := Left + (X - OldX);
    Top  := Top + (Y - OldY)
  end
end;

procedure TfrmShowPropForm.imgPropMouseUp(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  FormMoving := False
end;

procedure TfrmShowPropForm.tmrImageDownloadTimer(Sender : TObject);
begin
  acRefresh.Execute
end;

function TfrmShowPropForm.GetTimerInterval : Integer;
var
  ini : TIniFile;
begin
  ini := TIniFile.Create(dmUtils.GetAppConfigFileName);
  try
    Result := ini.ReadInteger('App', 'RefreshTime', 5) * 1000 * 60//in miliseconds
  finally
    FreeAndNil(ini)
  end
end;

procedure TfrmShowPropForm.SynShowImage;
begin
  if Assigned(ImageFile) then
    FreeAndNil(ImageFile);

  ImageFile := TFileStream.Create(ImageFileName,fmOpenRead);

  imgProp.Picture.LoadFromStream(ImageFile);

  imgProp.Visible := True;

  pnlInfo.Height := imgProp.Height;
  pnlInfo.Width  := imgProp.Width;

  Height := imgProp.Height;
  Width  := imgProp.Width
end;



end.

