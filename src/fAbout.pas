unit fAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,fCommon,
  StdCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TfrmCommon)
    Bevel1 : TBevel;
    btnOK : TButton;
    Image1 : TImage;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    lblBuildDate : TLabel;
    lblVersion : TLabel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout : TfrmAbout;

implementation

{$R *.lfm}

uses uVersion;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  inherited;
  lblVersion.Caption   := cVERSION;
  lblBuildDate.Caption := cBUILD_DATE
end;

end.

