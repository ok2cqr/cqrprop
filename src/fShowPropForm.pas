unit fShowPropForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, fCommon;

type

  { TfrmShowPropForm }

  TfrmShowPropForm = class(TfrmCommon)
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmShowPropForm : TfrmShowPropForm;

implementation

{$R *.lfm}
procedure TfrmShowPropForm.FormShow(Sender: TObject);
begin
  inherited;
  //this will hide the window from a Cinnamon panel
  ShowInTaskBar := stNever
end;

procedure TfrmShowPropForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  Application.Terminate
end;

end.

