unit fShowPropForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ActnList, fCommon;

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
    popMenu : TPopupMenu;
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
  private
    OldX, OldY : Integer;
    FormMoving : Boolean;
  public
    { public declarations }
  end;

var
  frmShowPropForm : TfrmShowPropForm;

implementation

{$R *.lfm}

procedure TfrmShowPropForm.acOptionsExecute(Sender : TObject);
begin
  //
end;

procedure TfrmShowPropForm.acAboutExecute(Sender : TObject);
begin
  //
end;

procedure TfrmShowPropForm.acCloseExecute(Sender : TObject);
begin
  Close()
end;

procedure TfrmShowPropForm.acRefreshExecute(Sender : TObject);
begin
  //
end;

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

end.

