unit fShowPropForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, fCommon;

type

  { TfrmShowPropForm }

  TfrmShowPropForm = class(TfrmCommon)
    procedure FormMouseDown(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer);
    procedure FormMouseMove(Sender : TObject; Shift : TShiftState; X,
      Y : Integer);
    procedure FormMouseUp(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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

procedure TfrmShowPropForm.FormMouseDown(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  if (Button = mbLeft) then
  begin
    FormMoving := True;
    OldX := X;
    OldY := Y
  end
end;

procedure TfrmShowPropForm.FormMouseMove(Sender : TObject; Shift : TShiftState;
  X, Y : Integer);
begin
  if FormMoving then
  begin
    Left := Left + (X - OldX);
    Top  := Top + (Y - OldY)
  end
end;

procedure TfrmShowPropForm.FormMouseUp(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  FormMoving := False
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

end.

