unit fMainFormHidden;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TfrmMainFormHidden }

  TfrmMainFormHidden = class(TForm)
    tmrMain: TTimer;
    procedure tmrMainTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMainFormHidden: TfrmMainFormHidden;

implementation

{$R *.lfm}

uses fShowPropForm;

{ TfrmMainFormHidden }

procedure TfrmMainFormHidden.tmrMainTimer(Sender: TObject);
begin
  tmrMain.Enabled := False;
  frmShowPropForm.Show;
end;

end.

