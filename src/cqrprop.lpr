program cqrprop;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  fMainFormHidden,
  fCommon,
  fShowPropForm,
  fAbout,
  fOptions,
  dUtils,
  uVersion { you can add units after this };

{$R *.res}

begin
  Application.Scaled := True;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMainFormHidden, frmMainFormHidden);
  Application.CreateForm(TfrmShowPropForm, frmShowPropForm);
  //this will hide app from Unity panel in Ubuntu, but won't hide it in Linux Mint
  Application.ShowMainForm := False;
  Application.Run;
end.
