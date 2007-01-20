program Designer;

uses
  FastMM4,
  Forms,
  uMainForm in 'uMainForm.pas' {frmMain},
  uConnections in 'uConnections.pas' {frmConnections},
  uGlobal in 'uGlobal.pas' {frmGlobals},
  uConfirmReplaceForm in 'uConfirmReplaceForm.pas' {frmConfirmReplace},
  uReplaceTextForm in 'uReplaceTextForm.pas' {frmTextReplace},
  uSearchTextForm in 'uSearchTextForm.pas' {frmTextSearch};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'HTML Report Designer';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGlobals, frmGlobals);
  Application.Run;
end.
