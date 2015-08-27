program ksComponents;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in 'untMain.pas' {frmMain},
  untSourceCode in 'untSourceCode.pas' {frmSourceCode};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

