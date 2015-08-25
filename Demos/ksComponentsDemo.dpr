program ksComponentsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in 'untMain.pas' {Form6},
  FMX.ListView in 'c:\program files (x86)\embarcadero\studio\16.0\source\fmx\FMX.ListView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
