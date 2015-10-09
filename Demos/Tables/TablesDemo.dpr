program TablesDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  unit1 in 'unit1.pas' {Form7};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
