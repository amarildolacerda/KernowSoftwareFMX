program FormTransitions;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in 'untMain.pas' {Form3},
  untOtherForm in 'untOtherForm.pas' {Form5},
  ksChatView in '..\..\ksChatView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
