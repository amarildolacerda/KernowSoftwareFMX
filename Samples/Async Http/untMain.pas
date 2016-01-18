unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, FMX.StdCtrls,
  FMX.Controls.Presentation, ksNetHttpClient, FMX.ScrollBox, FMX.Memo, FMX.Objects;

type
  TForm33 = class(TForm)
    ksNetHttpClient1: TksNetHttpClient;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Rectangle1: TRectangle;
    Arc1: TArc;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
     procedure DoReceiveData(Sender: TObject; AResponse: IHTTPResponse);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form33: TForm33;

implementation

{$R *.fmx}

procedure TForm33.Button1Click(Sender: TObject);
var
  AResponse: IHTTPResponse;
begin
  // standard (blocking) get method...
  Memo1.Lines.Clear;
  Application.ProcessMessages;
  AResponse := ksNetHttpClient1.Get('http://www.embarcadero.com');
  Memo1.Lines.Text := AResponse.ContentAsString;
end;

procedure TForm33.Button2Click(Sender: TObject);
begin
  // async get...
  Memo1.Lines.Clear;
  Application.ProcessMessages;
  ksNetHttpClient1.GetAsync('http://www.embarcadero.com', nil, DoReceiveData);
end;

procedure TForm33.DoReceiveData(Sender: TObject; AResponse: IHTTPResponse);
begin
  // async response...
  Memo1.Lines.Text := AResponse.ContentAsString;
end;

end.
