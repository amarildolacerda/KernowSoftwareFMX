unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, ksTypes, ksLoadingIndicator;

type
  TForm64 = class(TForm)
    ToolBar2: TToolBar;
    ToolBar1: TToolBar;
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    ksLoadingIndicator1: TksLoadingIndicator;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form64: TForm64;

implementation

{$R *.fmx}

procedure TForm64.Button1Click(Sender: TObject);
begin
  ksLoadingIndicator1.ShowLoading;
end;

procedure TForm64.Button2Click(Sender: TObject);
begin
  ksLoadingIndicator1.HideLoading;
end;

end.
