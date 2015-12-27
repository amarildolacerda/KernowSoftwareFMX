unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, ksTableView,
  FMX.Objects;

type
  TForm24 = class(TForm)
    ToolBar2: TToolBar;
    ToolBar1: TToolBar;
    Image1: TImage;
    Label1: TLabel;
    ksTableView1: TksTableView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form24: TForm24;

implementation

uses System.UIConsts;

{$R *.fmx}

procedure TForm24.FormCreate(Sender: TObject);
begin
  Image1.Visible := False;
  ksTableView1.BeginUpdate;
  try
    ksTableView1.Items.AddChatBubble('Oh, hello there, this is the start of the conversation', ksCbpLeft, claDodgerblue, claWhite);
    ksTableView1.Items.AddChatBubble('Great conversation! thanks for letting me know!', ksCbpRight, $FFDDDDDD, claBlack);
    ksTableView1.Items.AddChatBubble('ok, gotta go!', ksCbpLeft, claDodgerblue, claWhite);
    ksTableView1.Items.AddChatBubble('Bye!', ksCbpRight, $FFDDDDDD, claBlack);
  finally
    ksTableView1.EndUpdate;
  end;
end;

end.
