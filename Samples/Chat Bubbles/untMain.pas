unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, ksTableView,
  FMX.Objects, FMX.Edit, FMX.Layouts, ksChatView, ksTypes;

type
  TForm24 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    Button2: TButton;
    Image1: TImage;
    Image2: TImage;
    ksChatView1: TksChatView;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ksChatView1BeforePostText(Sender: TObject; var ABubble: TksChatBubbleInfo);
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

procedure TForm24.Button1Click(Sender: TObject);
begin
  ksChatView1.AddChatBubble('Hi There!', TksTableViewChatBubblePosition.ksCbpRight, $FFDDDDDD, claBlack, Image1.Bitmap);
end;

procedure TForm24.Button2Click(Sender: TObject);
begin
  ksChatView1.Clear;
end;

procedure TForm24.Edit1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then
    Button1Click(Self);
end;

procedure TForm24.FormCreate(Sender: TObject);
begin
  Width := FormFactor.Width;
  Height := FormFactor.Height;

  Image1.Visible := False;
  Image2.Visible := False;

  ksChatView1.MyImage := Image2.Bitmap;

  ksChatView1.AddChatBubble('Hi there, I thought I''d get in touch to let you know how great I think your components are, thanks a million!', ksCbpLeft, claDodgerblue, claWhite, Image2.Bitmap);
  ksChatView1.AddChatBubble('Thanks! Currently working on a new form transition compoenent and also the ability to add user images to chat bubbles like you see here', ksCbpRight, $FFDDDDDD, claBlack, Image1.Bitmap);
  ksChatView1.AddChatBubble('Wow!  '+#13+'You''re the man!!!', ksCbpLeft, claDodgerblue, claWhite, Image2.Bitmap);
end;

procedure TForm24.ksChatView1BeforePostText(Sender: TObject; var ABubble: TksChatBubbleInfo);
begin
  ABubble.Text := 'TEST_'+ABubble.Text;
end;

end.
