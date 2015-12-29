unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, ksTableView,
  FMX.Objects, FMX.Edit, FMX.Layouts;

type
  TForm24 = class(TForm)
    ToolBar2: TToolBar;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ksTableView1: TksTableView;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure Edit1Enter(Sender: TObject);
  private
    FPosition: TksTableViewChatBubblePosition;
    procedure AddChatText(AText: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form24: TForm24;

implementation

uses System.UIConsts, FMX.Platform, FMX.VirtualKeyboard;
{$R *.fmx}

procedure TForm24.AddChatText(AText: string);
var
  AColor, ATextColor: TAlphaColor;
begin
  if Trim(AText) = '' then
    Exit;

  // set the fill and text colours based upon the chat bubble alignment...
  if FPosition = ksCbpLeft then
  begin
    AColor := claDodgerblue;
    ATextColor := claWhite;
  end
  else
  begin
    AColor := $FFDDDDDD;
    ATextColor := claBlack;
  end;

  ksTableView1.Items.AddChatBubble(AText, FPosition, AColor, ATextColor);

  // switch the position for the next chat bubble...
  if FPosition = ksCbpLeft then
    FPosition := ksCbpRight
  else
    FPosition := ksCbpLeft;
end;

procedure TForm24.Button1Click(Sender: TObject);
begin
  AddChatText(Edit1.Text);
  Edit1.Text := '';
  SetFocused(nil);
  {$IFDEF MSWINDOWS}
  ksTableView1.ScrollToItem(ksTableView1.Items.LastItem, True);
  {$ENDIF}
end;

procedure TForm24.Button2Click(Sender: TObject);
begin
  ksTableView1.ClearItems;
  FPosition := ksCbpLeft;
end;

procedure TForm24.Edit1Enter(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  // no virtual keyboard so we scroll here instead...
  ksTableView1.ScrollToItem(ksTableView1.Items.LastItem, True);
  {$ENDIF}
end;

procedure TForm24.Edit1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then
    Button1Click(Self);
end;

procedure TForm24.FormCreate(Sender: TObject);
var
  VKToolbar: IFMXVirtualKeyboardToolbarService;
begin
  // hide the done button
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, IInterface(VKToolbar)) then
    VKToolbar.SetToolbarEnabled(False);

  FPosition := ksCbpLeft;
  ksTableView1.Appearence.SeparatorColor := claNull;
  Layout1.Height := 0;

  ksTableView1.BeginUpdate;
  try
    AddChatText('Oh, hello there, this is the start of the conversation');
    AddChatText('Great conversation! thanks for letting me know!');
    AddChatText('ok, gotta go!');
    AddChatText('Bye!');
  finally
    ksTableView1.EndUpdate;
  end;
end;

procedure TForm24.FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
begin
  // set to 0 here rather than Bounds.Height as it appears to be a Firemonkey bug.
  Layout1.Height := 0;
  ksTableView1.ScrollToItem(ksTableView1.Items.LastItem, True);
end;

procedure TForm24.FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
begin
  Layout1.Height := Bounds.Height;
  ksTableView1.ScrollToItem(ksTableView1.Items.LastItem, True);
end;

end.
