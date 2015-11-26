unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, ksTableView;

type
  TForm24 = class(TForm)
    ksTableView1: TksTableView;
    ToolBar2: TToolBar;
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ksTableView1ItemSwipe(Sender: TObject; ARow: TksTableViewItem; ASwipeDirection: TksSwipeDirection; AButtons: TksTableViewActionButtons);
    procedure ksTableView1ItemActionButtonClick(Sender: TObject; ARow: TksTableViewItem; AButton: TksTableViewActionButton);
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
var
  ICount: integer;
begin
  // add 50 items to the ksTableView...
  ksTableView1.BeginUpdate;
  try
    for ICount := 1 to 50 do
      ksTableView1.Items.AddItem('Item: '+IntToStr(ICount),'sub title', 'some detail', atMore);
  finally
    ksTableView1.EndUpdate;
  end;
end;

procedure TForm24.ksTableView1ItemActionButtonClick(Sender: TObject; ARow: TksTableViewItem; AButton: TksTableViewActionButton);
begin
  // show a message for the button clicked...
  ShowMessage('You clicked on: '+AButton.Text);
end;

procedure TForm24.ksTableView1ItemSwipe(Sender: TObject; ARow: TksTableViewItem; ASwipeDirection: TksSwipeDirection; AButtons: TksTableViewActionButtons);
begin
  if ASwipeDirection = ksSwipeRightToLeft then
  begin
    // add right-side action buttons...
    AButtons.AddButton('Copy', claDodgerblue, claWhite, 60);
    AButtons.AddButton('Move', claSilver, claWhite, 60);
  end;
  if ASwipeDirection = ksSwipeLeftToRight then
  begin
    // add left-side action buttons...
    AButtons.AddButton('Archive', claGreen, claWhite, 60);
    AButtons.AddButton('Reply', claOrange, claWhite, 60);
  end;
end;

end.
