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
    procedure ksTableView1CanDragItem(Sender: TObject; ADragRow: TksTableViewItem; var AllowDrag: Boolean);
    procedure ksTableView1CanDropItem(Sender: TObject; ADragRow, ADropRow: TksTableViewItem; var AllowDrop: Boolean);
    procedure ksTableView1DropItem(Sender: TObject; ADragRow, ADropRow: TksTableViewItem; var AllowMove: Boolean);
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
    for ICount := 1 to 10 do
      ksTableView1.Items.AddItem('Item: '+IntToStr(ICount),'sub title', 'some detail', atMore);
  finally
    ksTableView1.EndUpdate;
  end;
end;

procedure TForm24.ksTableView1CanDragItem(Sender: TObject; ADragRow: TksTableViewItem; var AllowDrag: Boolean);
begin
  AllowDrag := True;
end;

procedure TForm24.ksTableView1CanDropItem(Sender: TObject; ADragRow, ADropRow: TksTableViewItem; var AllowDrop: Boolean);
begin
  // allow dropping of items to all rows except index 4...
  AllowDrop := ADropRow.Index <> 4;
end;

procedure TForm24.ksTableView1DropItem(Sender: TObject; ADragRow, ADropRow: TksTableViewItem; var AllowMove: Boolean);
begin
  // Set AllowMove so that the item position is updated...
  AllowMove := True;
end;

end.
