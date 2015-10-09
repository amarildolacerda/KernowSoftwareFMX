unit unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, ksListView, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm7 = class(TForm)
    ToolBar1: TToolBar;
    ksListView1: TksListView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

uses System.UIConsts;

{$R *.fmx}

procedure TForm7.FormCreate(Sender: TObject);
var
  ARow: TKsListItemRow;
  ATbl: TksListItemRowTable;
  ICount: integer;
begin
  ksListView1.BeginUpdate;
  for ICount := 1 to 20 do
  begin
    ARow := ksListView1.Items.AddRow('', '', '', More);
    ARow.Height := 150;
    ATbl := ARow.AddTable(30, 30, 40, 20, 5, 5);
    ATbl.ColWidths[0] := 40;

    ATbl.Banding.Active := True;
    ATbl.Banding.Color1 := claPalegoldenrod;

    with ATbl.Cells[0, 2] do
    begin
     // Text := 'test';
      TextSettings.FontColor := claRed;
      TextSettings.HorzAlign := TTextAlign.Trailing;
      Padding.Right := 4;
      Text := 'Text';
    end;

    ATbl.MergeRowCells(3, 3, 2);
    ATbl.Cells[3,3].Text := 'Merged cells';
  end;
  ksListView1.EndUpdate;
end;

end.
