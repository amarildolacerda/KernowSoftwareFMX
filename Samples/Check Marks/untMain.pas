unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, ksTableView, FMX.ListBox;

type
  TForm24 = class(TForm)
    ksTableView1: TksTableView;
    ToolBar2: TToolBar;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Label1Click(Sender: TObject);
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

procedure TForm24.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.IsChecked then
    ksTableView1.CheckMarkOptions.CheckMarks := TksTableViewCheckMarks.cmMultiSelect
  else
    ksTableView1.CheckMarkOptions.CheckMarks := TksTableViewCheckMarks.cmSingleSelect;
end;

procedure TForm24.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: ksTableView1.CheckMarkOptions.Position := cmpLeft;
    1: ksTableView1.CheckMarkOptions.Position := cmpRight;
  end;
end;

procedure TForm24.FormCreate(Sender: TObject);
var
  ICount: integer;
begin
  // add 20 items to the ksTableView...
  ksTableView1.BeginUpdate;
  try
    for ICount := 1 to 20 do
      ksTableView1.Items.AddItem('Item: '+IntToStr(ICount), atNone);
  finally
    ksTableView1.EndUpdate;
  end;
end;

procedure TForm24.Label1Click(Sender: TObject);
begin
  ksTableView1.ItemIndex := 15;
  ksTableView1.BringSelectedIntoView;
  ShowMessage(FloatToStr(ksTableView1.ScrollViewPos));
end;

end.
