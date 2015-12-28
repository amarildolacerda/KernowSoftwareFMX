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
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  ksTableView1.Items[0].IndicatorColor := claGreen;
end;

procedure TForm24.CheckBox1Change(Sender: TObject);
begin
  ksTableView1.RowIndicators.Shadow := CheckBox1.IsChecked;
end;

procedure TForm24.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: ksTableView1.RowIndicators.Shape := ksRectangle;
    1: ksTableView1.RowIndicators.Shape := ksSquare;
    2: ksTableView1.RowIndicators.Shape := ksEllipse;
  end;
end;

procedure TForm24.FormCreate(Sender: TObject);
begin
  ksTableView1.BeginUpdate;
  try
    ksTableView1.Items.AddItem('Red', atMore).IndicatorColor := claRed;
    ksTableView1.Items.AddItem('Orange', atMore).IndicatorColor := claOrange;
    ksTableView1.Items.AddItem('Yellow', atMore).IndicatorColor := claYellow;
    ksTableView1.Items.AddItem('Green', atMore).IndicatorColor := claGreenyellow;
    ksTableView1.Items.AddItem('Blue', atMore).IndicatorColor := claBlue;
    ksTableView1.Items.AddItem('Indigo', atMore).IndicatorColor := claIndigo;
    ksTableView1.Items.AddItem('Violet', atMore).IndicatorColor := claViolet;
  finally
    ksTableView1.EndUpdate;
  end;
end;

end.
