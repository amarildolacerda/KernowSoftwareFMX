unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, ksSlideMenu, FMX.Layouts, FMX.Objects,
  FMX.TabControl, FMX.ListBox, FMX.ListView.Types, FMX.ListView, ksListView,
  ksSegmentButtons;

type
  TForm6 = class(TForm)
    ToolBar1: TToolBar;
    btnLeftMenu: TButton;
    btnRightMenu: TButton;
    Label1: TLabel;
    imgHome: TImage;
    imgSearch: TImage;
    imgCalendar: TImage;
    imgMenu: TImage;
    imgContact: TImage;
    layoutImages: TLayout;
    SlideMenu1: TksSlideMenu;

    SlideMenu2: TksSlideMenu;
    imgAbout: TImage;
    TabControl1: TTabControl;
    tabListView: TTabItem;
    tabSegmentButtons: TTabItem;
    lvGraphicsText: TksListView;
    lvSegmentButtons: TksListView;
    tabSwitches: TTabItem;
    lvSwitches: TksListView;
    tabIndicators: TTabItem;
    lvIndicators: TksListView;
    tabItemAccessorys: TTabItem;
    lvAccessorys: TksListView;
    tabCheckList: TTabItem;
    lvCheckList: TksListView;
    ToolBar2: TToolBar;
    Label2: TLabel;
    Switch1: TSwitch;
    Label3: TLabel;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    Label4: TLabel;
    Label5: TLabel;
    ToolBar5: TToolBar;
    Label6: TLabel;
    ToolBar6: TToolBar;
    Label7: TLabel;
    ToolBar7: TToolBar;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnRightMenuClick(Sender: TObject);
    procedure btnLeftMenuClick(Sender: TObject);
    procedure SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
    procedure lvSwitchesSwitchClick(Sender: TObject; AItem: TListViewItem;
      ASwitch: TksListItemRowSwitch; ARowID: string);
    procedure lvSegmentButtonsSegmentButtonClicked(Sender: TObject;
      AItem: TListViewItem; AButtons: TksListItemRowSegmentButtons;
      ARowID: string);
    procedure Switch1Switch(Sender: TObject);
    procedure lvCheckListItemClickEx(Sender: TObject; x, y: Single;
      AItem: TListViewItem; AId: string; ARowObj: TksListItemRowObj);
  private
    procedure BuildTextItemsListView;
    procedure BuildSegmentButtonListView;
    procedure BuildSwitchListView;
    procedure BuildIndicatorListView;
    procedure BuildAccessoryListView;
    procedure BuildCheckListView;
    { Private declarations }
  protected
    procedure DoShow; override;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses System.UIConsts;

{$R *.fmx}

procedure TForm6.btnLeftMenuClick(Sender: TObject);
begin
  SlideMenu1.ToggleMenu;
end;

procedure TForm6.btnRightMenuClick(Sender: TObject);
begin
  SlideMenu2.ToggleMenu;
end;

procedure TForm6.BuildTextItemsListView;
var
  ICount: integer;
begin
  lvGraphicsText.Items.BeginUpdate;
  try
    for ICount := 0 to 100 do
    begin
      with lvGraphicsText.AddRow do
      begin
        DrawBitmap(imgHome.Bitmap, 0, 24, 24);
        // first label...
        SetFontProperties('', 14, claBlack, []);
        TextOut('Line '+InttoStr(ICount), 40, -8, 150);

        SetFontProperties('', 12, claDimgray, []);
        TextOut('some extra text', 40, 8, 150);
        // right aligned label...
        TextColor := claDodgerblue;
        TextOutRight('right-aligned text :-)', 0, 120, 16);
      end;
    end;
  finally
    lvGraphicsText.Items.EndUpdate;
  end;
end;

procedure TForm6.BuildSegmentButtonListView;
var
  ICount: integer;
begin
  lvSegmentButtons.BeginUpdate;
  try
    for ICount := 1 to 100 do
    begin
      with lvSegmentButtons.AddRow do
      begin
        TextOut('Item '+IntToStr(ICount), 0);
        AddSegmentButtons(180, ['one', 'two', 'three']);
      end;
    end;
  finally
    lvSegmentButtons.EndUpdate;
  end;
end;

procedure TForm6.BuildSwitchListView;
var
  ICount: integer;
begin
  lvSwitches.BeginUpdate;
  try
    for ICount := 1 to 100 do
    begin
      with lvSwitches.AddRow do
      begin
        TextOut('Item '+IntToStr(ICount), 0);
        AddSwitchRight(0, False);
      end;
    end;
  finally
    lvSwitches.EndUpdate;
  end;
end;

procedure TForm6.BuildIndicatorListView;
var
  ICount: integer;
begin
  lvIndicators.BeginUpdate;
  try
    with lvIndicators.AddRow do
    begin
      TextOut('Green', 24);
      IndicatorColor := claGreen;
    end;
    with lvIndicators.AddRow do
    begin
      TextOut('Yellow', 24);
      IndicatorColor := claYellow;
    end;
    with lvIndicators.AddRow do
    begin
      TextOut('Blue', 24);
      IndicatorColor := claBlue;
    end;
    with lvIndicators.AddRow do
    begin
      TextOut('Red', 24);
      IndicatorColor := claRed;
    end;
    with lvIndicators.AddRow do
    begin
      TextOut('Orange', 24);
      IndicatorColor := claOrange;
    end;
    with lvIndicators.AddRow do
    begin
      TextOut('Teal', 24);
      IndicatorColor := claTeal;
    end;
    with lvIndicators.AddRow do
    begin
      TextOut('Fuchsia', 24);
      IndicatorColor := claFuchsia;
    end;
    with lvIndicators.AddRow do
    begin
      TextOut('Silver', 24);
      IndicatorColor := claSilver;
    end;
    with lvIndicators.AddRow do
    begin
      TextOut('Gray', 24);
      IndicatorColor := claGray;
    end;
    with lvIndicators.AddRow do
    begin
      TextOut('Black', 24);
      IndicatorColor := claBlack;
    end;
  finally
    lvIndicators.EndUpdate;
  end;
end;

procedure TForm6.BuildAccessoryListView;
var
  ICount: integer;
begin
  lvAccessorys.BeginUpdate;
  with lvAccessorys.AddRow do
  begin
    TextOut('No Accessory', 0);
    ShowAccessory := False;
  end;
  with lvAccessorys.AddRow do
  begin
    TextOut('"More" Accessory', 0);
    Accessory := TAccessoryType.More;
  end;
  with lvAccessorys.AddRow do
  begin
    TextOut('"Checkmark" Accessory', 0);
    Accessory := TAccessoryType.Checkmark;
  end;
  with lvAccessorys.AddRow do
  begin
    TextOut('"Detail" Accessory', 0);
    Accessory := TAccessoryType.Detail;
  end;
  lvAccessorys.EndUpdate;
end;

procedure Tform6.BuildCheckListView;
var
  ICount: integer;
begin
  lvCheckList.BeginUpdate;
  for ICount := 1 to 50 do
  begin
    lvCheckList.AddRow.TextOut('Item '+IntToStr(ICount), 0);
  end;
  lvCheckList.EndUpdate;
end;

procedure TForm6.DoShow;
begin
  inherited;
  BuildTextItemsListView;
  BuildSegmentButtonListView;
  BuildSwitchListView;
  BuildIndicatorListView;
  BuildAccessoryListView;
  BuildCheckListView;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  TabControl1.TabPosition := TTabPosition.None;
  TabControl1.TabIndex := 0;
  SlideMenu1.AddMenuItem('LISTVIEW', 'Graphics & Text', nil);
  SlideMenu1.AddMenuItem('SEGMENT_BUTTONS', 'Segment Buttons', nil);
  SlideMenu1.AddMenuItem('SWITCHES', 'Switches', nil);
  SlideMenu1.AddMenuItem('INDICATORS', 'Indicator Colours', nil);
  SlideMenu1.AddMenuItem('ACCESSORYS', 'Item Accessories', nil);
  SlideMenu1.AddMenuItem('CHECKLIST', 'Check List', nil);
  SlideMenu1.ItemIndex := 0;

  SlideMenu2.AddMenuItem('ANOTHER', 'Dummy menu item', imgHome.Bitmap);

  SlideMenu2.ItemIndex := 0;
  layoutImages.Visible := False;
end;




procedure TForm6.lvCheckListItemClickEx(Sender: TObject; x, y: Single;
  AItem: TListViewItem; AId: string; ARowObj: TksListItemRowObj);
begin
  Label3.Text := 'Checked count: '+IntToStr(lvCheckList.Items.CheckedCount(True));
end;

procedure TForm6.lvSegmentButtonsSegmentButtonClicked(Sender: TObject;
  AItem: TListViewItem; AButtons: TksListItemRowSegmentButtons; ARowID: string);
begin

  Label4.Text := ('Segment Click: row index: '+IntToStr(AItem.Index+1)+'   '+
              'button index: '+IntToStr(AButtons.ItemIndex)+#13+'   '+
              'text: '+AButtons.Captions[AButtons.ItemIndex]);
end;

procedure TForm6.lvSwitchesSwitchClick(Sender: TObject; AItem: TListViewItem;
  ASwitch: TksListItemRowSwitch; ARowID: string);
var
  ACheckedStr: string;
begin
  case ASwitch.IsChecked of
    True: ACheckedStr := '(Checked)';
    False: ACheckedStr := '(Unchecked)';
  end;
  Label6.Text := ('Switch clicked: row '+IntToStr(AItem.Index+1))+'   '+ACheckedStr;
end;

procedure TForm6.SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
begin
  if AId = 'LISTVIEW' then TabControl1.ActiveTab := tabListView;
  if AId = 'SEGMENT_BUTTONS' then TabControl1.ActiveTab := tabSegmentButtons;
  if AId = 'SWITCHES' then TabControl1.ActiveTab := tabSwitches;
  if AId = 'INDICATORS' then TabControl1.ActiveTab := tabIndicators;
  if AId = 'ACCESSORYS' then TabControl1.ActiveTab := tabItemAccessorys;
  if AId = 'CHECKLIST' then TabControl1.ActiveTab := tabCheckList;
  Application.ProcessMessages;
end;

procedure TForm6.Switch1Switch(Sender: TObject);
begin
  case Switch1.IsChecked of
    True: lvCheckList.CheckMarks := TksListViewCheckMarks.ksCmMultiSelect;
    False: lvCheckList.CheckMarks := TksListViewCheckMarks.ksCmSingleSelect;
  end;
end;

end.
