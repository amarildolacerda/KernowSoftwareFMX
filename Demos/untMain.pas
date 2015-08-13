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
    ksListView1: TksListView;
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
    ToolBar8: TToolBar;
    ksSegmentButtons1: TksSegmentButtons;
    Image1: TImage;
    Image2: TImage;
    tabEverything: TTabItem;
    lvSmoothScrolling: TksListView;
    Timer1: TTimer;
    lblLoading: TLabel;
    tabProgressBars: TTabItem;
    ToolBar9: TToolBar;
    Label9: TLabel;
    lvProgressBars: TksListView;
    procedure FormCreate(Sender: TObject);
    procedure btnRightMenuClick(Sender: TObject);
    procedure btnLeftMenuClick(Sender: TObject);
    procedure SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
    procedure Switch1Switch(Sender: TObject);
    procedure ksSegmentButtons1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SlideMenu1AfterSlideOut(Sender: TObject);
    procedure lvSegmentButtonsSegmentButtonClicked(Sender: TObject;
      AItem: TKsListItemRow; AButtons: TksListItemRowSegmentButtons;
      ARowID: string);
    procedure lvSwitchesSwitchClick(Sender: TObject; AItem: TKsListItemRow;
      ASwitch: TksListItemRowSwitch; ARowID: string);
    procedure lvCheckListItemClick(Sender: TObject; x, y: Single;
      AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
    procedure ksListView1ItemClick(Sender: TObject; x, y: Single;
      AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
  private
    procedure BuildTextItemsListView;
    procedure BuildSegmentButtonListView;
    procedure BuildProgressBarsListview;
    procedure BuildSwitchListView;
    procedure BuildIndicatorListView;
    procedure BuildAccessoryListView;
    procedure BuildCheckListView;
    procedure BuildEverythingListView;
    procedure ShowLoading;
    procedure HideLoading;
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
  ARow: TKsListItemRow;
begin
  if ksListView1.Items.Count > 0 then
    Exit;
  ShowLoading;
  ksListview1.BeginUpdate;
  try
    for ICount := 1 to 5 do
    begin
      ARow := ksListview1.Items.AddRow('Line '+InttoStr(ICount),            // main title
                                 'a sub title for '+IntToStr(ICount), // subtitle
                                 'detail text',                       // detail text
                                 More,                                // "more" accessory
                                 Image2.Bitmap);                      // image
      // set image to circle shape
      ARow.Image.ImageShape := ksCircleImage;
    end;
  finally
    ksListview1.EndUpdate;
    HideLoading;
  end;
end;

procedure TForm6.Button1Click(Sender: TObject);
begin
  ShowMessage(BoolToStr(ksListView1.IsShowing, True)+#13+#13+
              BoolToStr(lvSegmentButtons.IsShowing, True));
end;

procedure TForm6.BuildEverythingListView;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  if lvSmoothScrolling.Items.Count > 0 then
    Exit;
  ShowLoading;
  lvSmoothScrolling.BeginUpdate;
  try
    for ICount := 1 to 30 do
    begin
      ARow := lvSmoothScrolling.Items.AddRow('Line '+InttoStr(ICount),            // main title
                                 'sub title', // subtitle
                                 'detail',                       // detail text
                                 More,                                // "more" accessory
                                 Image2.Bitmap);                      // image
      ARow.AddSwitch(100, True);
      ARow.AddSegmentButtons(160, 120, ['one','two','three'], TListItemAlign.Leading);
      ARow.DrawBitmap(imgSearch.Bitmap, 290, 32, 32);
      ARow.DrawRect(330, 0, 20, 20, claBlack, claRed);
      ARow.DrawRoundRect(350, 0, 20, 20, 5, claBlack, claGreen);
      ARow.DrawEllipse(370, 0, 20, 20, claBlack, claBlue);
      ARow.ShowAccessory := True;
      // set image to circle shape

      ARow.Image.ImageShape := ksCircleImage;
    end;
  finally
    lvSmoothScrolling.EndUpdate;
    HideLoading;
  end;
end;


// code to create the segment buttons...

procedure TForm6.BuildSegmentButtonListView;
var
  ICount: integer;
  AColor: TAlphaColor;
  ARow: TKsListItemRow;
begin
  if lvSegmentButtons.Items.Count > 0 then
    Exit;
  ShowLoading;
  lvSegmentButtons.BeginUpdate;
  try

    for ICount := 1 to 4 do
    begin
      AColor := claNull;
      case ICount mod 4 of
        0: AColor := claNull;
        1: AColor := claDimgray;
        2: AColor := claRed;
        3: AColor := claGreen;
      end;
      ARow := lvSegmentButtons.Items.AddRow('Item '+IntToStr(ICount), '', None);
      ARow.AddSegmentButtons(180, ['one', 'two', 'three']).TintColor := AColor;
    end;
  finally
    lvSegmentButtons.EndUpdate;
    HideLoading;
  end;
end;


procedure TForm6.BuildSwitchListView;
var
  ICount: integer;
begin
  if lvSwitches.Items.Count > 0 then
    Exit;
  ShowLoading;
  lvSwitches.BeginUpdate;
  try
    for ICount := 1 to 100 do
    begin
      with lvSwitches.Items.AddRow('Item '+IntToStr(ICount), '', None) do
        AddSwitchRight(0, False);
    end;
  finally
    lvSwitches.EndUpdate;
    HideLoading;
  end;
end;

procedure TForm6.BuildIndicatorListView;
begin
  if lvIndicators.Items.Count > 0 then
    Exit;
  ShowLoading;
  lvIndicators.BeginUpdate;
  try
    lvIndicators.Items.AddRow('Green', '', None).IndicatorColor := claGreen;
    lvIndicators.Items.AddRow('Yellow', '', None).IndicatorColor := claYellow;
    lvIndicators.Items.AddRow('Blue', '', None).IndicatorColor := claBlue;
    lvIndicators.Items.AddRow('Red', '', None).IndicatorColor := claRed;
    lvIndicators.Items.AddRow('Orange', '', None).IndicatorColor := claOrange;
    lvIndicators.Items.AddRow('Teal', '', None).IndicatorColor := claTeal;
    lvIndicators.Items.AddRow('Fuchsia', '', None).IndicatorColor := claFuchsia;
    lvIndicators.Items.AddRow('Silver', '', None).IndicatorColor := claSilver;
    lvIndicators.Items.AddRow('Gray', '', None).IndicatorColor := claGray;
    lvIndicators.Items.AddRow('Black', '', None).IndicatorColor := claBlack;
  finally
    lvIndicators.EndUpdate;
    HideLoading;
  end;
end;

procedure TForm6.BuildProgressBarsListview;
begin
  if lvProgressBars.Items.Count > 0 then
    Exit;
  ShowLoading;
  lvProgressBars.BeginUpdate;
  try
    lvProgressBars.Items.AddRow('Progress 1', 'green/round corner', '', More).DrawProgressBar(20, 0, 120, 18, 25, claGreenyellow, 9);
    lvProgressBars.Items.AddRow('Progress 2', 'red/round corner', '', More).DrawProgressBar(20, 0, 120, 18, 50, claIndianred, 9);
    lvProgressBars.Items.AddRow('Progress 3', 'blue/square corner', '', More).DrawProgressBar(20, 0, 120, 18, 75, claSkyblue, 0);
    lvProgressBars.Items.AddRow('Progress 4', 'orange/square corner', '', More).DrawProgressBar(20, 0, 120, 18, 90, claOrange, 0);
  finally
    lvProgressBars.EndUpdate;
    HideLoading;
  end;
end;

procedure TForm6.BuildAccessoryListView;
begin
  if lvAccessorys.Items.Count > 0 then
    Exit;
  ShowLoading;
  lvAccessorys.BeginUpdate;
  try
    lvAccessorys.Items.AddRow('No Accessory', '', None);
    lvAccessorys.Items.AddRow('"More" Accessory', '', More);
    lvAccessorys.Items.AddRow('"Checkmark" Accessory', '', Checkmark);
    lvAccessorys.Items.AddRow('"Detail" Accessory', '', Detail);
  finally
    lvAccessorys.EndUpdate;
    HideLoading;
  end;
end;

procedure Tform6.BuildCheckListView;
var
  ICount: integer;
begin
  if lvCheckList.Items.Count > 0 then
    Exit;
  ShowLoading;
  lvCheckList.BeginUpdate;
  for ICount := 1 to 50 do
    lvCheckList.Items.AddRow('Item '+IntToStr(ICount), '', '', None);
  HideLoading;
  lvCheckList.EndUpdate;
end;

procedure TForm6.DoShow;
begin
  inherited;
  Timer1.Enabled := True;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  TabControl1.TabPosition := TTabPosition.None;
  TabControl1.TabIndex := 0;
  SlideMenu1.AddMenuItem('LISTVIEW', 'Graphics & Text', nil);
  SlideMenu1.AddMenuItem('PROGRESS_BARS', 'Progress Bars', nil);
  SlideMenu1.AddMenuItem('SEGMENT_BUTTONS', 'Segment Buttons', nil);
  SlideMenu1.AddMenuItem('SWITCHES', 'Switches', nil);
  SlideMenu1.AddMenuItem('INDICATORS', 'Indicator Colours', nil);
  SlideMenu1.AddMenuItem('ACCESSORYS', 'Item Accessories', nil);
  SlideMenu1.AddMenuItem('CHECKLIST', 'Check List', nil);
  SlideMenu1.AddMenuItem('EVERYTHING', 'Wanna see smooth scrolling?', nil);
  SlideMenu1.ItemIndex := 0;

  SlideMenu2.AddMenuItem('ANOTHER', 'Dummy menu item', imgHome.Bitmap);

  SlideMenu2.ItemIndex := 0;
  layoutImages.Visible := False;

  ksSegmentButtons1.AddButton('DEFAULT', 'DEFAULT');
  ksSegmentButtons1.AddButton('RED', 'RED');
  ksSegmentButtons1.AddButton('GREEN', 'GREEN');
  ksSegmentButtons1.AddButton('BLUE', 'BLUE');
end;




procedure TForm6.HideLoading;
begin
  lblLoading.Visible := False;
end;

procedure TForm6.ksListView1ItemClick(Sender: TObject; x, y: Single;
  AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
begin
  //ShowMessage(AITem.Index.ToString);
  //if AITem.Index = 0 then
  //  ksListView1.HideItem(1);
end;

procedure TForm6.ksSegmentButtons1Change(Sender: TObject);
begin
  case ksSegmentButtons1.ItemIndex of
    0: lvCheckList.CheckMarkStyle := ksCmsDefault;
    1: lvCheckList.CheckMarkStyle := ksCmsRed;
    2: lvCheckList.CheckMarkStyle := ksCmsGreen;
    3: lvCheckList.CheckMarkStyle := ksCmsBlue;
  end;
end;

procedure TForm6.lvCheckListItemClick(Sender: TObject; x, y: Single;
  AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
begin
  Label3.Text := 'Checked count: '+IntToStr(lvCheckList.Items.CheckedCount);
end;

procedure TForm6.lvSegmentButtonsSegmentButtonClicked(Sender: TObject;
  AItem: TKsListItemRow; AButtons: TksListItemRowSegmentButtons;
  ARowID: string);
begin
  Label4.Text := ('Segment Click: row index: '+IntToStr(AItem.Index+1)+'   '+
              'button index: '+IntToStr(AButtons.ItemIndex)+#13+'   '+
              'text: '+AButtons.Captions[AButtons.ItemIndex]);

end;

procedure TForm6.lvSwitchesSwitchClick(Sender: TObject; AItem: TKsListItemRow;
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

procedure TForm6.ShowLoading;
begin
  lblLoading.Visible := True;
  lblLoading.BringToFront;
  Application.ProcessMessages;
end;

procedure TForm6.SlideMenu1AfterSlideOut(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TForm6.SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
begin
  if AId = 'LISTVIEW' then TabControl1.ActiveTab := tabListView;
  if AId = 'SEGMENT_BUTTONS' then TabControl1.ActiveTab := tabSegmentButtons;
  if AId = 'PROGRESS_BARS' then TabControl1.ActiveTab := tabProgressBars;
  if AId = 'SWITCHES' then TabControl1.ActiveTab := tabSwitches;
  if AId = 'INDICATORS' then TabControl1.ActiveTab := tabIndicators;
  if AId = 'ACCESSORYS' then TabControl1.ActiveTab := tabItemAccessorys;
  if AId = 'CHECKLIST' then TabControl1.ActiveTab := tabCheckList;
  if AId = 'EVERYTHING' then TabControl1.ActiveTab := tabEverything;
  Application.ProcessMessages;
end;

procedure TForm6.Switch1Switch(Sender: TObject);
begin
  case Switch1.IsChecked of
    True: lvCheckList.CheckMarks := TksListViewCheckMarks.ksCmMultiSelect;
    False: lvCheckList.CheckMarks := TksListViewCheckMarks.ksCmSingleSelect;
  end;
end;

procedure TForm6.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  case TabControl1.TabIndex of
    0: BuildTextItemsListView;
    1: BuildProgressBarsListview;
    2: BuildSegmentButtonListView;
    3: BuildSwitchListView;
    4: BuildIndicatorListView;
    5: BuildAccessoryListView;
    6: BuildCheckListView;
    7: BuildEverythingListView;
  end;
end;

end.
