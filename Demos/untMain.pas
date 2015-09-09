unit untMain;

interface

{$IFDEF VER290}
  {$DEFINE XE8_OR_NEWER}
{$ENDIF}

{$IFDEF VER300}
  {$DEFINE XE8_OR_NEWER}
  {$DEFINE XE10_OR_NEWER}
{$ENDIF}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, ksSlideMenu, FMX.Layouts, FMX.Objects,
  FMX.TabControl, FMX.ListBox, FMX.ListView.Types, FMX.ListView, ksListView,
  ksSegmentButtons, FMX.Memo
  {$IFDEF XE8_OR_NEWER} ,FMX.ScrollBox, FMX.Menus {$ENDIF}
  {$IFDEF XE10_OR_NEWER}, FMX.ListView.Appearances, FMX.ListView.Adapters.Base {$ENDIF}
  ;

type
  TfrmMain = class(TForm)
    ToolBar1: TToolBar;
    btnRightMenu: TButton;
    Label1: TLabel;
    imgHome: TImage;
    imgSearch: TImage;
    imgCalendar: TImage;
    imgMenu: TImage;
    imgContact: TImage;
    layoutImages: TLayout;
    imgAbout: TImage;
    TabControl1: TTabControl;
    tabListView: TTabItem;
    tabSegmentButtons: TTabItem;
    ksListView1: TksListView;
    lvSegmentButtons: TksListView;
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
    tabProgressBars: TTabItem;
    ToolBar9: TToolBar;
    Label9: TLabel;
    lvProgressBars: TksListView;
    lblLoading: TLabel;
    SlideMenu1: TksSlideMenu;
    btnLeftMenu: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    Memo5: TMemo;
    Memo6: TMemo;
    Memo7: TMemo;
    imgSource1: TImage;
    imgSource2: TImage;
    imgSource3: TImage;
    imgSource4: TImage;
    imgSource5: TImage;
    imgSource6: TImage;
    imgSource7: TImage;
    procedure FormCreate(Sender: TObject);
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
    procedure lvCheckListItemClick(Sender: TObject; x, y: Single;
      AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
    procedure lvSegmentButtonsSwitchClick(Sender: TObject;
      AItem: TKsListItemRow; ASwitch: TksListItemRowSwitch; ARowID: string);
    procedure btnRightMenuClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    procedure BuildTextItemsListView;
    procedure BuildProgressBarsListview;
    procedure BuildSegmentButtonListView;
    procedure BuildIndicatorListView;
    procedure BuildAccessoryListView;
    procedure BuildCheckListView;
    procedure BuildEverythingListView;
    { Private declarations }
  protected
    procedure DoShow; override;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses System.UIConsts, untSourceCode;

{$R *.fmx}

procedure TfrmMain.btnLeftMenuClick(Sender: TObject);
begin

  SlideMenu1.ToggleMenu;
end;

procedure TfrmMain.BuildTextItemsListView;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  if ksListView1.Items.Count > 0 then
    Exit;
  ksListview1.BeginUpdate;
  try
    for ICount := 1 to 100 do
    begin
      ARow := ksListview1.Items.AddRow('Line '+InttoStr(ICount),      // main title
                                 'a sub title for '+IntToStr(ICount), // subtitle
                                 'detail text',                       // detail text
                                 More,                                // "more" accessory
                                 Image2.Bitmap);                      // image



      // set image to circle shape
      ARow.Image.ImageShape := ksCircleImage;
    end;
  finally
    ksListview1.EndUpdate;
  end;
end;




procedure TfrmMain.Button1Click(Sender: TObject);
begin
  ShowMessage(BoolToStr(ksListView1.IsShowing, True)+#13+#13+
              BoolToStr(lvSegmentButtons.IsShowing, True));
end;

procedure TfrmMain.BuildEverythingListView;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  if lvSmoothScrolling.Items.Count > 0 then
    Exit;
  lvSmoothScrolling.BeginUpdate;
  try
    for ICount := 1 to 30 do
    begin
      ARow := lvSmoothScrolling.Items.AddRow('Line '+InttoStr(ICount),  // main title
                                             'sub title',               // subtitle
                                             '',                        // detail text
                                             More,                      // "more" accessory
                                             Image2.Bitmap);            // image
      ARow.AddSegmentButtons(100, 110, ['one','two','three'], TListItemAlign.Leading).OffsetY:= -16;
      ARow.DrawBitmap(imgSearch.Bitmap, 100, 16, 24, 24);

      ARow.AddSwitch(215, True,  TListItemAlign.Leading).OffsetY := -16;
      ARow.DrawProgressBar(140, 16, 130, 18, Round(ICount * 3.3), claIndianred, 0, TListItemAlign.Leading).OffsetY := 16;
      ARow.ShowAccessory := True;

      // set image to circle shape
      ARow.Image.ImageShape := ksCircleImage;
    end;
  finally
    lvSmoothScrolling.EndUpdate;
  end;
end;


// code to create the segment buttons...

procedure TfrmMain.BuildSegmentButtonListView;
var
  ICount: integer;
  AColor: TAlphaColor;
  ARow: TKsListItemRow;
begin
  if lvSegmentButtons.Items.Count > 0 then
    Exit;
  lvSegmentButtons.BeginUpdate;
  try

    for ICount := 1 to 10 do
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

     with lvSegmentButtons.Items.AddRow('Item '+IntToStr(ICount*2), '', None) do
      AddSwitchRight(0, False);
    end;


  finally
    lvSegmentButtons.EndUpdate;
  end;
end;




procedure TfrmMain.BuildIndicatorListView;
begin
  if lvIndicators.Items.Count > 0 then
    Exit;
  lvIndicators.BeginUpdate;
  try
    lvIndicators.Items.AddRow('Green', 'indicator color', 'some detail', More).IndicatorColor := claGreen;
    lvIndicators.Items.AddRow('Yellow', 'indicator color', 'some detail', More).IndicatorColor := claYellow;
    lvIndicators.Items.AddRow('Blue', 'indicator color', 'some detail', More).IndicatorColor := claBlue;
    lvIndicators.Items.AddRow('Red', 'indicator color', 'some detail', More).IndicatorColor := claRed;
    lvIndicators.Items.AddRow('Orange', 'indicator color', 'some detail', More).IndicatorColor := claOrange;
    lvIndicators.Items.AddRow('Teal', 'indicator color', 'some detail', More).IndicatorColor := claTeal;
    lvIndicators.Items.AddRow('Fuchsia', 'indicator color', 'some detail', More).IndicatorColor := claFuchsia;
    lvIndicators.Items.AddRow('Silver', 'indicator color', 'some detail', More).IndicatorColor := claSilver;
    lvIndicators.Items.AddRow('Gray', 'indicator color', 'some detail', More).IndicatorColor := claGray;
    lvIndicators.Items.AddRow('Black', 'indicator color', 'some detail', More).IndicatorColor := claBlack;
  finally
    lvIndicators.EndUpdate;
  end;
end;

procedure TfrmMain.BuildProgressBarsListview;
var
  ARow: TKsListItemRow;
begin
  if lvProgressBars.Items.Count > 0 then
    Exit;

  lvProgressBars.BeginUpdate;
  with lvProgressBars.Items do
  begin
    try
      // row 1
      ARow := AddRow('Progress 1', 'green/round corner', '', More);
      ARow.DrawProgressBar(0, 0, 120, 18, 25, claGreenyellow, 9);

      // row 2
      ARow := AddRow('Progress 2', 'red/round corner', '', More);
      ARow.DrawProgressBar(0, 0, 120, 18, 50, claIndianred, 9);

      // row 3
      ARow := AddRow('Progress 3', 'blue/square corner', '', More);
      ARow.DrawProgressBar(0, 0, 120, 18, 75, claSkyblue, 0);

      // row 4
      ARow := AddRow('Progress 4', 'orange/square corner', '', More);
      ARow.DrawProgressBar(0, 0, 120, 18, 90, claOrange, 0);
    finally
      lvProgressBars.EndUpdate;
    end;
  end;

end;

procedure TfrmMain.btnRightMenuClick(Sender: TObject);
begin
  case TabControl1.TabIndex of
    0: DisplaySourceCode(Memo1.Lines, imgSource1.Bitmap);
    1: DisplaySourceCode(Memo2.Lines, imgSource2.Bitmap);
    2: DisplaySourceCode(Memo3.Lines, imgSource3.Bitmap);
    3: DisplaySourceCode(Memo4.Lines, imgSource4.Bitmap);
    4: DisplaySourceCode(Memo5.Lines, imgSource5.Bitmap);
    5: DisplaySourceCode(Memo6.Lines, imgSource6.Bitmap);
    6: DisplaySourceCode(Memo7.Lines, imgSource7.Bitmap);
  end;
end;

procedure TfrmMain.BuildAccessoryListView;
begin
  if lvAccessorys.Items.Count > 0 then
    Exit;
  lvAccessorys.BeginUpdate;
  try
    lvAccessorys.Items.AddRow('No Accessory', '', None);
    lvAccessorys.Items.AddRow('"More" Accessory', '', More);
    lvAccessorys.Items.AddRow('"Checkmark" Accessory', '', Checkmark);
    lvAccessorys.Items.AddRow('"Detail" Accessory', '', Detail);
  finally
    lvAccessorys.EndUpdate;
  end;
end;

procedure TfrmMain.BuildCheckListView;
var
  ICount: integer;
begin
  if lvCheckList.Items.Count > 0 then
    Exit;
  lvCheckList.BeginUpdate;
  for ICount := 1 to 50 do
    lvCheckList.Items.AddRow('Item '+IntToStr(ICount), '', '', None);
  lvCheckList.EndUpdate;
end;

procedure TfrmMain.DoShow;
begin
  inherited;
  Timer1.Enabled := True;
  Memo1.Visible := False;
  Memo2.Visible := False;
  Memo3.Visible := False;
  Memo4.Visible := False;
  Memo5.Visible := False;
  Memo6.Visible := False;
  Memo7.Visible := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

  TabControl1.TabPosition := TTabPosition.None;
  TabControl1.TabIndex := 0;
  SlideMenu1.AddMenuItem('LISTVIEW', 'Graphics & Text', nil);
  SlideMenu1.AddMenuItem('PROGRESS_BARS', 'Progress Bars', nil);
  SlideMenu1.AddMenuItem('SEGMENT_SWITCHES', 'Buttons & Switches', nil);
  SlideMenu1.AddMenuItem('INDICATORS', 'Indicator Colours', nil);
  SlideMenu1.AddMenuItem('ACCESSORYS', 'Item Accessories', nil);
  SlideMenu1.AddMenuItem('CHECKLIST', 'Check List', nil);
  SlideMenu1.AddMenuItem('EVERYTHING', 'Complex but oh so smooth ;-)', nil);
  SlideMenu1.ItemIndex := 0;

  //SlideMenu2.AddMenuItem('ANOTHER', 'Dummy menu item', imgHome.Bitmap);

  //SlideMenu2.ItemIndex := 0;
  layoutImages.Visible := False;

  ksSegmentButtons1.AddButton('DEFAULT', 'DEFAULT');
  ksSegmentButtons1.AddButton('RED', 'RED');
  ksSegmentButtons1.AddButton('GREEN', 'GREEN');
  ksSegmentButtons1.AddButton('BLUE', 'BLUE');
end;

procedure TfrmMain.ksSegmentButtons1Change(Sender: TObject);
begin
  case ksSegmentButtons1.ItemIndex of
    0: lvCheckList.CheckMarkStyle := ksCmsDefault;
    1: lvCheckList.CheckMarkStyle := ksCmsRed;
    2: lvCheckList.CheckMarkStyle := ksCmsGreen;
    3: lvCheckList.CheckMarkStyle := ksCmsBlue;
  end;
end;

procedure TfrmMain.lvCheckListItemClick(Sender: TObject; x, y: Single;
  AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
begin
  Label3.Text := 'Checked count: '+IntToStr(lvCheckList.Items.CheckedCount);
end;

procedure TfrmMain.lvSegmentButtonsSegmentButtonClicked(Sender: TObject;
  AItem: TKsListItemRow; AButtons: TksListItemRowSegmentButtons;
  ARowID: string);
begin
  Label4.Text := ('Segment Click: row index: '+IntToStr(AItem.Index+1)+'   '+
              'button index: '+IntToStr(AButtons.ItemIndex)+#13+'   '+
              'text: '+AButtons.Captions[AButtons.ItemIndex]);

end;
  procedure TfrmMain.lvSegmentButtonsSwitchClick(Sender: TObject;
  AItem: TKsListItemRow; ASwitch: TksListItemRowSwitch; ARowID: string);
var
  ACheckedStr: string;
begin
  case ASwitch.IsChecked of
    True: ACheckedStr := '(Checked)';
    False: ACheckedStr := '(Unchecked)';
  end;
  Label4.Text := ('Switch clicked: row '+IntToStr(AItem.Index+1))+'   '+ACheckedStr;
end;

procedure TfrmMain.MenuItem1Click(Sender: TObject);
begin
  ShowMessage(inttostr(ksListView1.ItemIndex));
end;

procedure TfrmMain.SlideMenu1AfterSlideOut(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TfrmMain.SlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
begin
  if AId = 'LISTVIEW' then TabControl1.ActiveTab := tabListView;
  if AId = 'SEGMENT_SWITCHES' then TabControl1.ActiveTab := tabSegmentButtons;
  if AId = 'PROGRESS_BARS' then TabControl1.ActiveTab := tabProgressBars;
  if AId = 'INDICATORS' then TabControl1.ActiveTab := tabIndicators;
  if AId = 'ACCESSORYS' then TabControl1.ActiveTab := tabItemAccessorys;
  if AId = 'CHECKLIST' then TabControl1.ActiveTab := tabCheckList;
  if AId = 'EVERYTHING' then TabControl1.ActiveTab := tabEverything;
  Application.ProcessMessages;
end;

procedure TfrmMain.Switch1Switch(Sender: TObject);
begin
  case Switch1.IsChecked of
    True: lvCheckList.CheckMarks := TksListViewCheckMarks.ksCmMultiSelect;
    False: lvCheckList.CheckMarks := TksListViewCheckMarks.ksCmSingleSelect;
  end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  case TabControl1.TabIndex of
    0: BuildTextItemsListView;
    1: BuildProgressBarsListview;
    2: BuildSegmentButtonListView;
    3: BuildIndicatorListView;
    4: BuildAccessoryListView;
    5: BuildCheckListView;
    6: BuildEverythingListView;
  end;
  Invalidate;
end;

end.
