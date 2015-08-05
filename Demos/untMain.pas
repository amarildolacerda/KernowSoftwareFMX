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
    procedure ksSegmentButtons1Change(Sender: TObject);
  private
    procedure BuildTextItemsListView;
    procedure BuildSegmentButtonListView;
    procedure BuildSwitchListView;
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
  BuildEverythingListView;
  //SlideMenu2.ToggleMenu;
end;


procedure TForm6.BuildTextItemsListView;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  ksListview1.Items.BeginUpdate;
  try
    for ICount := 1 to 1 do
    begin
      ARow := ksListview1.AddRow('Line '+InttoStr(ICount),            // main title
                                 'a sub title for '+IntToStr(ICount), // subtitle
                                 'detail text',                       // detail text
                                 More,                                // "more" accessory
                                 Image2.Bitmap);                      // image
      ARow.DrawEllipse(200, 0, 20, 20, claSilver, claSilver);
      // set image to circle shape
      ARow.Image.ImageShape := ksCircleImage;
    end;
  finally
    ksListview1.Items.EndUpdate;
  end;
end;

procedure TForm6.BuildEverythingListView;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
    lvSmoothScrolling.ClearItems;
  lvSmoothScrolling.Items.BeginUpdate;
  try
    for ICount := 1 to 20 do
    begin
      ARow := lvSmoothScrolling.AddRow('Line '+InttoStr(ICount),            // main title
                                 'sub title', // subtitle
                                 'detail',                       // detail text
                                 More,                                // "more" accessory
                                 Image2.Bitmap);                      // image
      ARow.AddSwitch(110, True);
      ARow.AddSegmentButtons(180, 120, ['one','two','three'], TListItemAlign.Leading);
      ARow.DrawBitmap(imgSearch.Bitmap, 310, 32, 32);
      ARow.DrawRect(360, 0, 20, 20, claBlack, claRed);
      //ARow.DrawRoundRect(390, 0, 20, 20, 5, claBlack, claGreen);
      //ARow.DrawEllipse(420, 0, 20, 20, claBlack, claBlue);
      ARow.ShowAccessory := True;
      // set image to circle shape

      ARow.Image.ImageShape := ksCircleImage;
    end;
  finally
    lvSmoothScrolling.Items.EndUpdate;
  end;
end;


// code to create the segment buttons...

procedure TForm6.BuildSegmentButtonListView;
var
  ICount: integer;
  AColor: TAlphaColor;
  ARow: TKsListItemRow;
begin
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
      ARow := lvSegmentButtons.AddRow('Item '+IntToStr(ICount), '', None);
      ARow.AddSegmentButtons(180, ['one', 'two', 'three']).TintColor := AColor;
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
      with lvSwitches.AddRow('Item '+IntToStr(ICount), '', None) do
        AddSwitchRight(0, False);
    end;
  finally
    lvSwitches.EndUpdate;
  end;
end;

procedure TForm6.BuildIndicatorListView;
begin
  lvIndicators.BeginUpdate;
  try
    lvIndicators.AddRow('Green', '', None).IndicatorColor := claGreen;
    lvIndicators.AddRow('Yellow', '', None).IndicatorColor := claYellow;
    lvIndicators.AddRow('Blue', '', None).IndicatorColor := claBlue;
    lvIndicators.AddRow('Red', '', None).IndicatorColor := claRed;
    lvIndicators.AddRow('Orange', '', None).IndicatorColor := claOrange;
    lvIndicators.AddRow('Teal', '', None).IndicatorColor := claTeal;
    lvIndicators.AddRow('Fuchsia', '', None).IndicatorColor := claFuchsia;
    lvIndicators.AddRow('Silver', '', None).IndicatorColor := claSilver;
    lvIndicators.AddRow('Gray', '', None).IndicatorColor := claGray;
    lvIndicators.AddRow('Black', '', None).IndicatorColor := claBlack;
  finally
    lvIndicators.EndUpdate;
  end;
end;

procedure TForm6.BuildAccessoryListView;
begin
  lvAccessorys.BeginUpdate;
  try
    lvAccessorys.AddRow('No Accessory', '', None);
    lvAccessorys.AddRow('"More" Accessory', '', More);
    lvAccessorys.AddRow('"Checkmark" Accessory', '', Checkmark);
    lvAccessorys.AddRow('"Detail" Accessory', '', Detail);
  finally
    lvAccessorys.EndUpdate;
  end;
end;

procedure Tform6.BuildCheckListView;
var
  ICount: integer;
begin
  lvCheckList.BeginUpdate;
  for ICount := 1 to 50 do
    lvCheckList.AddRow('Item '+IntToStr(ICount), '', '', None);

  lvCheckList.EndUpdate;
end;

procedure TForm6.DoShow;
begin
  inherited;
  BuildTextItemsListView;
  {BuildSegmentButtonListView;
  BuildEverythingListView;
  BuildSwitchListView;
  BuildIndicatorListView;
  BuildAccessoryListView;
  BuildCheckListView;   }
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




procedure TForm6.ksSegmentButtons1Change(Sender: TObject);
begin
  case ksSegmentButtons1.ItemIndex of
    0: lvCheckList.CheckMarkStyle := ksCmsDefault;
    1: lvCheckList.CheckMarkStyle := ksCmsRed;
    2: lvCheckList.CheckMarkStyle := ksCmsGreen;
    3: lvCheckList.CheckMarkStyle := ksCmsBlue;
  end;
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

end.
