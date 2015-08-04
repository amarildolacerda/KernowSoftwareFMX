{ *******************************************************************************
  *                                                                              *
  *  TksListView - Cached ListView Component                                     *
  *                                                                              *
  *  https://github.com/gmurt/KernowSoftwareFMX                                  *
  *                                                                              *
  *  Copyright 2015 Graham Murt                                                  *
  *                                                                              *
  *  email: graham@kernow-software.co.uk                                         *
  *                                                                              *
  *  Licensed under the Apache License, Version 2.0 (the "License");             *
  *  you may not use this file except in compliance with the License.            *
  *  You may obtain a copy of the License at                                     *
  *                                                                              *
  *    http://www.apache.org/licenses/LICENSE-2.0                                *
  *                                                                              *
  *  Unless required by applicable law or agreed to in writing, software         *
  *  distributed under the License is distributed on an "AS IS" BASIS,           *
  *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
  *  See the License for the specific language governing permissions and         *
  *  limitations under the License.                                              *
  *                                                                              *
  *******************************************************************************}

unit ksListView;

interface

{$IFDEF VER290}
  {$DEFINE XE8_OR_NEWER}
{$ENDIF}


uses
  Classes, FMX.Types, FMX.Controls, FMX.ListView, Types, FMX.TextLayout,
  FMX.ListView.Types, FMX.Graphics, Generics.Collections, System.UITypes,
  {$IFDEF XE8_OR_NEWER} FMX.ImgList, {$ENDIF}
  System.UIConsts, FMX.StdCtrls, FMX.Styles.Objects;

const
  C_LONG_TAP_DURATION     = 5;  // 500 ms
  C_BUTTON_HEIGHT = 29;
  C_SEGMENT_BUTTON_HEIGHT = 29;
  C_DEFAULT_TEXT_COLOR = claBlack;
  C_DEFAULT_HEADER_TEXT_COLOR = claBlack;
  C_DEFAULT_SEGMENT_BUTTON_COLOR = claNull;

type
  TksListViewCheckMarks = (ksCmNone, ksCmSingleSelect, ksCmMultiSelect);
  TksListViewCheckStyle = (ksCmsDefault, ksCmsRed, ksCmsGreen, ksCmsBlue);
  TksListViewShape = (ksRectangle, ksRoundRect, ksEllipse);
  TksItemImageShape = (ksRectangleImage, ksRoundRectImage, ksCircleImage);
  TksAccessoryType = (None, More, Checkmark, Detail);
  TksImageButtonStyle = (Action, Add, Camara, Compose, Information, ArrowLeft,
    ArrowDown, ArrowRight, ArrowUp, Delete, Details, Organise, PageCurl, Pause,
    Play, Refresh, Reply, Search, Stop, Trash);

  TksListView = class;
  TKsListItemRow = class;
  TksListItemRowObj = class;
  TksListItemRowSwitch = class;
  TksListItemRowButton = class;
  TksListItemRowSegmentButtons = class;
  TksControlBitmapCache = class;

  TksListViewRowClickEvent = procedure(Sender: TObject; x, y: single; AItem: TListViewItem; AId: string; ARowObj: TksListItemRowObj) of object;
  TksListViewClickSwitchEvent = procedure(Sender: TObject; AItem: TListViewItem; ASwitch: TksListItemRowSwitch; ARowID: string) of object;
  TksListViewClickButtonEvent = procedure(Sender: TObject; AItem: TListViewItem; AButton: TksListItemRowButton; ARowID: string) of object;
  TksListViewClickSegmentButtonEvent = procedure(Sender: TObject; AItem: TListViewItem; AButtons: TksListItemRowSegmentButtons; ARowID: string) of object;
  TksListViewFinishScrollingEvent = procedure(Sender: TObject; ATopIndex, AVisibleItems: integer) of object;


  // ------------------------------------------------------------------------------

  TksVisibleItems = record
    Count: integer;
    IndexStart: integer;
    IndexEnd: integer;
  end;


  TksListItemRowObj = class
  private
    FId: string;
    FRect: TRectF;
    FPlaceOffset: TPointF;
    FRow: TKsListItemRow;
    FAlign: TListItemAlign;
    FVertAlignment: TListItemAlign;
    FTagBoolean: Boolean;
    FGuid: string;
    FControlImageCache: TksControlBitmapCache;

    procedure SetRect(const Value: TRectF);
    procedure SetID(const Value: string);
    procedure Changed;
    procedure SetAlign(const Value: TListItemAlign);
    procedure SetVertAlign(const Value: TListItemAlign);
  protected
    procedure CalculateRect(ARowBmp: TBitmap); virtual;
    procedure DoChanged(Sender: TObject);
  public
    constructor Create(ARow: TKsListItemRow); virtual;
    function Render(ACanvas: TCanvas): Boolean; virtual;
    procedure MouseDown; virtual;
    procedure MouseUp; virtual;
    procedure Click(x, y: single); virtual;
    property Rect: TRectF read FRect write SetRect;
    property ID: string read FId write SetID;
    property Align: TListItemAlign read FAlign write SetAlign default TListItemAlign.Leading;
    property VertAlign: TListItemAlign read FVertAlignment write SetVertAlign default TListItemAlign.Center;
    property PlaceOffset: TPointF read FPlaceOffset write FPlaceOffset;
    property TagBoolean: Boolean read FTagBoolean write FTagBoolean;
  end;




  TksListItemRowText = class(TksListItemRowObj)
  private
    FFont: TFont;
    FAlignment: TTextAlign;

    FTextColor: TAlphaColor;
    FText: string;
    FWordWrap: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetAlignment(const Value: TTextAlign);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetText(const Value: string);
    procedure SetWordWrap(const Value: Boolean);
  protected
    procedure CalculateRect(ARowBmp: TBitmap); override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property Font: TFont read FFont write SetFont;
    property TextAlignment: TTextAlign read FAlignment write SetAlignment;
    property TextColor: TAlphaColor read FTextColor write SetTextColor;
    property Text: string read FText write SetText;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  end;

  // ------------------------------------------------------------------------------

  TksListItemRowImage = class(TksListItemRowObj)
  private
    FBitmap: TBitmap;
    FImageShape: TksItemImageShape;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetImageShape(const Value: TksItemImageShape);
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property ImageShape: TksItemImageShape read FImageShape write SetImageShape default ksRectangleImage;
  end;

  TksListItemRowShape = class(TksListItemRowObj)
  private
    FStroke: TBrush;
    FFill: TBrush;
    FShape: TksListViewShape;
    FCornerRadius: single;
    procedure SetCornerRadius(const Value: single);
    procedure SetShape(const Value: TksListViewShape);
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property Stroke: TBrush read FStroke;
    property Fill: TBrush read FFill;
    property CornerRadius: single read FCornerRadius write SetCornerRadius;
    property Shape: TksListViewShape read FShape write SetShape;
  end;

  TKsListItemRowAccessory = class(TksListItemRowObj)
  private
    FResources: TListItemStyleResources;
    FAccessoryType: TAccessoryType;
    FImage: TStyleObject;
    procedure SetAccessoryType(const Value: TAccessoryType);
  public
    constructor Create(ARow: TKsListItemRow); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property AccessoryType: TAccessoryType read FAccessoryType write SetAccessoryType;
  end;

  TksListItemRowSwitch = class(TksListItemRowObj)
  private
    FIsChecked: Boolean;
    procedure SetIsChecked(const Value: Boolean);
  public
    function Render(ACanvas: TCanvas): Boolean; override;
    procedure Toggle;
    property IsChecked: Boolean read FIsChecked write SetIsChecked;

  end;

  // ------------------------------------------------------------------------------

  TksListItemRowButton = class(TksListItemRowObj)
  private
    FTintColor: TAlphaColor;
    FText: string;
    FStyleLookup: string;
    FIsDown: Boolean;
    procedure SetTintColor(const Value: TAlphaColor);
    procedure SetText(const Value: string);
    procedure SetStyleLookup(const Value: string);
  public
    constructor Create(ARow: TKsListItemRow); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    procedure MouseDown; override;
    procedure MouseUp; override;
    property StyleLookup: string read FStyleLookup write SetStyleLookup;
    property Text: string read FText write SetText;
    property TintColor: TAlphaColor read FTintColor write SetTintColor;
  end;
  // ------------------------------------------------------------------------------

  TksListItemRowSegmentButtons = class(TksListItemRowObj)
  private
    FCaptions: TStrings;
    FItemIndex: integer;
    FTintColor: TAlphaColor;
    procedure SetItemIndex(const Value: integer);
    procedure SetTintColor(const Value: TAlphaColor);
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    procedure Click(x, y: single); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Captions: TStrings read FCaptions;
    property TintColor: TAlphaColor read FTintColor write SetTintColor;
  end;

  // ------------------------------------------------------------------------------

  TKsSegmentButtonPosition = (ksSegmentLeft, ksSegmentMiddle, ksSegmentRight);

  TksControlBitmapCache = class
  private
    FGuid: string;
    FOwner: TksListView;
    FSwitchOn: TBitmap;
    FSwitchOff: TBitmap;
    FCreatingCache: Boolean;
    FCachedButtons: TStringList;
    FImagesCached: Boolean;
    FButton: TSpeedButton;
    function GetSwitchImage(AChecked: Boolean): TBitmap;
    function GetButtonImage(AWidth, AHeight: single; AText: string; ATintColor: TAlphaColor;
      ASelected: Boolean; AStyleLookup: string): TBitmap;
  public
    constructor Create(Owner: TksListView);
    destructor Destroy; override;
    function CreateImageCache: Boolean;
    property SwitchImage[AChecked: Boolean]: TBitmap read GetSwitchImage;
    property ButtonImage[AWidth, AHeight: single;
                         AText: string;
                         ATintColor: TAlphaColor;
                         ASelected: Boolean;
                         AStyleLookup: string]: TBitmap read GetButtonImage;
    property ImagesCached: Boolean read FImagesCached;
  end;

  TKsListItemRow = class(TListItemImage)
  private
    FTitle: TksListItemRowText;
    FSubTitle: TksListItemRowText;
    FDetail: TksListItemRowText;
    FImage: TksListItemRowImage;
    FAccessory: TKsListItemRowAccessory;
    FCached: Boolean;
    FFont: TFont;
    FTextColor: TAlphaColor;
    FIndicatorColor: TAlphaColor;
    FList: TObjectList<TksListItemRowObj>;
    FId: string;
    FShowAccessory: Boolean;
    FAutoCheck: Boolean;
    FImageIndex: integer;
    FCanSelect: Boolean;
    function TextHeight(AText: string): single;
    function TextWidth(AText: string): single;
    function RowHeight(const AScale: Boolean = True): single;
    function RowWidth(const AScale: Boolean = True): single;
    function GetListView: TCustomListView;
    function GetRowObject(AIndex: integer): TksListItemRowObj;
    function GetRowObjectCount: integer;
    procedure SetAccessory(const Value: TAccessoryType);
    procedure SetShowAccessory(const Value: Boolean);
    function GetAccessory: TAccessoryType;
    procedure SetAutoCheck(const Value: Boolean);
    //procedure SetChecked(const Value: Boolean);
    procedure SetImageIndex(const Value: integer);
    function GetSearchIndex: string;
    procedure SetSearchIndex(const Value: string);
    procedure SetIndicatorColor(const Value: TAlphaColor);
    procedure SetCanSelect(const Value: Boolean);
    property ListView: TCustomListView read GetListView;
    procedure DoOnListChanged(Sender: TObject; const Item: TksListItemRowObj;
      Action: TCollectionNotification);
    function ScreenWidth: single;
    procedure ProcessClick;
    procedure Changed;
    procedure ReleaseAllDownButtons;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    procedure CacheRow;
    procedure RealignStandardElements;
    // bitmap functions...
    function DrawBitmap(ABmp: TBitmap; x, AWidth, AHeight: single): TksListItemRowImage overload;
    {$IFDEF XE8_OR_NEWER}
    function DrawBitmap(ABmpIndex: integer; x, AWidth, AHeight: single): TksListItemRowImage overload;
    {$ENDIF}
    function DrawBitmap(ABmp: TBitmap; x, y, AWidth, AHeight: single): TksListItemRowImage overload;
    function DrawBitmapRight(ABmp: TBitmap; AWidth, AHeight, ARightPadding: single): TksListItemRowImage;
    // shape functions...
    function DrawRect(x, y, AWidth, AHeight: single; AStroke, AFill: TAlphaColor): TksListItemRowShape;
    function DrawRoundRect(x, y, AWidth, AHeight, ACornerRadius: single; AStroke, AFill: TAlphaColor): TksListItemRowShape;
    function DrawEllipse(x, y, AWidth, AHeight: single; AStroke, AFill: TAlphaColor): TksListItemRowShape;
    // switch
    function AddSwitch(x: single; AIsChecked: Boolean; const AAlign: TListItemAlign = TListItemAlign.Leading): TksListItemRowSwitch;
    function AddSwitchRight(AMargin: integer; AIsChecked: Boolean): TksListItemRowSwitch;
    // buttons...
    function AddButton(AWidth: integer; AText: string; const ATintColor: TAlphaColor = claNull): TksListItemRowButton; overload;
    function AddButton(AStyle: TksImageButtonStyle; const ATintColor: TAlphaColor = claNull): TksListItemRowButton; overload;
    function AddSegmentButtons(AWidth: integer;
                               ACaptions: array of string;
                               const AItemIndex: integer = -1;
                               const ATintColor: TAlphaColor = C_DEFAULT_SEGMENT_BUTTON_COLOR): TksListItemRowSegmentButtons; overload;
    // text functions...
    function TextOut(AText: string; x: single; const AVertAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, y, AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOutRight(AText: string; y, AWidth: single; AXOffset: single; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText; overload;
    // font functions...
    procedure SetFontProperties(AName: string; ASize: integer; AColor: TAlphaColor; AStyle: TFontStyles);
    // properties...
    property Title: TksListItemRowText read FTitle;
    property SubTitle: TksListItemRowText read FSubTitle;
    property Detail: TksListItemRowText read FDetail;
    property Font: TFont read FFont;
    property TextColor: TAlphaColor read FTextColor write FTextColor;
    property RowObject[AIndex: integer]: TksListItemRowObj read GetRowObject;
    property RowObjectCount: integer read GetRowObjectCount;
    property ID: string read FId write FId;
    property Cached: Boolean read FCached write FCached;
    property IndicatorColor: TAlphaColor read FIndicatorColor write SetIndicatorColor;
    property Accessory: TAccessoryType read GetAccessory write SetAccessory;
    property ShowAccessory: Boolean read FShowAccessory write SetShowAccessory default True;
    property AutoCheck: Boolean read FAutoCheck write SetAutoCheck default False;
    property Image: TksListItemRowImage read FImage write FImage;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property SearchIndex: string read GetSearchIndex write SetSearchIndex;
    property CanSelect: Boolean read FCanSelect write SetCanSelect default True;
  end;


  // ------------------------------------------------------------------------------

  TksListViewAppearence = class(TPersistent)
  private
    FListView: TksListView;
    FBackground: TAlphaColor;
    FItemBackground: TAlphaColor;
    FAlternatingItemBackground: TAlphaColor;
    procedure SetBackground(const Value: TAlphaColor);
    procedure SetItemBackground(const Value: TAlphaColor);
    procedure SetAlternatingItemBackground(const Value: TAlphaColor);
  public
    constructor Create(AListView: TksListView);
  published
    property Background: TAlphaColor read FBackground write SetBackground
      default claWhite;
    property ItemBackground: TAlphaColor read FItemBackground
      write SetItemBackground default claWhite;
    property AlternatingItemBackground: TAlphaColor
      read FAlternatingItemBackground write SetAlternatingItemBackground
      default claGainsboro;
  end;

  // ------------------------------------------------------------------------------

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidiOSDevice)]
  TksListView = class(TCustomListView)
  private
    FScreenScale: single;
    //FDefaultRowHeight: integer;
    FAppearence: TksListViewAppearence;
    FOnItemClickEx: TksListViewRowClickEvent;
    FOnItemRightClickEx: TksListViewRowClickEvent;
    FMouseDownPos: TPointF;
    //FDetailFont: TFont;
    FCurrentMousepos: TPointF;
    FItemHeight: integer;
    FClickTimer: TTimer;
    FLastWidth: integer;
    FMouseDownDuration: integer;
    FOnLongClick: TksListViewRowClickEvent;
    FClickedRowObj: TksListItemRowObj;
    FClickedItem: TListViewItem;
    FSelectOnRightClick: Boolean;
    FOnSwitchClicked: TksListViewClickSwitchEvent;
    FOnButtonClicked: TksListViewClickButtonEvent;
    FOnSegmentButtonClicked: TksListViewClickSegmentButtonEvent;
    FCacheTimer: TTimer;
    FScrollTimer: TTimer;
    FLastScrollPos: integer;
    FScrolling: Boolean;
    FOnFinishScrolling: TksListViewFinishScrollingEvent;
    FControlBitmapCache: TksControlBitmapCache;
    FCheckMarks: TksListViewCheckMarks;
    FCheckMarkStyle: TksListViewCheckStyle;
    FUpdateCount: integer;
    FItemImageSize: integer;
    procedure SetItemHeight(const Value: integer);
    procedure DoClickTimer(Sender: TObject);
    function GetCachedRow(index: integer): TKsListItemRow;
    procedure OnCacheTimer(Sender: TObject);
    procedure DoScrollTimer(Sender: TObject);
    //function CountUncachedRows: integer;
    procedure SetCheckMarks(const Value: TksListViewCheckMarks);
    //function GetIsUpdating: Boolean;
    function RowObjectAtPoint(ARow: TKsListItemRow; x, y: single): TksListItemRowObj;
    procedure ReleaseAllDownButtons;
    procedure SetCheckMarkStyle(const Value: TksListViewCheckStyle);
    procedure SetItemImageSize(const Value: integer);
    { Private declarations }
  protected
    procedure SetColorStyle(AName: string; AColor: TAlphaColor);
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoItemClick(const AItem: TListViewItem); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure DoItemChange(const AItem: TListViewItem); override;
    procedure Paint; override;
    
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RedrawAllRows;
    function AddRow(AText, ADetail: string;
                    AAccessory: TksAccessoryType;
                    const AImageIndex: integer = -1;
                    const AFontSize: integer = 14;
                    AFontColor: TAlphaColor = C_DEFAULT_TEXT_COLOR): TKsListItemRow; overload;
    function AddRow(AText, ASubTitle, ADetail: string;
                    AAccessory: TksAccessoryType;
                    const AImageIndex: integer = -1;
                    const AFontSize: integer = 14;
                    AFontColor: TAlphaColor = C_DEFAULT_TEXT_COLOR): TKsListItemRow; overload;
    function AddRow(AText, ASubTitle, ADetail: string;
                    AAccessory: TksAccessoryType;
                    AImage: TBitmap;
                    const AFontSize: integer = 14;
                    AFontColor: TAlphaColor = C_DEFAULT_TEXT_COLOR): TKsListItemRow; overload;
    function AddHeader(AText: string): TKsListItemRow;
    function ItemsInView: TksVisibleItems;
    procedure BeginUpdate; {$IFDEF XE8_OR_NEWER} override; {$ENDIF}
    procedure EndUpdate; {$IFDEF XE8_OR_NEWER} override; {$ENDIF}
    property CachedRow[index: integer]: TKsListItemRow read GetCachedRow;
    procedure UncheckAll;
    procedure CheckAll;
    { Public declarations }
  published
    property Appearence: TksListViewAppearence read FAppearence
      write FAppearence;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 44;
    property ItemImageSize: integer read FItemImageSize write SetItemImageSize default 32;
    property OnEditModeChange;
    property OnEditModeChanging;
    property EditMode;
    property Transparent default False;
    property AllowSelection;
    property AlternatingColors;
    property ItemIndex;
    {$IFDEF XE8_OR_NEWER}
    property Images;
    {$ENDIF}
    property ScrollViewPos;
    property ItemSpaces;
    property SideSpace;
    property OnItemClickEx: TksListViewRowClickEvent read FOnItemClickEx write FOnItemClickEx;
    property OnItemClickRightEx: TksListViewRowClickEvent read FOnItemRightClickEx write FOnItemRightClickEx;
    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property CheckMarks: TksListViewCheckMarks read FCheckMarks write SetCheckMarks default ksCmNone;
    property CheckMarkStyle: TksListViewCheckStyle read FCheckMarkStyle write SetCheckMarkStyle default ksCmsDefault;
    property ClipChildren default True;
    property ClipParent default False;
    property Cursor default crDefault;
    property DisableFocusEffect default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property SelectOnRightClick: Boolean read FSelectOnRightClick write FSelectOnRightClick default False;
    property Size;
    property TabOrder;
    property TabStop;
    property Visible default True;
    property Width;
    { events }
    property OnApplyStyleLookup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;

    property HelpContext;
    property HelpKeyword;
    property HelpType;

    property StyleLookup;
    property TouchTargetExpansion;

    property OnDblClick;

    { ListView selection events }
    property CanSwipeDelete;

    property OnChange;
    property OnChangeRepainted;
    {$IFDEF XE8_OR_NEWER}
    property OnItemsChange;
    property OnScrollViewChange;
    property OnFilter;
    property PullRefreshWait;
    {$ENDIF}
    property OnItemClick;

    property OnDeletingItem;
    property OnDeleteItem;
    property OnDeleteChangeVisible;
    property OnSearchChange;
    property OnPullRefresh;
    property DeleteButtonText;

    property AutoTapScroll;
    property AutoTapTreshold;
    property ShowSelection;
    property DisableMouseWheel;

    property SearchVisible;
    property SearchAlwaysOnTop;
    property SelectionCrossfade;
    property PullToRefresh;
    property OnLongClick: TksListViewRowClickEvent read FOnLongClick write FOnLongClick;
    property OnSwitchClick: TksListViewClickSwitchEvent read FOnSwitchClicked write FOnSwitchClicked;
    property OnButtonClicked: TksListViewClickButtonEvent read FOnButtonClicked write FOnButtonClicked;
    property OnSegmentButtonClicked: TksListViewClickSegmentButtonEvent read FOnSegmentButtonClicked write FOnSegmentButtonClicked;
    property OnScrollFinish: TksListViewFinishScrollingEvent read FOnFinishScrolling write FOnFinishScrolling;
  end;

procedure Register;

implementation

uses SysUtils, FMX.Platform, FMX.Forms, FMX.SearchBox, FMX.Objects;

const
{$IFDEF IOS}
  DefaultScrollBarWidth = 7;
{$ELSE}
{$IFDEF MACOS}
  DefaultScrollBarWidth = 7;
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  DefaultScrollBarWidth = 16;
{$ENDIF}

{$IFDEF ANDROID}
  DefaultScrollBarWidth = 7;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('kernow Software FMX', [TksListView]);
end;


// ------------------------------------------------------------------------------

function GetScreenScale: single;
var
  Service: IFMXScreenService;
begin
  Service := IFMXScreenService(TPlatformServices.Current.GetPlatformService
    (IFMXScreenService));
  Result := Service.GetScreenScale;
end;

function IsBlankBitmap(ABmp: TBitmap): Boolean;
var
  ABlank: TBitmap;
begin
  ABlank := TBitmap.Create(ABmp.Width, ABmp.Height);
  try
    ABlank.Clear(claNull);
    Result := ABmp.EqualsBitmap(ABlank);
  finally
    {$IFDEF IOS}
    ABlank.DisposeOf;
    {$ELSE}
    ABlank.Free;
    {$ENDIF}
  end;
end;

function CreateGuidStr: string;
var
  AGuid: TGUID;
  AStr: string;
  ICount: integer;
begin
  Result := '';
  CreateGUID(AGuid);
  AStr := GUIDToString(AGuid);
  for ICount := 1 to Length(AStr) do
  begin
    if CharInSet(AStr[ICount], ['A'..'Z','0'..'9']) then
      Result := Result + UpCase(AStr[ICount]);
  end;
end;

// ------------------------------------------------------------------------------

{ TksListItemRowObj }

procedure TksListItemRowObj.CalculateRect(ARowBmp: TBitmap);
var
  w,h: single;
  ABmpWidth: single;
begin
  w := FRect.Width;
  h := FRect.Height;
  
  ABmpWidth := ARowBmp.Width / GetScreenScale;

  FRect := RectF(0, 0, w, h);
  if FAlign = TListItemAlign.Leading then
    OffsetRect(FRect, FPlaceOffset.X, 0);

  if FAlign = TListItemAlign.Trailing then
    OffsetRect(FRect, ABmpWidth - (FRect.Width+ DefaultScrollBarWidth + FPlaceOffset.X {+ FRow.ListView.ItemSpaces.Right}), 0);

  case VertAlign of
    TListItemAlign.Center: OffsetRect(FRect, 0, (FRow.Owner.Height - FRect.Height) / 2);
    TListItemAlign.Trailing: OffsetRect(FRect, 0, (FRow.Owner.Height - FRect.Height));
  end;

  OffsetRect(FRect, 0, FPlaceOffset.Y);
end;

procedure TksListItemRowObj.Changed;
begin
  FRow.Cached := False;
end;

procedure TksListItemRowObj.Click(x, y: single);
begin
  // overridden in descendant classes.
end;

constructor TksListItemRowObj.Create(ARow: TKsListItemRow);
var
  AGuid: TGUID;
begin
  inherited Create;
  FRow := ARow;
  FControlImageCache := TksListView(ARow.ListView).FControlBitmapCache;
  FAlign := TListItemAlign.Leading;
  FPlaceOffset := PointF(0,0);
  FTagBoolean := False;
  CreateGUID(AGuid);
  FGuid := GUIDToString(AGuid);
end;

procedure TksListItemRowObj.DoChanged(Sender: TObject);
begin
  Changed;
end;

procedure TksListItemRowObj.MouseDown;
begin
  // overridden in descendant classes
end;

procedure TksListItemRowObj.MouseUp;
begin
  // overridden in descendant classes
end;

function TksListItemRowObj.Render(ACanvas: TCanvas): Boolean;
begin
  Result := True;
end;

procedure TksListItemRowObj.SetAlign(const Value: TListItemAlign);
begin
  FAlign := Value;
  Changed;
end;

procedure TksListItemRowObj.SetID(const Value: string);
begin
  FId := Value;
  Changed;
end;

procedure TksListItemRowObj.SetRect(const Value: TRectF);
begin
  FRect := Value;
  Changed;
end;

procedure TksListItemRowObj.SetVertAlign(const Value: TListItemAlign);
begin
  FVertAlignment := Value;
  Changed;
end;

// ------------------------------------------------------------------------------

{ TksListItemRowText }

procedure TksListItemRowText.CalculateRect(ARowBmp: TBitmap);
var
  ASaveFont: TFont;
begin
  if (FRect.Width = 0) or (FRect.Height = 0) then
  begin
    ASaveFont := TFont.Create;
    try
      ASaveFont.Assign(ARowBmp.Canvas.Font);
      ARowBmp.Canvas.Font.Assign(FFont);
      if FRect.Width = 0 then FRect.Width := ARowBmp.Canvas.TextWidth(FText);
      if FRect.Height = 0 then FRect.Height := ARowBmp.Canvas.TextHeight(FText);
      ARowBmp.Canvas.Font.Assign(ASaveFont);
    finally
      {$IFDEF IOS}
      ASaveFont.DisposeOf;
      {$ELSE}
      ASaveFont.Free;
      {$ENDIF}
    end;
  end;
  inherited;
end;

constructor TksListItemRowText.Create(ARow: TKsListItemRow);
begin
  inherited Create(ARow);
  FFont := TFont.Create;
  FTextColor := C_DEFAULT_TEXT_COLOR;
  FWordWrap := False;
  VertAlign := TListItemAlign.Center;
end;

destructor TksListItemRowText.Destroy;
begin
  {$IFDEF IOS}
  FFont.DisposeOf;
  {$ELSE}
  FFont.Free;
  {$ENDIF}
  inherited;
end;

function TksListItemRowText.Render(ACanvas: TCanvas): Boolean;
begin
  inherited Render(ACanvas);
  ACanvas.Fill.Color := FTextColor;
  ACanvas.Font.Assign(FFont);
  ACanvas.FillText(FRect, FText, FWordWrap, 1, [], FAlignment);
  Result := True;
end;

procedure TksListItemRowText.SetAlignment(const Value: TTextAlign);
begin
  FAlignment := Value;
  Changed;
end;

procedure TksListItemRowText.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TksListItemRowText.SetText(const Value: string);
begin
  FText := Value;
  Changed;
end;

procedure TksListItemRowText.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
  Changed;
end;

procedure TksListItemRowText.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  Changed;
end;

// ------------------------------------------------------------------------------

{ TksListItemRowImage }

constructor TksListItemRowImage.Create(ARow: TKsListItemRow);
begin
  inherited Create(ARow);
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := DoChanged;
  FVertAlignment := TListItemAlign.Center;
  FImageShape := ksRectangleImage;
end;

destructor TksListItemRowImage.Destroy;
begin
  {$IFDEF IOS}
  FBitmap.DisposeOf;
  {$ELSE}
  FBitmap.Free;
  {$ENDIF}
  inherited;
end;

function TksListItemRowImage.Render(ACanvas: TCanvas): Boolean;
var
  ABmp: TBitmap;
  ARectangle: TRectangle;
begin
  Result := inherited Render(ACanvas);
  ARectangle := TRectangle.Create(FRow.ListView.Parent);
  try
    ARectangle.Width := FBitmap.Width;
    ARectangle.Height := FBitmap.Height;
    ARectangle.Stroke.Kind := TBrushKind.None;
    ARectangle.Fill.Bitmap.Bitmap.Assign(FBitmap);

    ARectangle.Fill.Kind := TBrushKind.Bitmap;

    if FImageShape = ksRoundRectImage then
    begin
      ARectangle.XRadius := 16;
      ARectangle.YRadius := 16;
    end;

    if FImageShape = ksCircleImage then
    begin
      ARectangle.XRadius := 32;
      ARectangle.YRadius := 32;
    end;

    ABmp := TBitmap.Create(128, 128);
    try
      ABmp.Clear(claNull);
      ABmp.Canvas.BeginScene;
      ARectangle.PaintTo(ABmp.Canvas, RectF(0, 0, ABmp.Width, ABmp.Height), nil);
      ABmp.Canvas.EndScene;
      ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), FRect, 1);
    finally
      {$IFDEF IOS}
      ABmp.DisposeOf;
      {$ELSE}
      ABmp.Free;
      {$ENDIF}
    end;
  finally
    ARectangle.Free;
  end;
end;

procedure TksListItemRowImage.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  FBitmap.BitmapScale := GetScreenScale;
  Changed;
end;

procedure TksListItemRowImage.SetImageShape(const Value: TksItemImageShape);
begin
  if FImageShape <> Value then
  begin
    FImageShape := Value;
    Changed;
  end;
end;

// ------------------------------------------------------------------------------

{ TksListItemRowShape }

constructor TksListItemRowShape.Create(ARow: TKsListItemRow);
begin
  inherited;
  FStroke := TBrush.Create(TBrushKind.Solid, claBlack);
  FFill := TBrush.Create(TBrushKind.Solid, claNull);
  FShape := ksRectangle;
end;

destructor TksListItemRowShape.Destroy;
begin
  {$IFDEF IOS}
  FFill.DisposeOf;
  FStroke.DisposeOf;
  {$ELSE}
  FFill.Free;
  FStroke.Free;
  {$ENDIF}
  inherited;
end;

function TksListItemRowShape.Render(ACanvas: TCanvas): Boolean;
var
  ARect: TRectF;
  ACorners: TCorners;
begin
  Result := inherited Render(ACanvas);
  ARect := FRect;
  ACorners := [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight];
  ACanvas.Fill.Assign(FFill);

  if FShape = ksEllipse then
    ACanvas.FillEllipse(ARect, 1)
  else
    ACanvas.FillRect(ARect, FCornerRadius, FCornerRadius, ACorners, 1);
  ACanvas.Fill.Assign(FStroke);

  if FShape = ksEllipse then
    ACanvas.DrawEllipse(ARect, 1)
  else
    ACanvas.DrawRect(ARect, FCornerRadius, FCornerRadius, ACorners, 1);
end;

procedure TksListItemRowShape.SetCornerRadius(const Value: single);
begin
  FCornerRadius := Value;
  Changed;
end;

procedure TksListItemRowShape.SetShape(const Value: TksListViewShape);
begin
  FShape := Value;
  Changed;
end;

// ------------------------------------------------------------------------------

{ TksListItemRow }

procedure TKsListItemRow.CacheRow;
var
  ICount: integer;
  AMargins: TBounds;
  ABmpWidth: integer;
  AImage: TBitmap;
  ASize: TSizeF;
begin
  if FCached then
    Exit;
  AMargins := (Owner.Parent as TCustomListView).ItemSpaces;
  BeginUpdate;
  try
    ABmpWidth := Round(RowWidth) - Round((AMargins.Left + AMargins.Right) * GetScreenScale);
    Bitmap.Height := Round(RowHeight);
    Bitmap.Width := ABmpWidth;
    Bitmap.Clear(claNull);
    Bitmap.Canvas.BeginScene;

    if FIndicatorColor <> claNull then
    begin
      Bitmap.Canvas.Fill.Color := FIndicatorColor;
      Bitmap.Canvas.FillRect(RectF(0, 8, 6, RowHeight(False)-8), 0, 0, [], 1, Bitmap.Canvas.Fill);
    end;

    {$IFDEF XE8_OR_NEWER}
    if FImageIndex > -1 then
    begin
      ASize.cx := 32;
      ASize.cy := 32;
      AImage := ListView.Images.Bitmap(ASize, FImageIndex);
      DrawBitmap(AImage, 0, ASize.cx, ASize.cy);
    end;
    {$ENDIF}

    if FAutoCheck then
    begin
      FAccessory.AccessoryType := TAccessoryType.Checkmark;
      if (Owner as TListViewItem).Checked then
      begin
        FAccessory.CalculateRect(Bitmap);
        FAccessory.Render(Bitmap.Canvas);
      end;
    end
    else
    begin
      if FShowAccessory then
      begin
        FAccessory.CalculateRect(Bitmap);
        FAccessory.Render(Bitmap.Canvas);
      end;
    end;

    FImage.CalculateRect(Bitmap);
    FImage.Render(Bitmap.Canvas);

    FTitle.CalculateRect(Bitmap);
    FTitle.Render(Bitmap.Canvas);

    FSubTitle.CalculateRect(Bitmap);
    FSubTitle.Render(Bitmap.Canvas);

    FDetail.CalculateRect(Bitmap);
    FDetail.Render(Bitmap.Canvas);

    for ICount := 0 to FList.Count - 1 do
    begin
      FList[ICount].CalculateRect(Bitmap);
      if FList[ICount].Render(Bitmap.Canvas) = False then
      begin
        FCached := False;
        Bitmap.Canvas.EndScene;
        Bitmap.Clear(claNull);
        Exit;
      end;
    end;
    Bitmap.Canvas.EndScene;
    FCached := True;
  finally
    EndUpdate;
  end;
end;

procedure TKsListItemRow.Changed;
begin
  FCached := False;
  if not ListView.IsUpdating then
  begin
    CacheRow;
    ListView.Repaint;
  end;
end;

procedure TKsListItemRow.RealignStandardElements;
var
  AOffset: single;
begin
  if FSubTitle.Text <> '' then
  begin
    FTitle.PlaceOffset := PointF(0, -9);
    FSubTitle.PlaceOffset := PointF(0,9);
  end;
  AOffset := 0;
  if FIndicatorColor <> claNull then AOffset := 16;
  if FImage.Bitmap.Width > 0 then AOffset := FImage.Rect.Width+8;
  begin
    FTitle.PlaceOffset := PointF(AOffset, FTitle.PlaceOffset.Y);
    FSubTitle.PlaceOffset := PointF(AOffset, FSubTitle.PlaceOffset.Y);
  end;
  if ShowAccessory then
    FDetail.PlaceOffset := PointF(FDetail.PlaceOffset.X+24, FDetail.PlaceOffset.Y);
end;

procedure TKsListItemRow.ReleaseAllDownButtons;
var
  ICount: integer;
begin
  for ICount := 0 to FList.Count-1 do
  begin
    if (FList[ICount] is TksListItemRowButton) then
    begin
      (FList[ICount] as TksListItemRowButton).FIsDown := False;
      Changed;
    end;
  end;
end;

constructor TKsListItemRow.Create(const AOwner: TListItem);
var
  ABmp: TBitmap;
  lv: TksListView;
begin
  inherited Create(AOwner);
  lv := (ListView as TksListView);
  FImage := TksListItemRowImage.Create(Self);// TBitmap.Create;
  FAccessory := TKsListItemRowAccessory.Create(Self);
  FTitle := TksListItemRowText.Create(Self);
  FSubTitle := TksListItemRowText.Create(Self);
  FDetail := TksListItemRowText.Create(Self);

  {$IFDEF MSWINDOWS}
  ScalingMode := TImageScalingMode.Original;
  {$ENDIF}
  PlaceOffset.X := 0;
  FIndicatorColor := claNull;
  OwnsBitmap := True;
  FList := TObjectList<TksListItemRowObj>.Create(True);
  FList.OnNotify := DoOnListChanged;
  FImage.Rect := RectF(0, 0, lv.ItemImageSize, lv.ItemImageSize);
  ABmp := TBitmap.Create;
  ABmp.BitmapScale := GetScreenScale;
  ABmp.Width := Round(RowWidth);
  ABmp.Height := Round(RowHeight);
  ABmp.Clear(claNull);
  Bitmap := ABmp;
  FTextColor := C_DEFAULT_TEXT_COLOR;
  FFont := TFont.Create;
  FCached := False;
  FShowAccessory := True;
  FAutoCheck := False;
  FImageIndex := -1;
  FCanSelect := True;
  FDetail.Align := TListItemAlign.Trailing;

  FSubTitle.TextColor := claGray;
  FSubTitle.Font.Size := 12;
  FDetail.TextColor := claDodgerblue;
  FDetail.Font.Size := 12;
end;

destructor TKsListItemRow.Destroy;
begin
  {$IFDEF IOS}
  FList.DisposeOf;
  FFont.DisposeOf;
  FAccessory.DisposeOf;
  FImage.DisposeOf;
  FTitle.DisposeOf;
  FSubTitle.DisposeOf;
  FDetail.DisposeOf;
  {$ELSE}
  FList.Free;
  FFont.Free;
  FAccessory.Free;
  FImage.Free;
  FTitle.Free;
  FSubTitle.Free;
  FDetail.Free;
  {$ENDIF}
  inherited;
end;

function TKsListItemRow.ScreenWidth: single;
begin
  Result := TksListView(Owner.Parent).Width;
{$IFDEF MSWINDOWS}
  Result := Result - 40;
{$ENDIF}
end;

function TKsListItemRow.TextHeight(AText: string): single;
begin
  Bitmap.Canvas.Font.Assign(FFont);
  Result := Bitmap.Canvas.TextHeight(AText);
end;

function TKsListItemRow.TextWidth(AText: string): single;
begin
  Bitmap.Canvas.Font.Assign(FFont);
  Result := Bitmap.Canvas.TextWidth(AText);
end;

function TKsListItemRow.RowHeight(const AScale: Boolean = True): single;
var
  lv: TksListView;
begin
  lv := TksListView(Owner.Parent);
  Result := lv.ItemHeight;
  if AScale then
    Result := Result * GetScreenScale;
end;

function TKsListItemRow.RowWidth(const AScale: Boolean = True): single;
var
  lv: TksListView;
begin
  lv := TksListView(Owner.Parent);
  Result := lv.Width;
  if AScale then
    Result := Result * GetScreenScale;
end;

function TKsListItemRow.GetAccessory: TAccessoryType;
begin
  Result := FAccessory.AccessoryType;
end;

function TKsListItemRow.GetListView: TCustomListView;
begin
  Result := (Owner.Parent as TCustomListView);
end;

function TKsListItemRow.GetRowObject(AIndex: integer): TksListItemRowObj;
begin
  Result := FList[AIndex];
end;

function TKsListItemRow.GetRowObjectCount: integer;
begin
  Result := FList.Count;
end;

function TKsListItemRow.GetSearchIndex: string;
begin
  Result := TListViewItem(Owner).Text;
end;



procedure TKsListItemRow.ProcessClick;
begin
  if FAutoCheck then
  begin
    Accessory := TAccessoryType.Checkmark;
    (Owner as TListViewItem).Checked := not (Owner as TListViewItem).Checked;
    // prevent deselecting if single select...
    if TksListView(ListView).CheckMarks = TksListViewCheckMarks.ksCmSingleSelect then
      (Owner as TListViewItem).Checked := True;
    FCached := False;
    CacheRow;
  end;
end;

procedure TKsListItemRow.DoOnListChanged(Sender: TObject;
  const Item: TksListItemRowObj; Action: TCollectionNotification);
begin
  FCached := False;
end;

// ------------------------------------------------------------------------------

// bitmap drawing functions...

function TKsListItemRow.DrawBitmap(ABmp: TBitmap; x, AWidth, AHeight: single): TksListItemRowImage;
begin
  Result := DrawBitmap(ABmp, x, 0, AWidth, AHeight);
end;

{$IFDEF XE8_OR_NEWER}

function TKsListItemRow.DrawBitmap(ABmpIndex: integer;
  x, AWidth, AHeight: single): TksListItemRowImage overload;
var
  ABmp: TBitmap;
  il: TCustomImageList;
  ASize: TSizeF;
begin
  Result := nil;
  il := ListView.Images;
  if il = nil then
    Exit;
  ASize.cx := 64;
  ASize.cy := 64;
  ABmp := il.Bitmap(ASize, ABmpIndex);
  Result := DrawBitmap(ABmp, x, AWidth, AHeight);
end;

{$ENDIF}

function TKsListItemRow.DrawBitmap(ABmp: TBitmap; x, y, AWidth, AHeight: single): TksListItemRowImage;
begin
  Result := TksListItemRowImage.Create(Self);
  Result.FRect := RectF(0, 0, AWidth, AHeight);
  Result.PlaceOffset := PointF(x,y);
  Result.VertAlign := TListItemAlign.Center;
  Result.Bitmap := ABmp;
  FList.Add(Result);
end;

function TKsListItemRow.DrawBitmapRight(ABmp: TBitmap;
  AWidth, AHeight, ARightPadding: single): TksListItemRowImage;
var
  AYpos: single;
  AXPos: single;
begin
  AYpos := (RowHeight(False) - AHeight) / 2;
  AXPos := ScreenWidth - (AWidth + ARightPadding);
  Result := DrawBitmap(ABmp, AXPos, AYpos, AWidth, AHeight);
end;

function TKsListItemRow.DrawRect(x, y, AWidth, AHeight: single; AStroke,
  AFill: TAlphaColor): TksListItemRowShape;
begin
  Result := TksListItemRowShape.Create(Self);
  Result.FRect := RectF(0, 0, AWidth, AHeight);
  Result.PlaceOffset := PointF(x,y);
  Result.Stroke.Color := AStroke;
  Result.Fill.Color := AFill;
  Result.VertAlign := TListItemAlign.Center;
  FList.Add(Result);
end;

function TKsListItemRow.DrawRoundRect(x, y, AWidth, AHeight,
  ACornerRadius: single; AStroke, AFill: TAlphaColor): TksListItemRowShape;
begin
  Result := DrawRect(x, y, AWidth, AHeight, AStroke, AFill);
  Result.CornerRadius := ACornerRadius;
end;

function TKsListItemRow.DrawEllipse(x, y, AWidth, AHeight: single; AStroke,
  AFill: TAlphaColor): TksListItemRowShape;
begin
  Result := DrawRect(x, y, AWidth, AHeight, AStroke, AFill);
  Result.Shape := ksEllipse;
end;

function TKsListItemRow.AddButton(AStyle: TksImageButtonStyle; const ATintColor: TAlphaColor = claNull): TksListItemRowButton;
var
  AStr: string;
begin
  Result := AddButton(44, '', ATintColor);
  Result.Rect := RectF(0, 0, 44, 44);
  case AStyle of
    Action: AStr := 'actiontoolbuttonbordered';
    Add: AStr := 'addtoolbuttonbordered';
    Camara: AStr := 'cameratoolbuttonbordered';
    Compose: AStr := 'composetoolbuttonbordered';
    Information: AStr := 'infotoolbuttonbordered';
    ArrowLeft: AStr := 'arrowlefttoolbuttonbordered';
    ArrowUp: AStr := 'arrowuptoolbuttonbordered';
    ArrowRight: AStr := 'arrowrighttoolbuttonbordered';
    ArrowDown: AStr := 'arrowdowntoolbuttonbordered';
    Delete: AStr := 'deleteitembutton';
    Details: AStr := 'detailstoolbuttonbordered';
    Organise: AStr := 'organizetoolbuttonbordered';
    PageCurl: AStr := 'pagecurltoolbutton';
    Pause: AStr := 'pausetoolbuttonbordered';
    Play: AStr := 'playtoolbuttonbordered';
    Refresh: AStr := 'refreshtoolbuttonbordered';
    Reply: AStr := 'replytrashtoolbuttonbordered';
    Search: AStr := 'searchtrashtoolbuttonbordered';
    Stop: AStr := 'stoptrashtoolbuttonbordered';
    Trash: AStr := 'trashtoolbuttonbordered';
  end;
  Result.StyleLookup := AStr;
end;

function TKsListItemRow.AddButton(AWidth: integer;
                                  AText: string;
                                  const ATintColor: TAlphaColor = claNull): TksListItemRowButton;
begin
  Result := TksListItemRowButton.Create(Self);
  Result.Align := TListItemAlign.Trailing;
  Result.VertAlign := TListItemAlign.Center;
  Result.Rect := RectF(0, 0, AWidth, 32);
  Result.StyleLookup := 'listitembutton';
  if ATintColor <> claNull then
  begin
    Result.TintColor := ATintColor;
  end;
  Result.Text := AText;
  ShowAccessory := False;
  FList.Add(Result);
end;


function TKsListItemRow.AddSegmentButtons(AWidth: integer;
                                          ACaptions: array of string;
                                          const AItemIndex: integer = -1;
                                          const ATintColor: TAlphaColor = C_DEFAULT_SEGMENT_BUTTON_COLOR): TksListItemRowSegmentButtons;
var
  ICount: integer;
begin
  CanSelect := False;
  Result := TksListItemRowSegmentButtons.Create(Self);
  Result.Align := TListItemAlign.Trailing;
  Result.VertAlign := TListItemAlign.Center;
  Result.Rect := RectF(0, 0, AWidth, C_SEGMENT_BUTTON_HEIGHT);
  Result.TintColor := ATintColor;
  for ICount := Low(ACaptions) to High(ACaptions) do
    Result.Captions.Add(ACaptions[ICount]);
  Result.ItemIndex := AItemIndex;
  ShowAccessory := False;
  FList.Add(Result);
end;


function TKsListItemRow.AddSwitch(x: single;
                                  AIsChecked: Boolean;
                                  const AAlign: TListItemAlign = TListItemAlign.Leading): TksListItemRowSwitch;
var
  s: TSwitch;
  ASize: TSizeF;
begin
  s := TSwitch.Create(nil);
  try
    ASize.Width := s.Width;
    ASize.Height := s.Height;
  finally
    {$IFDEF IOS}
    s.DisposeOf;
    {$ELSE}
    s.Free;
    {$ENDIF}
  end;
  Result := TksListItemRowSwitch.Create(Self);
  Result.Rect := RectF(0, 0, ASize.Width, ASize.Height);
  Result.Align := AAlign;
  Result.VertAlign := TListItemAlign.Center;
  Result.PlaceOffset := PointF(x, 0);
  Result.IsChecked := AIsChecked;
  CanSelect := False;
  FList.Add(Result);
end;

function TksListItemRow.AddSwitchRight(AMargin: integer; AIsChecked: Boolean): TksListItemRowSwitch;
begin
  Result := AddSwitch(AMargin, AIsChecked, TListItemAlign.Trailing)
end;

procedure TKsListItemRow.SetAccessory(const Value: TAccessoryType);
begin
  FAccessory.AccessoryType := Value;
end;

procedure TKsListItemRow.SetAutoCheck(const Value: Boolean);
begin
  FAutoCheck := Value;
  if FAutoCheck then
    FAccessory.AccessoryType := TAccessoryType.Checkmark;
  Changed;
end;

procedure TKsListItemRow.SetCanSelect(const Value: Boolean);
begin
  if FCanSelect <> Value then
  begin
    FCanSelect := Value;
    ListView.Repaint;
  end;
end;

{procedure TKsListItemRow.SetChecked(const Value: Boolean);
begin
  if (Owner as TListViewItem).Checked <> Value then
  begin
    (Owner as TListViewItem).Checked := Value;
    Changed;
  end;
end;   }

procedure TKsListItemRow.SetFontProperties(AName: string; ASize: integer;
  AColor: TAlphaColor; AStyle: TFontStyles);
begin
  if AName <> '' then
    FFont.Family := AName;
  FFont.Size := ASize;
  FTextColor := AColor;
  FFont.Style := AStyle;
end;

procedure TKsListItemRow.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TKsListItemRow.SetIndicatorColor(const Value: TAlphaColor);
begin
  FIndicatorColor := Value;
  RealignStandardElements;
  Changed;
end;

procedure TKsListItemRow.SetSearchIndex(const Value: string);
begin
  TListViewItem(Owner).Text := Value;
end;

procedure TKsListItemRow.SetShowAccessory(const Value: Boolean);
begin
  if FShowAccessory <> Value then
  begin
    FShowAccessory := Value;
    Changed;
  end;
end;

// ------------------------------------------------------------------------------

// text drawing functions...

function TKsListItemRow.TextOut(AText: string; x: single;
  const AVertAlign: TTextAlign = TTextAlign.Center;
  const AWordWrap: Boolean = False): TksListItemRowText;
var
  AWidth: single;
begin
  AWidth := TextWidth(AText);
  Result := TextOut(AText, x,  AWidth, AVertAlign, AWordWrap);
end;

function TKsListItemRow.TextOut(AText: string; x, AWidth: single;
  const AVertAlign: TTextAlign = TTextAlign.Center;
  const AWordWrap: Boolean = False): TksListItemRowText;
begin
  Result := TextOut(AText, x, 0, AWidth, AVertAlign, AWordWrap);
end;


function TKsListItemRow.TextOut(AText: string; x, y, AWidth: single;
  const AVertAlign: TTextAlign = TTextAlign.Center;
  const AWordWrap: Boolean = False): TksListItemRowText;
var
  AHeight: single;

begin
  Result := TksListItemRowText.Create(Self);
  Result.Font.Assign(FFont);
  AHeight := TextHeight(AText);
  Result.FPlaceOffset := PointF(x, y);
  if AWordWrap then
    AHeight := RowHeight(False);
  if AWidth = 0 then
    AWidth := TextWidth(AText);
  Result.FRect := RectF(0, 0, AWidth, AHeight);
  case AVertAlign of
    TTextAlign.Leading: Result.VertAlign := TListItemAlign.Leading;
    TTextAlign.Center: Result.VertAlign := TListItemAlign.Center;
    TTextAlign.Trailing: Result.VertAlign := TListItemAlign.Trailing;
  end;
  Result.TextAlignment := TTextAlign.Leading;
  Result.TextColor := FTextColor;
  Result.Text := AText;
  Result.WordWrap := AWordWrap;
  if SearchIndex = '' then
    SearchIndex := AText;
  FList.Add(Result);
end;

function TKsListItemRow.TextOutRight(AText: string; y, AWidth: single;
  AXOffset: single; const AVertAlign: TTextAlign = TTextAlign.Center)
  : TksListItemRowText;
begin
  Result := TextOut(AText, AXOffset, y, AWidth, AVertAlign);
  Result.Align := TListItemAlign.Trailing;
  Result.TextAlignment := TTextAlign.Trailing;
end;


// ------------------------------------------------------------------------------

{ TksListViewAppearence }

constructor TksListViewAppearence.Create(AListView: TksListView);
begin
  inherited Create;
  FListView := AListView;
  FBackground := claWhite;
  FItemBackground := claWhite;
  FAlternatingItemBackground := claGainsboro;
end;

procedure TksListViewAppearence.SetAlternatingItemBackground
  (const Value: TAlphaColor);
begin
  FAlternatingItemBackground := Value;
  FListView.ApplyStyle;
end;

procedure TksListViewAppearence.SetBackground(const Value: TAlphaColor);
begin
  FBackground := Value;
  FListView.ApplyStyle;
end;

procedure TksListViewAppearence.SetItemBackground(const Value: TAlphaColor);
begin
  FItemBackground := Value;
  FListView.ApplyStyle;
end;

// ------------------------------------------------------------------------------

{ TksListView }

procedure TksListView.CheckAll;
var
  ICount: integer;
begin
  for ICount := 0 to Items.Count-1 do
    Items[ICount].Checked := True;
  RedrawAllRows;
end;

{function TksListView.CountUncachedRows: integer;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  Result := 0;
  for ICount := 0 to Items.Count - 1 do
  begin
    ARow := CachedRow[ICount];
    if ARow <> nil then
    begin
      if ARow.Cached = False then
        Result := Result + 1;
    end;
  end;
end;  }

constructor TksListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScreenScale := GetScreenScale;
  FAppearence := TksListViewAppearence.Create(Self);
  FControlBitmapCache := TksControlBitmapCache.Create(Self);
  FItemHeight := 44;
  FClickTimer := TTimer.Create(Self);
  FLastWidth := 0;
  FSelectOnRightClick := False;
  FCacheTimer := TTimer.Create(Self);
  FCacheTimer.Interval := 100;
  FCacheTimer.OnTimer := OnCacheTimer;
  FCacheTimer.Enabled := True;
  FLastScrollPos := 0;
  FScrolling := False;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Interval := 500;
  FScrollTimer.OnTimer := DoScrollTimer;
  FScrollTimer.Enabled := True;
  FCheckMarks := ksCmNone;
  FCheckMarkStyle := ksCmsDefault;
  FItemImageSize := 32;
end;

destructor TksListView.Destroy;
begin
  {$IFDEF IOS}
  FAppearence.DisposeOf;
  FClickTimer.DisposeOf;
  FCacheTimer.DisposeOf;
  FScrollTimer.DisposeOf;
  FControlBitmapCache.DisposeOf;
  {$ELSE}
  FAppearence.Free;
  FClickTimer.Free;
  FCacheTimer.Free;
  FScrollTimer.Free;
  FControlBitmapCache.Free;
  {$ENDIF}
  inherited;
end;

function TksListView.AddHeader(AText: string): TKsListItemRow;
begin
  Result := AddRow('', '', None);
  Result.Owner.Purpose := TListItemPurpose.Header;
  Result.Font.Style := [];
  Result.TextColor := C_DEFAULT_HEADER_TEXT_COLOR;
  Result.Font.Size := 14;
  Result.TextOut(AText, 0, -3, 0, TTextAlign.Trailing);
  Result.CacheRow;
end;

function TksListView.AddRow(AText, ADetail: string; AAccessory: TksAccessoryType;
  const AImageIndex: integer = -1; const AFontSize: integer = 14;
  AFontColor: TAlphaColor = C_DEFAULT_TEXT_COLOR): TKsListItemRow;
begin
  Result := AddRow(AText, '', ADetail, AAccessory, AImageIndex, AFontSize, AFontColor);
end;

function TksListView.AddRow(AText, ASubTitle, ADetail: string; AAccessory: TksAccessoryType;
  const AImageIndex: integer = -1; const AFontSize: integer = 14;
  AFontColor: TAlphaColor = C_DEFAULT_TEXT_COLOR): TKsListItemRow;
var
  AItem: TListViewItem;
begin
  AItem := Items.Add;
  AItem.Height := ItemHeight;
  AItem.Objects.Clear;
  AItem.Purpose := TListItemPurpose.None;
  Result := TKsListItemRow.Create(AItem);
  if FCheckMarks <> ksCmNone then
    Result.AutoCheck := True;
  Result.Name := 'ksRow';
  Result.ShowAccessory := AAccessory <> None;
  case AAccessory of
    More: Result.Accessory := TAccessoryType.More;
    Checkmark: Result.Accessory := TAccessoryType.Checkmark;
    Detail: Result.Accessory := TAccessoryType.Detail;
  end;
  Result.SetFontProperties('', AFontSize, AFontColor, []);

  Result.ImageIndex := AImageIndex;

  Result.Title.Text := AText;
  Result.SubTitle.Text := ASubTitle;
  Result.Detail.Text := ADetail;

  Result.RealignStandardElements;
end;

function TksListView.AddRow(AText, ASubTitle, ADetail: string;
                            AAccessory: TksAccessoryType;
                            AImage: TBitmap;
                            const AFontSize: integer = 14;
                            AFontColor: TAlphaColor = C_DEFAULT_TEXT_COLOR): TKsListItemRow;
var
  AItem: TListViewItem;
begin
  AItem := Items.Add;
  AItem.Height := ItemHeight;
  AItem.Objects.Clear;
  AItem.Purpose := TListItemPurpose.None;
  Result := TKsListItemRow.Create(AItem);
  if FCheckMarks <> ksCmNone then
    Result.AutoCheck := True;
  Result.Name := 'ksRow';
  Result.ShowAccessory := AAccessory <> None;
  case AAccessory of
    More: Result.Accessory := TAccessoryType.More;
    Checkmark: Result.Accessory := TAccessoryType.Checkmark;
    Detail: Result.Accessory := TAccessoryType.Detail;
  end;
  Result.SetFontProperties('', AFontSize, AFontColor, []);
  Result.Image.Bitmap.Assign(AImage);

  Result.Title.Text := AText;
  Result.SubTitle.Text := ASubTitle;
  Result.Detail.Text := ADetail;

  Result.RealignStandardElements;
end;


procedure TksListView.SetCheckMarks(const Value: TksListViewCheckMarks);
begin
  if FCheckMarks <> Value then
  begin
    FCheckMarks := Value;
    UncheckAll;
    RedrawAllRows;
  end;
end;

procedure TksListView.SetCheckMarkStyle(const Value: TksListViewCheckStyle);
begin
  if FCheckMarkStyle <> Value then
  begin
    FCheckMarkStyle := Value;
    RedrawAllRows;
  end;
end;

procedure TksListView.SetColorStyle(AName: string; AColor: TAlphaColor);
var
  StyleObject: TFmxObject;
begin
  StyleObject := FindStyleResource(AName);
  if StyleObject <> nil then
  begin
    (StyleObject as TColorObject).Color := AColor;
    Invalidate;
  end;
end;

procedure TksListView.SetItemHeight(const Value: integer);
begin
  BeginUpdate;
  try
    FItemHeight := Value;
    RedrawAllRows;
  finally
    ItemAppearance.ItemHeight := Value;
    EndUpdate;
  end;
  Repaint;
end;

procedure TksListView.SetItemImageSize(const Value: integer);
begin
  BeginUpdate;
  try
    FItemImageSize := Value;
    RedrawAllRows;
  finally
    ItemAppearance.ItemHeight := Value;
    EndUpdate;
  end;
  Repaint;
end;

procedure TksListView.UncheckAll;
var
  ICount: integer;
begin
  for ICount := 0 to Items.Count-1 do
    Items[ICount].Checked := False;
  RedrawAllRows;
end;

procedure TksListView.ApplyStyle;
begin
  SetColorStyle('background', FAppearence.Background);
  SetColorStyle('itembackground', FAppearence.ItemBackground);
  SetColorStyle('alternatingitembackground',
    FAppearence.AlternatingItemBackground);
  inherited;
end;

procedure TksListView.BeginUpdate;
begin
  inherited;
  Inc(FUpdateCount);
end;

procedure TksListView.DoClickTimer(Sender: TObject);
var
  ARow: TKsListItemRow;
  AId: string;
  AMouseDownRect: TRectF;
begin
  if FClickedItem = nil then
  begin
    FClickTimer.Enabled := False;
    FMouseDownDuration := 0;
    Exit;
  end;

  FMouseDownDuration := FMouseDownDuration + 1;
  AId := '';
  if FMouseDownDuration >= C_LONG_TAP_DURATION  then
  begin
    FClickTimer.Enabled := False;

    ARow := CachedRow[FClickedItem.Index];
    if ARow <> nil then
      AId := ARow.ID;
    AMouseDownRect := RectF(FMouseDownPos.X-8, FMouseDownPos.Y-8, FMouseDownPos.X+8, FMouseDownPos.Y+8);
    if PtInRect(AMouseDownRect, FCurrentMousepos) then
    begin
      if Assigned(FOnLongClick) then
        FOnLongClick(Self, FMouseDownPos.x, FMouseDownPos.y, FClickedItem, AId, FClickedRowObj);
    end;
    ItemIndex := -1;
  end;
end;


procedure TksListView.DoItemChange(const AItem: TListViewItem);
var
  ARow: TKsListItemRow;
begin
  inherited;
  ARow := CachedRow[AItem.Index];
  ARow.FCached := False;
  ARow.CacheRow;
end;

procedure TksListView.DoItemClick(const AItem: TListViewItem);
begin
  inherited;
end;

procedure TksListView.DoScrollTimer(Sender: TObject);
var
  AVisibleItems: TksVisibleItems;
begin

  if FScrolling = False then
  begin
    if Trunc(ScrollViewPos) <> FLastScrollPos then
    begin
      FScrolling := True;
      FLastScrollPos := Trunc(ScrollViewPos);
      Exit;
    end;
  end
  else
  begin
    if FLastScrollPos = Trunc(ScrollViewPos) then
    begin
      FScrolling := False;
      if Assigned(FOnFinishScrolling) then
      begin
        AVisibleItems := ItemsInView;
        FOnFinishScrolling(Self, AVisibleItems.IndexStart, AVisibleItems.Count);
      end;
    end;
  end;
  FLastScrollPos := Trunc(ScrollViewPos);
end;

procedure TksListView.MouseUp(Button: TMouseButton; Shift: TShiftState; x,
  y: single);
var
  AId: string;
  ARow: TKsListItemRow;
  ARow2: TKsListItemRow;
  ICount: integer;
  AMouseDownRect: TRect;
  ALongTap: Boolean;
begin
  inherited;
  FClickTimer.Enabled := False;
  ALongTap := FMouseDownDuration >= C_LONG_TAP_DURATION ;
  FMouseDownDuration := 0;

  x := x - ItemSpaces.Left;

  ReleaseAllDownButtons;
  AMouseDownRect := Rect(Round(FMouseDownPos.X-8), Round(FMouseDownPos.Y-8), Round(FMouseDownPos.X+8), Round(FMouseDownPos.Y+8));
  if not PtInRect(AMouseDownRect, Point(Round(x),Round(y))) then
    Exit;


  if FClickedItem <> nil then
  begin
    AId := '';
    ARow := CachedRow[FClickedItem.Index];
    if ARow <> nil then
    begin
      AId := ARow.ID;
      FClickedRowObj := RowObjectAtPoint(ARow, x, y);
      ARow.ProcessClick;
      if FCheckMarks = TksListViewCheckMarks.ksCmSingleSelect then
      begin
        for ICount := 0 to Items.Count-1 do
        begin
          if ICount <> FClickedItem.Index then
          begin
            ARow2 := CachedRow[ICount];
            (ARow2.Owner as TListViewItem).Checked := False;
            InvalidateRect(GetItemRect(TListViewItem(ARow2.Owner).Index));
          end;
        end;
      end;
    end;
    if not ALongTap then
    begin
      // normal click.
      if Button = TMouseButton.mbLeft then
      begin
        Application.ProcessMessages;
        if Assigned(FOnItemClickEx) then
          FOnItemClickEx(Self, FMouseDownPos.x, FMouseDownPos.y, FClickedItem, AId, FClickedRowObj)
      else
        if Assigned(FOnItemRightClickEx) then
          FOnItemRightClickEx(Self, FMouseDownPos.x, FMouseDownPos.y, FClickedItem, AId, FClickedRowObj);
      end;
      if FClickedRowObj <> nil then
      begin
        FClickedRowObj.Click(FMouseDownPos.X - FClickedRowObj.Rect.Left, FMouseDownPos.Y - FClickedRowObj.Rect.Top);
        if (FClickedRowObj is TksListItemRowSwitch) then
        begin
          (FClickedRowObj as TksListItemRowSwitch).Toggle;
          if Assigned(FOnSwitchClicked) then
            FOnSwitchClicked(Self, FClickedItem, (FClickedRowObj as TksListItemRowSwitch), AId);
        end;
        if (FClickedRowObj is TksListItemRowButton) then
        begin
          if Assigned(FOnButtonClicked) then
            FOnButtonClicked(Self, FClickedItem, (FClickedRowObj as TksListItemRowButton), AId);
        end;
        if (FClickedRowObj is TksListItemRowSegmentButtons) then
        begin
          if Assigned(FOnSegmentButtonClicked) then
            FOnSegmentButtonClicked(Self, FClickedItem, (FClickedRowObj as TksListItemRowSegmentButtons), AId);
        end;
        if FClickedRowObj <> nil then
          FClickedRowObj.MouseUp;
        ARow.CacheRow;

      end;
      InvalidateRect(GetItemRect(TListViewItem(ARow.Owner).Index));
    end;
  end;
end;




procedure TksListView.OnCacheTimer(Sender: TObject);
begin
  FCacheTimer.Enabled := False;
  if FControlBitmapCache.ImagesCached = False then
    FControlBitmapCache.CreateImageCache;
  if FControlBitmapCache.ImagesCached then
  begin
    FCacheTimer.OnTimer := nil;
    FCacheTimer.Enabled := False;
    Exit;
  end;
  FCacheTimer.Enabled := True;
end;

procedure TksListView.Paint;
begin
  if not (csDesigning in ComponentState) then
  begin
    if (IsUpdating) or (FControlBitmapCache.ImagesCached = False) then
      Exit;
    if (ItemIndex > -1) then
      ShowSelection := CachedRow[ItemIndex].CanSelect;
  end;
  inherited;
end;

procedure TksListView.RedrawAllRows;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  BeginUpdate;
  for ICount := 0 to Items.Count-1 do
  begin
    ARow := CachedRow[ICount];
    if ARow <> nil then
    begin
      ARow.Cached := False;
      ARow.CacheRow;
    end;
  end;
  EndUpdate;
end;


procedure TksListView.ReleaseAllDownButtons;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  for ICount := 0 to Items.Count-1 do
  begin
    ARow := CachedRow[ICount];
    ARow.ReleaseAllDownButtons;

  end;
end;

procedure TksListView.Resize;
begin
  inherited;
  RedrawAllRows;
end;

function TksListView.RowObjectAtPoint(ARow: TKsListItemRow; x, y: single): TksListItemRowObj;
var
  ICount: integer;
  AObjRect: TRectF;
begin
  Result := nil;
  for ICount := 0 to ARow.RowObjectCount - 1 do
  begin
    AObjRect := ARow.RowObject[ICount].Rect;
    if (FMouseDownPos.x >= (AObjRect.Left - 5)) and
      (FMouseDownPos.x <= (AObjRect.Right + 5)) then
    begin
      Result := ARow.RowObject[ICount];
    end;
  end;
end;

procedure TksListView.EndUpdate;
var
  ICount: integer;
  AItem: TListViewItem;
  ARow: TKsListItemRow;
begin
  inherited EndUpdate;
  Dec(FUpdateCount);
  if FUpdateCount > 0 then
    Exit;
  for ICount := 0 to Items.Count - 1 do
  begin
    AItem := Items[ICount];
    ARow := CachedRow[AItem.Index];
    if ARow <> nil then
      ARow.CacheRow;
  end;
  Invalidate;
end;

function TksListView.GetCachedRow(index: integer): TKsListItemRow;
begin
  Result := Items[index].Objects.FindObject('ksRow') as TKsListItemRow;
end;

{function TksListView.GetIsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;  }

function TksListView.ItemsInView: TksVisibleItems;
var
  ICount: integer;
  r: TRectF;
  cr: TRectF;
  ASearchHeight: integer;
  ASearchBox: TSearchBox;
begin
  cr := RectF(0, 0, Width, Height);;
  if SearchVisible then
  begin
    ASearchBox := TSearchBox.Create(nil);
    try
      ASearchHeight := Round(ASearchBox.Height);
    finally
      {$IFDEF IOS}
      ASearchBox.DisposeOf;
      {$ELSE}
      ASearchBox.Free;
      {$ENDIF}
    end;
    cr.Top := ASearchHeight;
  end;
  Result.IndexStart := -1;
  Result.IndexEnd := -1;
  Result.Count := 0;

  for ICount := 0 to Items.Count-1 do
  begin
    if IntersectRectF(r, GetItemRect(ICount), cr) then
    begin
      if Result.IndexStart = -1 then
        Result.IndexStart := ICount
      else
        Result.IndexEnd := ICount;
      Result.Count := Result.Count + 1;
    end;
  end;
end;

procedure TksListView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  FMouseDownPos := PointF(x-ItemSpaces.Left, y);
  FClickedItem := nil;
  FClickedRowObj := nil;

  FCurrentMousepos := FMouseDownPos;
  FMouseDownDuration := 0;
  for Icount := 0 to Items.Count-1 do
  begin
    if PtInRect(GetItemRect(ICount), PointF(x,y)) then
    begin
      FClickedItem := Items[ICount];
      ARow := CachedRow[ICount];
      if (Button = TMouseButton.mbRight) and (FSelectOnRightClick) then
        ItemIndex := Icount;
      FClickedRowObj := RowObjectAtPoint(ARow, x, y);
    end;
  end;
  inherited;
  Application.ProcessMessages;
  FClickTimer.Interval := 100;
  FClickTimer.OnTimer := DoClickTimer;
  FClickTimer.Enabled := True;
  if FClickedRowObj <> nil then
    FClickedRowObj.MouseDown;
  Invalidate;
end;

procedure TksListView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FCurrentMousepos := PointF(x-ItemSpaces.Left, y);
end;

{ TksListItemRowSwitch }


function TksListItemRowSwitch.Render(ACanvas: TCanvas): Boolean;
var
  ABmp: TBitmap;
begin
  Result := inherited Render(ACanvas);
  if FControlImageCache.ImagesCached = False then
  begin
    Result := False;
    Exit;
  end;
  ABmp := TBitmap.Create;
  try
    ABmp.Assign(FControlImageCache.SwitchImage[FIsChecked]);
    ACanvas.DrawBitmap(ABmp, RectF(0,0,ABmp.Width,ABmp.Height), FRect, 1);
  finally
    {$IFDEF IOS}
    ABmp.DisposeOf;
    {$ELSE}
    ABmp.Free;
    {$ENDIF}
  end;
end;

procedure TksListItemRowSwitch.SetIsChecked(const Value: Boolean);
begin
  FIsChecked := Value;
  Changed;
end;

procedure TksListItemRowSwitch.Toggle;
begin
  IsChecked := not IsChecked;
end;

{ TKsListItemRowAccessory }

constructor TKsListItemRowAccessory.Create(ARow: TKsListItemRow);
begin
  inherited;
  FResources := FRow.GetStyleResources;
  FImage := FResources.AccessoryImages[FAccessoryType].Normal;
  FRect := RectF(0, 0, FImage.Width, FImage.Height);
  FAlign := TListItemAlign.Trailing;
  FVertAlignment := TListItemAlign.Center;
end;

function TKsListItemRowAccessory.Render(ACanvas: TCanvas): Boolean;
var
  ARect: TRectF;
  ABmp: TBitmap;
  APath: TPathData;
begin
  inherited Render(ACanvas);
  if (FAccessoryType = TAccessoryType.Checkmark) and
     (TksListView(FRow.ListView).CheckMarkStyle <> ksCmsDefault)  then
  begin
    ARect := RectF(FRect.Left, FRect.Top, FRect.Left + (64*GetScreenScale), FRect.Top + (64*GetScreenScale));
    OffsetRect(ARect, (-8), 0);
    InflateRect(ARect, 2, 2);

    ABmp := TBitmap.Create(Round(64*GetScreenScale), Round(64*GetScreenScale));
    try
      ABmp.Clear(claNull);
      ABmp.Canvas.BeginScene;
      // custom check drawing...
      case TksListView(FRow.ListView).CheckMarkStyle  of
        ksCmsGreen: ABmp.Canvas.Fill.Color := claLimegreen;
        ksCmsRed: ABmp.Canvas.Fill.Color := claRed;
        ksCmsBlue: ABmp.Canvas.Fill.Color := claBlue;
      end;

      ABmp.Canvas.FillEllipse(RectF(0, 0, ABmp.Width, ABmp.Height), 1);
      ACanvas.Stroke.Color := ABmp.Canvas.Fill.Color;

      ABmp.Canvas.StrokeThickness := 8;

      ABmp.Canvas.Stroke.Color := claWhite;
      ABmp.Canvas.Fill.Color := claWhite;
      ABmp.Canvas.StrokeJoin := TStrokeJoin.Miter;
      ABmp.Canvas.StrokeCap := TStrokeCap.Flat;

      APath := TPathData.Create;
      try
        APath.MoveTo(PointF(ABmp.Height * 0.25, (ABmp.Height * 0.55)));
        APath.LineTo(PointF(ABmp.Height * 0.4, (ABmp.Height * 0.7)));
        APath.LineTo(PointF(ABmp.Width * 0.70, ABmp.Height * 0.25));
        ABmp.Canvas.DrawPath(APath, 1);
      finally
        {$IFDEF IOS}
        APath.DisposeOf;
        {$ELSE}
        APath.Free;
        {$ENDIF}
      end;
      ABmp.Canvas.EndScene;

      ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), RectF(ARect.Left, ARect.Top, ARect.Left+20, ARect.Top+20), 1);
    finally
      {$IFDEF IOS}
      ABmp.DisposeOf;
      {$ELSE}
      ABmp.Free;
      {$ENDIF}
    end;
  end
  else
  begin
    FImage := FResources.AccessoryImages[FAccessoryType].Normal;
    FImage.DrawToCanvas(ACanvas, FRect, 1);
  end;
  Result := True;
end;

procedure TKsListItemRowAccessory.SetAccessoryType(const Value: TAccessoryType);
begin
  FAccessoryType := Value;
  Changed;
end;

{ TksListItemRowSegmentButtons }


procedure TksListItemRowSegmentButtons.Click(x, y: single);
var
  ABtnWidth: single;
begin
  inherited;
  ABtnWidth := FRect.Width / FCaptions.Count;
  ItemIndex := Trunc(x / ABtnWidth);
end;

constructor TksListItemRowSegmentButtons.Create(ARow: TKsListItemRow);
begin
  inherited;
  FCaptions := TStringList.Create;
  FItemIndex := -1;
end;

destructor TksListItemRowSegmentButtons.Destroy;
begin
  {$IFDEF IOS}
  FCaptions.DisposeOf;
  {$ELSE}
  FCaptions.Free;
  {$ENDIF}
  inherited;
end;

function TksListItemRowSegmentButtons.Render(ACanvas: TCanvas): Boolean;
var
  ABmp: TBitmap;
  ABtnWidth: integer;
  ABtnRect: TRectF;
  ICount: integer;
  AHeight: single;
begin
  Result := inherited Render(ACanvas);
  if FControlImageCache.ImagesCached = False then
    Exit;
  ABtnWidth := Trunc(FRect.Width / FCaptions.Count);
  ABtnRect := RectF(FRect.Left, FRect.Top, FRect.Left + ABtnWidth, FRect.Bottom);
  for ICount := 0 to FCaptions.Count-1 do
  begin
    if FItemIndex = -1 then
      FItemIndex := 0;
    AHeight := FRect.Height;

    ABmp := TBitmap.Create;
    try
      if ICount = 0 then ABmp.Assign(FControlImageCache.ButtonImage[ABtnWidth, AHeight, FCaptions[ICount], FTintColor, ICount = FItemIndex, 'segmentedbuttonleft'])
      else
        if ICount = FCaptions.Count-1 then ABmp.Assign(FControlImageCache.ButtonImage[ABtnWidth, AHeight, FCaptions[ICount], FTintColor, ICount = FItemIndex, 'segmentedbuttonright'])
      else
        ABmp.Assign(FControlImageCache.ButtonImage[ABtnWidth, AHeight, FCaptions[ICount], FTintColor, ICount = FItemIndex, 'segmentedbuttonmiddle']);
    
      if ABmp <> nil then
      begin
        if IsBlankBitmap(ABmp) then
          Exit;
        ACanvas.DrawBitmap(ABmp, RectF(0,0,ABmp.Width,ABmp.Height), ABtnRect, 1, True);
        Result := True;
      end;
    finally
      {$IFDEF IOS}
      ABmp.DisposeOf;
      {$ELSE}
      ABmp.Free;
      {$ENDIF}
    end;
    OffsetRect(ABtnRect, ABtnWidth-1, 0);
  end;
end;

procedure TksListItemRowSegmentButtons.SetItemIndex(const Value: integer);
begin
  if FItemIndex = Value then
    Exit;
  if Value > FCaptions.Count-1 then
    FItemIndex := FCaptions.Count-1
  else
    FItemIndex := Value;
  Changed;
end;

procedure TksListItemRowSegmentButtons.SetTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
  Changed;
end;

{ TksControlBitmapCache }

constructor TksControlBitmapCache.Create(Owner: TksListView);
begin
  inherited Create;
  FOwner := Owner;
  FImagesCached := False;
  FCreatingCache := False;
  FButton := TSpeedButton.Create(Owner.Parent);
  FButton.Height := C_SEGMENT_BUTTON_HEIGHT;
  FCachedButtons := TStringList.Create;
  FGuid := CreateGuidStr;
end;

destructor TksControlBitmapCache.Destroy;
var
  ICount: integer;
begin
  {$IFDEF IOS}
  FSwitchOn.DisposeOf;
  FSwitchOff.DisposeOf;
  for ICount := FCachedButtons.Count-1 downto 0 do
    FCachedButtons.Objects[ICount].DisposeOf;
  FCachedButtons.DisposeOf;
  {$ELSE}
  FSwitchOn.Free;
  FSwitchOff.Free;
  for ICount := FCachedButtons.Count-1 downto 0 do
    FCachedButtons.Objects[ICount].Free;
  FCachedButtons.Free;
  {$ENDIF}
  inherited;
end;

function TksControlBitmapCache.CreateImageCache: Boolean;
var
  ASwitch: TSwitch;
  AForm: TFmxObject;
begin
  Result := False;
  if FCreatingCache then
    Exit;
  if (FImagesCached) then
  begin
    Result := True;
    Exit;
  end;

  try
    FCreatingCache := True;
    Result := False;
    AForm := FOwner.Parent;
    while (AForm is TForm) = False do
        AForm := AForm.Parent;

    ASwitch := TSwitch.Create(AForm);
    try
      ASwitch.Name :=  '_'+CreateGuidStr;
      AForm.InsertObject(0, ASwitch);
      Application.ProcessMessages;
      ASwitch.IsChecked := True;
      FSwitchOn := ASwitch.MakeScreenshot;
      if IsBlankBitmap(FSwitchOn) then
      begin
        AForm.RemoveObject(ASwitch);
        Application.ProcessMessages;
        {$IFDEF IOS}
        ASwitch.DisposeOf;
        FSwitchOn.DisposeOf;
        {$ELSE}
        ASwitch.Free;
        FSwitchOn.Free;
        {$ENDIF};
        FSwitchOn := nil;
        ASwitch := nil;
        Exit;
      end;
         // application not ready to create images.
      ASwitch.IsChecked := False;
      FSwitchOff := ASwitch.MakeScreenshot;
    finally
      if ASwitch <> nil then
      begin
        AForm.RemoveObject(ASwitch);
        Application.ProcessMessages;
        {$IFDEF IOS}
        ASwitch.DisposeOf;
        {$ELSE}
        ASwitch.Free;
        {$ENDIF}
      end;
    end;

    FButton.Position := FOwner.Position;
    AForm.InsertObject(0, FButton);
    Application.ProcessMessages;

    FImagesCached := True;

  finally
    FCreatingCache := False;
  end;

  Application.ProcessMessages;
  FOwner.RedrawAllRows;
end;


function TksControlBitmapCache.GetButtonImage(AWidth, AHeight: single; AText: string; ATintColor: TAlphaColor; ASelected: Boolean;
  AStyleLookup: string): TBitmap;
var
  AId: string;
begin
  Result := nil;
  if FButton.Parent = nil then
    Exit;
  AId := FloatToStr(AWidth)+'_'+
         FloatToStr(AWidth)+'_'+
         AText+' '+
         BoolToStr(ASelected)+'_'+
         IntToStr(ATintColor)+'_'+
         AStyleLookup;
  if FCachedButtons.IndexOf(AId) > -1 then
  begin
    Result := TBitmap(FCachedButtons.Objects[FCachedButtons.IndexOf(AId)]);
    Exit;
  end;
  if FButton.Parent = nil then
  begin
    if FOwner.Parent = nil then
      Exit;
    FButton.Position := FOwner.Position;
    FOwner.Parent.InsertObject(0, FButton);
    Application.ProcessMessages;
  end;
  FButton.Name := '_'+CreateGuidStr;
  FButton.TextSettings.FontColorForState.Active := ATintColor;
  FButton.TextSettings.FontColorForState.Normal := ATintColor;
  FButton.TextSettings.FontColorForState.Pressed := claWhite;
  FButton.FontColor := ATintColor;
  FButton.StyleLookup := AStyleLookup;
  FButton.TintColor := ATintColor;
  FButton.StyledSettings := FButton.StyledSettings - [TStyledSetting.FontColor];

  FButton.StaysPressed := True;
  FButton.GroupName := 'cacheButton';
  FButton.Width := AWidth;
  FButton.Height := AHeight;
  FButton.Text := AText;


  FButton.IsPressed := ASelected;

  Result := FButton.MakeScreenshot;

  if IsBlankBitmap(Result) then
  begin
    {$IFDEF IOS}
    Result.DisposeOf;
    {$ELSE}
    Result.Free;
    {$ENDIF}
    Result := nil;
    Exit;
  end;

  FCachedButtons.AddObject(AId, Result);
end;

function TksControlBitmapCache.GetSwitchImage(AChecked: Boolean): TBitmap;
begin
  Result := nil;
  case AChecked of
    True: Result := FSwitchOn;
    False: Result := FSwitchOff;
  end;
end;

{ TksListItemRowButton }

constructor TksListItemRowButton.Create(ARow: TKsListItemRow);
begin
  inherited;
  FTintColor := claNull;
  FIsDown := False;
end;

procedure TksListItemRowButton.MouseDown;
begin
  inherited;
  FIsDown := True;
  Changed;
  FRow.CacheRow;
end;

procedure TksListItemRowButton.MouseUp;
begin
  inherited;
  FIsDown := False;
  Changed;
  FRow.CacheRow;
end;

function TksListItemRowButton.Render(ACanvas: TCanvas): Boolean;
var
  ABmp: TBitmap;
  AHeight: single;
begin
  inherited Render(ACanvas);
  Result := False;
  if FControlImageCache.ImagesCached = False then
    Exit;

  AHeight := FRect.Height;

  ABmp := TBitmap.Create;
  try
    ABmp.Assign(FControlImageCache.ButtonImage[FRect.Width, AHeight, FText, FTintColor, FIsDown, FStyleLookup]);
    if ABmp <> nil then
    begin
      if IsBlankBitmap(ABmp) then
        Exit;
      ACanvas.DrawBitmap(ABmp, RectF(0,0,ABmp.Width,ABmp.Height), FRect, 1, True);
      Result := True;
    end;
  finally
    {$IFDEF IOS}
    ABmp.DisposeOf;
    {$ELSE}
    ABmp.Free;
    {$ENDIF}
  end;
end;

procedure TksListItemRowButton.SetStyleLookup(const Value: string);
begin
  if FStyleLookup <> Value then
  begin
    FStyleLookup := Value;
    Changed;
  end;
end;

procedure TksListItemRowButton.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TksListItemRowButton.SetTintColor(const Value: TAlphaColor);
begin
  if FTintColor <> Value then
  begin
    FTintColor := Value;
    Changed;
  end;
end;

end.
