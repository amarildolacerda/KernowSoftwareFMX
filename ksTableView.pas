{*******************************************************************************
*                                                                              *
*  TksTableView - High-Performance Mobile Scrolling List Component             *
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
*  See the License forthe specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksTableView;

interface

uses Classes, FMX.Controls, FMX.Layouts, FMX.Types, Types, Generics.Collections,
  FMX.Graphics, FMX.Objects, FMX.InertialMovement, System.UITypes,
  System.UIConsts, System.Rtti,
  FMX.SearchBox, FMX.Styles, FMX.Styles.Objects;

{$IFDEF VER290}
{$DEFINE XE8_OR_NEWER}
{$ENDIF}
{$IFDEF VER300}
{$DEFINE XE8_OR_NEWER}
{$DEFINE XE10_OR_NEWER}
{$ENDIF}

const
  C_TABLEVIEW_DEFAULT_ITEM_HEIGHT = 44;
  C_TABLEVIEW_DEFAULT_HEADER_HEIGHT = 30;

  C_TABLEVIEW_DEFAULT_SELECTED_COLOR = $FFD6EFF9;
  C_TABLEVIEW_DEFAULT_FONT_SIZE = 13;

  //C_TABLEVIEW_DEFAULT_HEADER_TEXT_COLOR = claBlack;
  C_TABLEVIEW_DEFAULT_HEADER_COLOR = $FFF0F0F0;

  C_TABLEVIEW_DEFAULT_INDICATOR_WIDTH = 6;
  C_TABLEVIEW_DEFAULT_INDICATOR_HEIGHT = 0;
  // default which stretches to row height

  C_TABLEVIEW_DEFAULT_IMAGE_SIZE = 24;

  C_TABlEVIEW_SCROLL_THRESHOLD = 10;

  C_TABLEVIEW_PAGE_SIZE = 50;

  {$IFDEF ANDROID}
  C_TABLEIEW_DEFAULT_SWITCH_COLOR = claDodgerBlue;
  {$ELSE}
  C_TABLEIEW_DEFAULT_SWITCH_COLOR = claLimeGreen;
  {$ENDIF}

type
  TksTableViewItem = class;
  TksTableViewItemObject = class;
  TksTableView = class;
  TksTableViewActionButtons = class;
  TksTableViewActionButon = class;

  TksTableItemAlign = (Leading, Center, Trailing);
  TksSwipeDirection = (ksSwipeUnknown, ksSwipeLeftToRight, ksSwipeRightToLeft, ksSwipeTopToBottom, ksSwipeBottomToTop);
  TksTableViewShape = (ksRectangle, ksRoundRect, ksEllipse);
  TksTableViewItemPurpose = (None, Header);
  TksTableViewCheckMarks = (cmNone, cmSingleSelect, cmMultiSelect);
  TksTableViewActionButtonAlignment = (abLeftActionButtons, abRightActionButtons);
  TksTableViewTextWidth = (ksWidth10Percent,
                           ksWidth20Percent,
                           ksWidth30Percent,
                           ksWidth40Percent,
                           ksWidth50Percent,
                           ksWidth60Percent,
                           ksWidth70Percent,
                           ksWidth80Percent,
                           ksWidth90Percent);


  TksTableViewRowCacheEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARow: TksTableViewItem; ARect: TRectF) of object;
  TksTableViewDeletingItemEvent = procedure(Sender: TObject; AItem: TksTableViewItem; var ACanDelete: Boolean) of object;
  TksTableViewDeleteItemEvent = procedure(Sender: TObject; AItem: TksTableViewItem) of object;
  TksTableViewItemClickEvent = procedure(Sender: TObject; x, y: single; AItem: TksTableViewItem; AId: string; ARowObj: TksTableViewItemObject) of object;
  TksItemSwipeEvent = procedure(Sender: TObject; ARow: TksTableViewItem; ASwipeDirection: TksSwipeDirection; AButtons: TksTableViewActionButtons) of object;
  TksItemActionButtonClickEvent = procedure(Sender: TObject; ARow: TksTableViewItem; AButton: TksTableViewActionButon) of object;
  TksItemChecMarkChangedEvent = procedure(Sender: TObject; ARow: TksTableViewItem; AChecked: Boolean) of object;

  TksAccessoryType = (atNone, atMore, atCheckmark, atDetail, atBack, atRefresh,
    atAction, atPlay, atRewind, atForward, atPause, atStop, atAdd, atPrior,
    atNext, atArrowUp, atArrowDown, atArrowLeft, atArrowRight, atReply,
    atSearch, atBookmarks, atTrash, atOrganize, atCamera, atCompose, atInfo,
    atPagecurl, atDetails, atRadioButton, atRadioButtonChecked, atCheckBox,
    atCheckBoxChecked, atUserDefined1, atUserDefined2, atUserDefined3);

  TksTableViewActionButon = class
  private
    FWidth: integer;
    FTextColor: TAlphaColor;
    FColor: TAlphaColor;
    FText: string;
    FIsDeleteButton: Boolean;
  public
    constructor Create(AIsDelete: Boolean);
    property Text: string read FText write FText;
    property TextColor: TAlphaColor read FTextColor write FTextColor default claWhite;
    property Color: TAlphaColor read FColor write FColor;
    property Width: integer read FWidth write FWidth default 80;
    property IsDeleteButton: Boolean read FIsDeleteButton;
  end;

  TksTableViewActionButtons = class(TObjectList<TksTableViewActionButon>)
  private
    [weak]FTableItem: TksTableviewItem;
    FPercentWidth: integer;
    FAlignment: TksTableViewActionButtonAlignment;
    FAnimating: Boolean;
    procedure SetPercentWidth(const Value: integer);
    function GetVisible: Boolean;
    procedure ShowButtons;
    procedure HideButtons(ASync: Boolean);
    function TotalWidth: integer;
    property PercentWidth: integer read FPercentWidth write SetPercentWidth;
    property Visible: Boolean read GetVisible;
    procedure Render(ACanvas: TCanvas; ARect: TRectF);
    function ButtonFromXY(x, y: single): TksTableViewActionButon;
  public
    constructor Create(AOwner: TksTableViewItem);
    function AddButton(AText: string; AColor, ATextColor: TAlphaColor; AWidth: integer): TksTableViewActionButon;
  end;

  TksTableViewItemObject = class
  private
    [weak]FTableItem: TksTableViewItem;
    FAlign: TksTableItemAlign;
    FVertAlign: TksTableItemAlign;
    FHitText: Boolean;
    FID: string;

    FWidth: single;
    FHeight: single;
    FPlaceOffset: TPointF;
    FHitTest: Boolean;
    // FPadding: TRectF;
    procedure SetHeight(const Value: single);
    procedure SetWidth(const Value: single);
    procedure SetPlaceOffset(const Value: TPointF);
    procedure SetHitTest(const Value: Boolean);
  protected
    // function AdjustItemRectForAccessory: Boolean; virtual;
    function GetAlign: TksTableItemAlign;
    function GetID: string;
    function GetVertAlign: TksTableItemAlign;
    function GetItemRect: TRectF;
    function GetObjectRect: TRectF; virtual;
    procedure Changed;
    procedure Render(ACanvas: TCanvas); virtual; abstract;
    procedure SetAlign(Value: TksTableItemAlign);
    procedure SetVertAlign(Value: TksTableItemAlign);
    procedure SetID(Value: string);
    property ObjectRect: TRectF read GetObjectRect;
  public
    constructor Create(ATableItem: TksTableViewItem); virtual;
    property Align: TksTableItemAlign read GetAlign write SetAlign;
    property Height: single read FHeight write SetHeight;
    property HitTest: Boolean read FHitTest write SetHitTest default True;
    property ID: string read GetID write SetID;
    property PlaceOffset: TPointF read FPlaceOffset write SetPlaceOffset;
    property VertAlign: TksTableItemAlign read GetVertAlign write SetVertAlign;
    property Width: single read FWidth write SetWidth;
  end;

  TksTableViewItemText = class(TksTableViewItemObject)
  private
    FBackground: TAlphaColor;
    FText: string;
    FFont: TFont;
    FTextColor: TAlphaColor;
    FTextAlign: TTextAlign;
    FTextVertAlign: TTextAlign;
    FWordWrap: Boolean;
    FTrimming: TTextTrimming;
    FIsHtmlText: Boolean;
    function GetText: string;
    function GetFont: TFont;
    function GetTextColor: TAlphaColor;
    function GetTextAlign: TTextAlign;
    function GetTextVertAlign: TTextAlign;
    function GetTrimming: TTextTrimming;
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetTextAlign(const Value: TTextAlign);
    procedure SetTextVertAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure SetBackground(const Value: TAlphaColor);
  protected
    procedure Render(ACanvas: TCanvas); override;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    destructor Destroy; override;
    property Background: TAlphaColor read FBackground write SetBackground default claNull;
    property Font: TFont read GetFont write SetFont;
    property Text: string read GetText write SetText;
    property TextColor: TAlphaColor read GetTextColor write SetTextColor default claBlack;
    property TextAlignment: TTextAlign read GetTextAlign write SetTextAlign default TTextAlign.Leading;
    property TextVertAlign: TTextAlign read GetTextVertAlign write SetTextVertAlign default TTextAlign.Leading;
    property Trimming: TTextTrimming read GetTrimming write SetTrimming default TTextTrimming.Character;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property IsHtmlText: Boolean read FIsHtmlText;
  end;

  TksTableViewItemBaseImage = class(TksTableViewItemObject)
  strict private
    FBitmap: TBitmap;
    [weak]FExternalBitmap: TBitmap;
  private
    FOwnsBitmap: Boolean;
    procedure SetBitmap(const Value: TBitmap);
    function GetBitmap: TBitmap;
    procedure SetOwnsBitmap(const Value: Boolean);
  protected
    procedure Render(ACanvas: TCanvas); override;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property OwnsBitmap: Boolean read FOwnsBitmap write SetOwnsBitmap default False;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    destructor Destroy; override;


  end;

  TksTableViewItemImage = class(TksTableViewItemBaseImage)
  public
    property Bitmap;
  end;

  TksTableViewItemShape = class(TksTableViewItemObject)
  private
    FStroke: TStrokeBrush;
    FFill: TBrush;
    FShape: TksTableViewShape;
    FCornerRadius: single;
    procedure SetCornerRadius(const Value: single);
    procedure SetFill(const Value: TBrush);
    procedure SetShape(const Value: TksTableViewShape);
    procedure SetStroke(const Value: TStrokeBrush);
  protected
    procedure Render(ACanvas: TCanvas); override;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    destructor Destroy; override;
    property Stroke: TStrokeBrush read FStroke write SetStroke;
    property Fill: TBrush read FFill write SetFill;
    property Shape: TksTableViewShape read FShape write SetShape;
    property CornerRadius: single read FCornerRadius write SetCornerRadius;
  end;

  TksTableViewItemSwitch = class(TksTableViewItemObject)
  private
    FChecked: Boolean;
    FSelectedColor: TAlphaColor;
    procedure SetChecked(const Value: Boolean);
    procedure SetSelectedColor(const Value: TAlphaColor);
  protected
    procedure Render(ACanvas: TCanvas); override;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    property Checked: Boolean read FChecked write SetChecked default False;
    property SelectedColor: TAlphaColor read FSelectedColor write SetSelectedColor default C_TABLEIEW_DEFAULT_SWITCH_COLOR;
  end;

  TksTableViewItemAccessory = class(TksTableViewItemImage)
  private
    FAccessory: TksAccessoryType;
  protected
    function GetObjectRect: TRectF; override;
    function GetAccessory: TksAccessoryType;
    procedure SetAccessory(const Value: TksAccessoryType);
    property Accessory: TksAccessoryType read GetAccessory write SetAccessory;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
  end;

  TksTableViewItemObjects = class(TObjectList<TksTableViewItemObject>)
  private
    [weak]FTableView: TksTableView;
  public
    constructor Create(ATableView: TksTableView); virtual;
  end;

  TksTableViewItem = class(TFmxObject)
  private
    [weak]FTableView: TksTableView;
    FID: string;
    FAbsoluteIndex: integer;
    FIndicator: TksTableViewItemShape;
    FImage: TksTableViewItemImage;
    FTitle: TksTableViewItemText;
    FSubTitle: TksTableViewItemText;
    FDetail: TksTableViewItemText;
    FAccessory: TksTableViewItemAccessory;
    FHeight: single;
    FItemRect: TRectF;
    FCached: Boolean;
    FCaching: Boolean;
    FBitmap: TBitmap;
    FIndex: integer;
    FSearchIndex: string;
    FChecked: Boolean;
    FUpdating: Boolean;
    FPurpose: TksTableViewItemPurpose;
    FObjects: TksTableViewItemObjects;
    FFont: TFont;
    FTextColor: TAlphaColor;
    FActionButtons: TksTableViewActionButtons;
    FCanSelect: Boolean;
    FTitleWidth: TksTableViewTextWidth;
    FTagString: string;
    FTagInteger: integer;
    function MatchesSearch(AFilter: string): Boolean;
    function IsVisible(AViewport: TRectF): Boolean;
    function GetHeight: single;
    function GetItemRect: TRectF;
    function GetInternalRect: TRectF;
    //function GetAccessoryRect: TRectF;
    function GetIndex: integer;
    function GetAbsoluteIndex: integer;
    function GetSearchIndex: string;
    function GetCached: Boolean;
    procedure SetSearchIndex(const Value: string);
    procedure SetItemRect(const Value: TRectF);
    procedure SetIndex(const Value: integer);
    //procedure SetAccessory(const Value: TksAccessoryType);
    procedure Changed;
    procedure RealignStandardObjects;
    procedure SetHeight(const Value: single);
    //procedure ReleaseCache;
    procedure SetCached(const Value: Boolean);
    function GetPurpose: TksTableViewItemPurpose;
    procedure SetPurpose(const Value: TksTableViewItemPurpose);
    procedure SetFont(const Value: TFont);
    procedure SetTextColor(const Value: TAlphaColor);
    //function PartiallyInView(AViewport: TRectF): Boolean;
    procedure SetChecked(const Value: Boolean);
    procedure DoClick(x, y: single);
    function GetIndicatorColor: TAlphaColor;
    procedure SetIndicatorColor(const Value: TAlphaColor);
    procedure DoSwipe(ADirecton: TksSwipeDirection);
    procedure SetTitleWidth(const Value: TksTableViewTextWidth);
  protected
    procedure Render(ACanvas: TCanvas; AScrollPos: single);
    procedure CacheItem(const AForceCache: Boolean = False);

  public
    constructor Create(ATableView: TksTableView); reintroduce;
    destructor Destroy; override;

    function IsLastItem: Boolean;

    // image functions...
    function DrawBitmap(ABmp: TBitmap; x, AWidth, AHeight: single): TksTableViewItemImage; overload;
    function DrawBitmap(ABmp: TBitmap; x, y, AWidth, AHeight: single): TksTableViewItemImage overload;

    // text functions...
    function TextWidth(AText: string; AIsHtml: Boolean): single;
    function TextHeight(AText: string; AWordWrap, AIsHtml: Boolean; const AMaxWidth: single): single;

    procedure SetItemFontStyle(AFontStyle: TFontStyles);
    procedure SetItemTextColor(AColor: TAlphaColor);

    function TextOut(AText: string; x: single; const AVertAlign: TksTableItemAlign = TksTableItemAlign.Center; const AWordWrap: Boolean = False): TksTableViewItemText; overload;
    function TextOut(AText: string; x, AWidth: single; const AVertAlign: TksTableItemAlign = TksTableItemAlign.Center; const AWordWrap: Boolean = False): TksTableViewItemText; overload;
    function TextOut(AText: string; x, y, AWidth: single; const AVertAlign: TksTableItemAlign = TksTableItemAlign.Center; const AWordWrap: Boolean = False): TksTableViewItemText; overload;
    function TextBox(AText: string; ARect: TRectF; ATextAlign: TTextAlign; ATextLayout: TTextAlign; const ABackground: TAlphaColor = claNull): TksTableViewItemText; overload;
    function TextBoxHtml(AText: string; ARect: TRectF): TksTableViewItemText;
    function TextOutRight(AText: string; y, AWidth: single; AXOffset: single; const AVertAlign: TksTableItemAlign = TksTableItemAlign.Center): TksTableViewItemText; overload;

    // shape functions...
    function DrawRect(x, y, AWidth, AHeight: single; AStroke, AFill: TAlphaColor): TksTableViewItemShape;


    function AddSwitch(x: single; AIsChecked: Boolean; const AAlign: TksTableItemAlign = TksTableItemAlign.Trailing): TksTableViewItemSwitch;


    property AbsoluteIndex: integer read GetAbsoluteIndex;
    property Accessory: TksTableViewItemAccessory read FAccessory;
    property CanSelect: Boolean read FCanSelect write FCanSelect default True;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Font: TFont read FFont write SetFont;
    property Height: single read GetHeight write SetHeight;
    property ItemRect: TRectF read GetItemRect write SetItemRect;
    property IndicatorColor: TAlphaColor read GetIndicatorColor write SetIndicatorColor;
    // property InternalRect: TRectF read GetInternalRect;
    property ID: string read FID write FID;
    property Image: TksTableViewItemImage read FImage;
    property Title: TksTableViewItemText read FTitle;
    property SubTitle: TksTableViewItemText read FSubTitle;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default claBlack;
    property Detail: TksTableViewItemText read FDetail;
    property Index: integer read GetIndex write SetIndex;
    property SearchIndex: string read GetSearchIndex write SetSearchIndex;
    property Objects: TksTableViewItemObjects read FObjects;
    property Cached: Boolean read GetCached write SetCached default False;
    property Purpose: TksTableViewItemPurpose read GetPurpose write SetPurpose default None;
    property TagString: string read FTagString write FTagString;
    property TagInteger: integer read FTagInteger write FTagInteger default 0;
    property TitleWidth: TksTableViewTextWidth read FTitleWidth write SetTitleWidth default ksWidth60Percent;
    //property ActionButtonsWidth: integer read FActionButtonsWidth write SetActionButtonsWidth;
  end;

  TksTableViewItems = class(TObjectList<TksTableViewItem>)
  private
    [weak]FTableView: TksTableView;
    procedure UpdateIndexes;
    function GetLastItem: TksTableViewItem;
  protected
    function GetTotalItemHeight: single;
  public
    constructor Create(ATableView: TksTableView; AOwnsObjects: Boolean); virtual;
    function AddItem(AText: string; const AAccessory: TksAccessoryType = atNone): TksTableViewItem; overload;
    function AddItem(AText, ADetail: string; const AAccessory: TksAccessoryType = atNone): TksTableViewItem; overload;
    function AddItem(AText, ASubTitle, ADetail: string; const AAccessory: TksAccessoryType = atNone): TksTableViewItem; overload;

    function AddHeader(AText: string): TksTableViewItem;
    procedure DeleteItem(AItem: TksTableViewItem);
    property LastItem: TksTableViewItem read GetLastItem;
  end;

  // ------------------------------------------------------------------------------

  TksTableViewBackgroundText = class(TPersistent)
  private
    FFont: TFont;
    FTextColor: TAlphaColor;
    FText: string;
    FEnabled: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: string);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Font: TFont read FFont write SetFont;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default claSilver;
    property Text: string read FText write SetText;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  // ------------------------------------------------------------------------------

  TksTableViewAppearence = class(TPersistent)
  private
    [weak]FListView: TksTableView;
    FBackground: TAlphaColor;
    FItemBackground: TAlphaColor;
    FAlternatingItemBackground: TAlphaColor;
    FSeparatorColor: TAlphaColor;
    FHeaderColor: TAlphaColor;
    FSelectedColor: TAlphaColor;
    procedure SetBackground(const Value: TAlphaColor);
    procedure SetItemBackground(const Value: TAlphaColor);
    procedure SetAlternatingItemBackground(const Value: TAlphaColor);
    procedure SetSeparatorBackground(const Value: TAlphaColor);
    procedure SetHeaderColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
  public
    constructor Create(AListView: TksTableView);
  published
    property Background: TAlphaColor read FBackground write SetBackground default claWhite;
    property HeaderColor: TAlphaColor read FHeaderColor write SetHeaderColor default claNull;
    property SeparatorColor: TAlphaColor read FSeparatorColor write SetSeparatorBackground default $FFF0F0F0;
    property ItemBackground: TAlphaColor read FItemBackground write SetItemBackground default claWhite;
    property SelectedColor: TAlphaColor read FSelectedColor write SetSelectedColor default C_TABLEVIEW_DEFAULT_SELECTED_COLOR;
    property AlternatingItemBackground: TAlphaColor read FAlternatingItemBackground write SetAlternatingItemBackground default claNull;
  end;

  TksDeleteButton = class(TPersistent)
  private
    FEnabled: Boolean;
    FText: string;
    FColor: TAlphaColor;
    FTextColor: TAlphaColor;
    FWidth: integer;
  public
    constructor Create; virtual;
  published
    property Color: TAlphaColor read FColor write FColor default claRed;
    property TextColor: TAlphaColor read FTextColor write FTextColor default claWhite;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Text: string read FText write FText;
    property Width: integer read FWidth write FWidth default 60;
  end;


  TksListViewRowIndicators = class(TPersistent)
  private
    FWidth: integer;
    FHeight: integer;
    FVisible: Boolean;
    FOutlined: Boolean;
    FShadow: Boolean;
  published
    constructor Create; virtual;
    property Width: integer read FWidth write FWidth default C_TABLEVIEW_DEFAULT_INDICATOR_WIDTH;
    property Height: integer read FHeight write FHeight default C_TABLEVIEW_DEFAULT_INDICATOR_HEIGHT;
    property Visible: Boolean read FVisible write FVisible default False;
    property Outlined: Boolean read FOutlined write FOutlined default True;
    property Shadow: Boolean read FShadow write FShadow default True;
  end;

  TksTableViewTextDefault = class(TPersistent)
  private
    FFont: TFont;
    FTextColor: TAlphaColor;
    procedure SetFont(const Value: TFont);
    procedure SetTextColor(const Value: TAlphaColor);
  public
    constructor Create;
    destructor Destroy; override;
  published
    procedure Assign(Source: TPersistent);
    property Font: TFont read FFont write SetFont;
    property TextColor: TAlphaColor read FTextColor write SetTextColor;
  end;


  TksTableViewTextDefaults = class(TPersistent)
  private
    FTitle: TksTableViewTextDefault;
    FSubtitle: TksTableViewTextDefault;
    FDetail: TksTableViewTextDefault;
    FHeader: TksTableViewTextDefault;
    procedure SetDetail(const Value: TksTableViewTextDefault);
    procedure SetSubTitle(const Value: TksTableViewTextDefault);
    procedure SetTitle(const Value: TksTableViewTextDefault);
    procedure SetHeader(const Value: TksTableViewTextDefault);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Title: TksTableViewTextDefault read FTitle write SetTitle;
    property SubTitle: TksTableViewTextDefault read FSubtitle write SetSubTitle;
    property Detail: TksTableViewTextDefault read FDetail write SetDetail;
    property Header: TksTableViewTextDefault read FHeader write SetHeader;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidiOSDevice32 or pidiOSDevice64 or pidAndroid)]
  TksTableView = class(TControl)
  private
    FSearchBox: TSearchBox;
    FItems: TksTableViewItems;
    FFilteredItems: TksTableViewItems;
    FTimerService: IFMXTimerService;
    FAniCalc: TAniCalculations;
    FScrollPos: single;
    FPainting: Boolean;
    FScrolling: Boolean;
    FAppearence: TksTableViewAppearence;
    FHeaderHeight: integer;
    FItemHeight: integer;
    FItemImageSize: integer;
    FSearchVisible: Boolean;
    FShowSelection: Boolean;
    FItemIndex: integer;
    FMouseDownPoint: TPointF;
    FMouseCurrentPos: TPointF;
    FUpdateCount: integer;
    FShowAccessory: Boolean;
    FSelectTimer: TFmxHandle;
    FDeselectTimer: TFmxHandle;
    FKeepSelection: Boolean;
    FSwipeDirection: TksSwipeDirection;
    FMouseDownItem: TksTableViewItem;
    FMouseDown: Boolean;
    FBackgroundText: TksTableViewBackgroundText;
    FRowIndicators: TksListViewRowIndicators;
    FDeleteButton: TksDeleteButton;
    FTextDefaults: TksTableViewTextDefaults;
    FStickyHeaders: Boolean;

    // events...
    FItemClickEvent: TksTableViewItemClickEvent;
    FOnPullRefresh: TNotifyEvent;
    FPullToRefresh: Boolean;
    FNeedsRefresh: Boolean;
    FCheckMarks: TksTableViewCheckMarks;
    FOnItemSwipe: TksItemSwipeEvent;
    FOnItemActionButtonClick: TksItemActionButtonClickEvent;
    FOnDeleteItem: TksTableViewDeleteItemEvent;
    FOnDeletingItem: TksTableViewDeletingItemEvent;
    FBeforeRowCache: TksTableViewRowCacheEvent;
    FAfterRowCache: TksTableViewRowCacheEvent;
    FOnItemChecMarkChanged: TksItemChecMarkChangedEvent;
    FFullWidthSeparator: Boolean;

    function GetViewPort: TRectF;
    procedure SetScrollViewPos(const Value: single);
    procedure AniCalcStart(Sender: TObject);
    procedure AniCalcChange(Sender: TObject);
    procedure AniCalcStop(Sender: TObject);
    procedure CacheItems(AForceRedraw: Boolean);
    function GetTopItem: TksTableViewItem;
    function GetVisibleItems: TList<TksTableViewItem>;
    procedure SetHeaderHeight(const Value: integer);
    procedure SetItemImageSize(const Value: integer);
    procedure SetKsItemHeight(const Value: integer);
    procedure SetSearchVisible(const Value: Boolean);
    procedure SetShowSelection(const Value: Boolean);
    procedure SetItemIndex(const Value: integer);
    procedure DoFilterChanged(Sender: TObject);
    function GetScrollViewPos: single;
    function GetSearchHeight: single;
    //function GetIsUpdating: Boolean;
    function GetShowAccessory: Boolean;
    procedure SetShowAccessory(const Value: Boolean);
    function GetSelectedItem: TksTableViewItem;
    function GetItemIndex: integer;
    procedure DeselectItem(const ADelay: integer = 0);
    procedure DoDeselectItem;
    procedure SetKeepSelection(const Value: Boolean);
    function GetItemFromYPos(AYPos: single): TksTableViewItem;
    procedure DoPullToRefresh;
    procedure UpdateFilteredItems;
    procedure DoSelectItem;
    procedure SetCheckMarks(const Value: TksTableViewCheckMarks);
    procedure HideAllActionButtons(ASync: Boolean);
    procedure SetTextDefaults(const Value: TksTableViewTextDefaults);
    function CreateTimer(AInterval: integer; AProc: TTimerProc): TFmxHandle;
    procedure KillTimer(ATimer: TFmxHandle);
    procedure KillAllTimers;
    procedure SetFullWidthSeparator(const Value: Boolean);
  protected
    function GetTotalItemHeight: single;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; x, y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure DoMouseLeave; override;
    procedure Resize; override;
    function GetMouseDownBox: TRectF;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearItems;
    procedure BeginUpdate; {$IFDEF XE8_OR_NEWER} override; {$ENDIF}
    procedure EndUpdate;   {$IFDEF XE8_OR_NEWER} override; {$ENDIF}
    procedure Invalidate;
    procedure UpdateItemRects;
    procedure UncheckAll;
    procedure UpdateScrollingLimits;
    property UpdateCount: integer read FUpdateCount;
    property TopItem: TksTableViewItem read GetTopItem;
    property VisibleItems: TList<TksTableViewItem> read GetVisibleItems;
    property ViewPort: TRectF read GetViewPort;
    property ScrollViewPos: single read GetScrollViewPos write SetScrollViewPos;
    property Items: TksTableViewItems read FItems;
    property FilteredItems: TksTableViewItems read FFilteredItems;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property SelectedItem: TksTableViewItem read GetSelectedItem;
  published
    property Align;
    property Anchors;
    property Appearence: TksTableViewAppearence read FAppearence write FAppearence;
    property BackgroundText: TksTableViewBackgroundText read FBackgroundText write FBackgroundText;
    property CanFocus default True;
    property CanParentFocus;
    property CheckMarks: TksTableViewCheckMarks read FCheckMarks write SetCheckMarks default TksTableViewCheckMarks.cmNone;
    property ClipChildren default True;
    property ClipParent default False;
    property Cursor default crDefault;

    property DeleteButton: TksDeleteButton read FDeleteButton write FDeleteButton;
    property FullWidthSeparator: Boolean read FFullWidthSeparator write SetFullWidthSeparator default True;

    property HeaderHeight: integer read FHeaderHeight write SetHeaderHeight default C_TABLEVIEW_DEFAULT_HEADER_HEIGHT;

    property ItemHeight: integer read FItemHeight write SetKsItemHeight default C_TABLEVIEW_DEFAULT_ITEM_HEIGHT;
    property ItemImageSize: integer read FItemImageSize write SetItemImageSize default C_TABLEVIEW_DEFAULT_IMAGE_SIZE;
    property KeepSelection: Boolean read FKeepSelection write SetKeepSelection default False;
    property Locked default False;
    property Height;

    property HitTest default True;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property PullToRefresh: Boolean read FPullToRefresh write FPullToRefresh default True;
    property RotationAngle;
    property RotationCenter;
    property RowIndicators: TksListViewRowIndicators read FRowIndicators write FRowIndicators;
    property Scale;
    property SearchVisible: Boolean read FSearchVisible write SetSearchVisible default False;
    property ShowAccessory: Boolean read GetShowAccessory write SetShowAccessory default True;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
    property StickyHeaders: Boolean read FStickyHeaders write FStickyHeaders default True;
    property TabOrder;
    property TabStop;
    property TextDefaults: TksTableViewTextDefaults read FTextDefaults write SetTextDefaults;
    property Visible default True;
    property Width;

    // events...
    property AfterRowCache: TksTableViewRowCacheEvent read FAfterRowCache write FAfterRowCache;
    property BeforeRowCache: TksTableViewRowCacheEvent read FBeforeRowCache write FBeforeRowCache;

    property OnDblClick;
    property OnDeletingItem: TKsTableViewDeletingItemEvent read FOnDeletingItem write FOnDeletingItem;
    property OnDeleteItem: TKsTableViewDeleteItemEvent read FOnDeleteItem write FOnDeleteItem;
    property OnItemActionButtonClick: TksItemActionButtonClickEvent read FOnItemActionButtonClick write FOnItemActionButtonClick;
    //property OnItemCheckmarkChanged: TksItemChecMarkChangedEvent read FOnItemChecMarkChanged write FOnItemChecMarkChanged;
    property OnItemClick: TksTableViewItemClickEvent read FItemClickEvent write FItemClickEvent;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnPullRefresh: TNotifyEvent read FOnPullRefresh write FOnPullRefresh;
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
    property OnItemSwipe: TksItemSwipeEvent read FOnItemSwipe write FOnItemSwipe;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    (*
      property Transparent default False;
      property AllowSelection;
      property AlternatingColors;

      property OnItemClickRight: TksListViewRowClickEvent read FOnItemRightClick write FOnItemRightClick;
      property CheckMarks: TksListViewCheckMarks read FCheckMarks write SetCheckMarks default ksCmNone;
      property CheckMarkStyle: TksListViewCheckStyle read FCheckMarkStyle write SetCheckMarkStyle default ksCmsDefault;
      property DeleteButton: TksDeleteButton read FDeleteButton write FDeleteButton;
      property DisableFocusEffect default True;
      property DragMode default TDragMode.dmManual;
      property EnableDragHighlight default True;
      property Enabled default True;
      property FullWidthSeparator: Boolean read FFullWidthSeparator write FFullWidthSeparator default True;
      property SelectOnRightClick: Boolean read FSelectOnRightClick write FSelectOnRightClick default False;
      property Size;
      property RowIndicators: TksListViewRowIndicators read FRowIndicators write FRowIndicators;


      property OnSelectOption: TksOptionSelectionEvent read FOnOptionSelection write FOnOptionSelection;
      property OnEmbeddedEditChange: TksEmbeddedEditChange read FOnEmbeddedEditChange write FOnEmbeddedEditChange;
      property OnEmbeddedListBoxChange: TksEmbeddedListBoxChange read FOnEmbeddedListBoxChange write FOnEmbeddedListBoxChange;

      property OnSelectDate: TksListViewSelectDateEvent read FOnSelectDate write FOnSelectDate;
      property OnSelectPickerItem: TksListViewSelectPickerItem read FOnSelectPickerItem write FOnSelectPickerItem;
      property OnSearchFilterChanged: TksSearchFilterChange read FOnSearchFilterChanged write FOnSearchFilterChanged;
      { events }
      property OnApplyStyleLookup;
      { Drag and Drop events }
      property PageCaching: TksPageCaching read FPageCaching write FPageCaching;

      property StyleLookup;
      property TouchTargetExpansion;


      { ListView selection events }
      property OnChange;
      property OnChangeRepainted;
      {$IFDEF XE8_OR_NEWER}
      property OnItemsChange;
      property OnScrollViewChange;
      property OnFilter;
      property PullRefreshWait;
      {$ENDIF}

      property OnDeletingItem;
      property OnDeleteItem: TKsDeleteItemEvent read FOnDeleteItem write FOnDeleteItem;
      property OnDeleteChangeVisible;
      property OnSearchChange;


      property AutoTapScroll;
      property AutoTapTreshold;
      //property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
      property ShowSelection: Boolean read FShowSelection write SetShowSelection;
      property DisableMouseWheel;


      property SearchAlwaysOnTop;
      property SelectionCrossfade;
      property KeepSelection: Boolean read FKeepSelection write FKeepSelection default False;
      property OnLongClick: TksListViewRowClickEvent read FOnLongClick write FOnLongClick;
      property OnSwitchClick: TksListViewClickSwitchEvent read FOnSwitchClicked write FOnSwitchClicked;
      property OnButtonClicked: TksListViewClickButtonEvent read FOnButtonClicked write FOnButtonClicked;
      property OnSegmentButtonClicked: TksListViewClickSegmentButtonEvent read FOnSegmentButtonClicked write FOnSegmentButtonClicked;
      property OnScrollFinish: TksListViewFinishScrollingEvent read FOnFinishScrolling write FOnFinishScrolling;
      property OnScrollLastItem: TNo
      tifyEvent read FOnScrollLastItem write FOnScrollLastItem;
      property OnItemSwipe: TksItemSwipeEvent read FOnItemSwipe write FOnItemSwipe;
      property OnItemActionButtonClick: TksItemActionButtonClickEvent read FOnItemActionButtonClick write FOnItemActionButtonClick; *)
  end;

procedure Register;

procedure DrawSwitch(ACanvas: TCanvas; ARect: TRectF; AChecked: Boolean; ASelectedColor: TAlphaColor);


implementation

uses SysUtils, FMX.Platform, Math, FMX.Forms, FMX.TextLayout, System.Math.Vectors,
  FMX.Ani, System.Threading;

type
  TksTableViewAccessoryImage = class(TBitmap)
  public
    procedure SetBitmap(ASource: TBitmap);
    procedure DrawToCanvas(ACanvas: TCanvas; ADestRect: TRectF);
  end;

  TksTableViewAccessoryImageList = class(TObjectList<TksTableViewAccessoryImage>)
  private
    FImageScale: integer;
    FImageMap: TBitmap;
    function GetAccessoryFromResource(AStyleName: string; const AState: string = ''): TksTableViewAccessoryImage;
    procedure Initialize;
    function GetAccessoryImage(AAccessory: TksAccessoryType): TksTableViewAccessoryImage;
  public
    constructor Create;
    destructor Destroy; override;
    property Images[AAccessory: TksAccessoryType]: TksTableViewAccessoryImage read GetAccessoryImage; default;
    property ImageMap: TBitmap read FImageMap;
  end;

var
  AccessoryImages: TksTableViewAccessoryImageList;
  ATextLayout: TTextLayout;
  _ScreenScale: single;
  AIsSwiping: Boolean;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksTableView]);
end;

// ------------------------------------------------------------------------------

function GetScreenScale: single;
var
  Service: IFMXScreenService;
begin
  if _ScreenScale > 0 then
  begin
    Result := _ScreenScale;
    Exit;
  end;
  Service := IFMXScreenService(TPlatformServices.Current.GetPlatformService
    (IFMXScreenService));

  Result := Service.GetScreenScale;

  {$IFDEF IOS}
  if Result < 2 then
   Result := 2;
  {$ENDIF}

  _ScreenScale := Result;

end;

function GetColorOrDefault(AColor, ADefaultIfNull: TAlphaColor): TAlphaColor;
begin
  Result := AColor;
  if Result = claNull then
    Result := ADefaultIfNull;
end;

// ------------------------------------------------------------------------------

procedure RenderText(ACanvas: TCanvas; x, y, AWidth, AHeight: single;
  AText: string; AFont: TFont; ATextColor: TAlphaColor; AWordWrap: Boolean;
  AHorzAlign: TTextAlign; AVertAlign: TTextAlign; ATrimming: TTextTrimming);
begin
  ATextLayout.BeginUpdate;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := AWordWrap;
  ATextLayout.Font.Assign(AFont);
  ATextLayout.Color := ATextColor;
  ATextLayout.HorizontalAlign := AHorzAlign;
  ATextLayout.VerticalAlign := AVertAlign;
  ATextLayout.Trimming := ATrimming;
  ATextLayout.TopLeft := PointF(x, y);
  ATextLayout.MaxSize := PointF(AWidth, AHeight);
  ATextLayout.EndUpdate;
  ATextLayout.RenderLayout(ACanvas);
end;

function GetTextSizeHtml(AText: string; AFont: TFont;
  const AWidth: single = 0): TPointF;
{$IFDEF USE_TMS_HTML_ENGINE}
var
  AnchorVal, StripVal, FocusAnchor: string;
  XSize, YSize: single;
  HyperLinks, MouseLink: integer;
  HoverRect: TRectF;
  ABmp: TBitmap;
{$ENDIF}
begin
  Result := PointF(0, 0);
{$IFDEF USE_TMS_HTML_ENGINE}
  XSize := AWidth;

  if XSize <= 0 then
    XSize := MaxSingle;

  ABmp := TBitmap.Create(10, 10);
  try
    ABmp.BitmapScale := GetScreenScale;
    ABmp.Canvas.Assign(AFont);
{$IFDEF USE_TMS_HTML_ENGINE}
    HTMLDrawEx(ABmp.Canvas, AText, RectF(0, 0, XSize, MaxSingle), 0, 0, 0, 0, 0,
      False, False, False, False, False, False, False, 1, claNull, claNull,
      claNull, claNull, AnchorVal, StripVal, FocusAnchor, XSize, YSize,
      HyperLinks, MouseLink, HoverRect, 1, nil, 1);
    Result := PointF(XSize, YSize);
{$ELSE}
    Result := PointF(0, 0);
{$ENDIF}
  finally
    FreeAndNil(ABmp);
  end;
{$ENDIF}
end;

function GetTextWidth(AText: string; AFont: TFont): single;
var
  APoint: TPointF;
begin
  ATextLayout.BeginUpdate;
  // Setting the layout MaxSize
  APoint.x := MaxSingle;
  APoint.y := 100;
  ATextLayout.MaxSize := APoint;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := False;
  ATextLayout.Font.Assign(AFont);
  ATextLayout.HorizontalAlign := TTextAlign.Leading;
  ATextLayout.EndUpdate;
  Result := ATextLayout.Width;
end;

function GetTextHeight(AText: string; AFont: TFont; AWordWrap: Boolean;
  const AWidth: single = 0): single;
var
  APoint: TPointF;
begin
  ATextLayout.BeginUpdate;
  // Setting the layout MaxSize
  APoint.x := MaxSingle;
  if AWidth > 0 then
    APoint.x := AWidth;
  APoint.y := 100;

  ATextLayout.MaxSize := APoint;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := AWordWrap;
  ATextLayout.Font.Assign(AFont);
  ATextLayout.HorizontalAlign := TTextAlign.Leading;
  ATextLayout.VerticalAlign := TTextAlign.Leading;
  ATextLayout.EndUpdate;
  Result := ATextLayout.TextHeight;
end;

procedure RenderHhmlText(ACanvas: TCanvas; x, y, AWidth, AHeight: single;
  AText: string; AFont: TFont; ATextColor: TAlphaColor; AWordWrap: Boolean;
  AHorzAlign: TTextAlign; AVertAlign: TTextAlign; ATrimming: TTextTrimming);
{$IFDEF USE_TMS_HTML_ENGINE}
var
  AnchorVal, StripVal, FocusAnchor: string;
  XSize, YSize: single;
  HyperLinks, MouseLink: integer;
  HoverRect: TRectF;
{$ENDIF}
begin
{$IFDEF USE_TMS_HTML_ENGINE}
  ACanvas.Fill.Color := ATextColor;
  ACanvas.Font.Assign(AFont);
  HTMLDrawEx(ACanvas, AText, RectF(x, y, x + AWidth, y + AHeight), 0, 0, 0, 0,
    0, False, False, True, False, False, False, AWordWrap, 1, claNull, claNull,
    claNull, claNull, AnchorVal, StripVal, FocusAnchor, XSize, YSize,
    HyperLinks, MouseLink, HoverRect, 1, nil, 1);
{$ELSE}
  AFont.Size := 10;
  RenderText(ACanvas, x, y, AWidth, AHeight, 'Requires TMS FMX', AFont,
    ATextColor, AWordWrap, AHorzAlign, AVertAlign, ATrimming);
{$ENDIF}
end;

procedure DrawSwitch(ACanvas: TCanvas; ARect: TRectF; AChecked: Boolean; ASelectedColor: TAlphaColor);
var
  ABmp: TBitmap;
  r: TRectF;
  ASwitchRect: TRectF;
  s: single;
begin
  s := 2;
  ABmp := TBitmap.Create(Round(ARect.Width * s), Round(ARect.Height * s));
  try
    ABmp.Clear(claNull);
    ABmp.BitmapScale := s;

    ABmp.Canvas.BeginScene;
    ABmp.Canvas.StrokeThickness := s*2;

    r := RectF(0, 0, ABmp.Height, ABmp.Height);
    InflateRect(r, -s, -s);
    if not AChecked then
      ASwitchRect := r;

    ABmp.Canvas.Stroke.Color := claSilver;

    //ABmp.Canvas.Fill.Color := claNull;
    ABmp.Canvas.Fill.Color := claWhite;
    if AChecked then
    begin
      Abmp.Canvas.Fill.Color := ASelectedColor;
      ABmp.Canvas.Stroke.Color := ASelectedColor;
    end
    else
      ABmp.Canvas.Fill.Color := $FFEEEEEE;

    ABmp.Canvas.FillEllipse(r, 1, ABmp.Canvas.Fill);
    ABmp.Canvas.DrawEllipse(r, 1, ABmp.Canvas.Stroke);
    OffsetRect(r, ABmp.Width-(r.Height+(s*2)), 0);
    if AChecked then
      ASwitchRect := r;

    ABmp.Canvas.FillEllipse(r, 1, ABmp.Canvas.Fill);
    ABmp.Canvas.DrawEllipse(r, 1, ABmp.Canvas.Stroke);

    if AChecked then
      Abmp.Canvas.Stroke.Color := ABmp.Canvas.Fill.Color;
    ABmp.Canvas.FillRect(RectF(0  + (r.Width/2), 0, ABmp.Width - (r.Width/2), ABmp.Height), 0, 0,  AllCorners, 1, ABmp.Canvas.Fill);

    r := RectF(ABmp.Height/2, 0, ABmp.Width-(ABmp.Height/2), ABmp.Height);


    ABmp.Canvas.FillRect(r, 0, 0, AllCorners, 1, ABmp.Canvas.Fill);
    ABmp.Canvas.StrokeThickness := s{*4}*2;
    r.Top := r.Top + s;
    r.Bottom := r.Bottom - s;
    r.Left := r.Left- (GetScreenScale*4);
    r.Right := r.Right + (GetScreenScale*4);
    ABmp.Canvas.DrawRectSides(r, 0, 0, AllCorners, 1, [TSide.Top, TSide.Bottom], ABmp.Canvas.Stroke);
    ABmp.Canvas.StrokeThickness := s{*2};

    ABmp.Canvas.Fill.Color := claWhite;

    if AChecked then
    begin

      //InflateRect(ASwitchRect, s/2, s/2);
      ABmp.Canvas.FillEllipse(ASwitchRect, 1);
    end
    else
    begin
      //InflateRect(ASwitchRect, s/2, s/2);
      //ABmp.Canvas.fil.Color := claWhite;
      ABmp.Canvas.Stroke.Color := claSilver;
      ABmp.Canvas.FillEllipse(ASwitchRect, 1);
      ABmp.Canvas.DrawEllipse(ASwitchRect, 1);
    end;
    ABmp.Canvas.EndScene;
    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ARect, 1, False);
  finally
    FreeAndNil(ABmp);
  end;
end;

// ------------------------------------------------------------------------------

procedure TksTableViewItemObject.Changed;
begin
  FTableItem.Cached := False;
end;

constructor TksTableViewItemObject.Create(ATableItem: TksTableViewItem);
begin
  inherited Create;
  FTableItem := ATableItem;
  FHitText := True;
  // FPadding := RectF(0, 0, 0, 0);
end;

function TksTableViewItemObject.GetAlign: TksTableItemAlign;
begin
  Result := FAlign;
end;

{function TksTableViewItemObject.GetHeight: single;
begin
  Result := FHeight;
end; }

function TksTableViewItemObject.GetID: string;
begin
  Result := FID;
end;

function TksTableViewItemObject.GetItemRect: TRectF;
begin
  Result := FTableItem.ItemRect;
end;

function TksTableViewItemObject.GetObjectRect: TRectF;
var
  ARowRect: TRectF;
begin
  ARowRect := GetItemRect;
  //ARowRect.Left := GetScreenScale*4;
  //ARowRect.Width := GetItemRect.Width-(GetScreenScale*8);
  //ARowRect.Right := ARowRect.Right - (GetScreenScale*8);

  {if Self <> FTableItem.Accessory then
  begin
    if (FTableItem.Accessory.Accessory <> atNone) then
      ARowRect.Right := ARowRect.Right - (8 + FTableItem.Accessory.Width);
  end;}

  Result := RectF(ARowRect.Left, 0, FWidth, FHeight);

  case FAlign of
    TksTableItemAlign.Center: OffsetRect(Result, ((ARowRect.Width - Result.Width) / 2), 0);
    TksTableItemAlign.Trailing: OffsetRect(Result, (ARowRect.Width - Result.Width), 0);
  end;

  case FVertAlign of
    TksTableItemAlign.Center: OffsetRect(Result, 0, (ARowRect.Height - Result.Height) / 2);
    TksTableItemAlign.Trailing: OffsetRect(Result, 0, (ARowRect.Height - Result.Height));
  end;

  OffsetRect(Result, FPlaceOffset.x, FPlaceOffset.y);
end;

function TksTableViewItemObject.GetVertAlign: TksTableItemAlign;
begin
  Result := FVertAlign;
end;


procedure TksTableViewItemObject.SetAlign(Value: TksTableItemAlign);
begin
  FAlign := Value;
end;

procedure TksTableViewItemObject.SetHeight(const Value: single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TksTableViewItemObject.SetHitTest(const Value: Boolean);
begin
  FHitTest := Value;
end;

procedure TksTableViewItemObject.SetID(Value: string);
begin
  FID := Value;
end;

procedure TksTableViewItemObject.SetPlaceOffset(const Value: TPointF);
begin
  FPlaceOffset := Value;
  Changed;
end;

procedure TksTableViewItemObject.SetVertAlign(Value: TksTableItemAlign);
begin
  FVertAlign := Value;
end;

procedure TksTableViewItemObject.SetWidth(const Value: single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

// ------------------------------------------------------------------------------

{ TksTableViewItemText }

constructor TksTableViewItemText.Create(ATableItem: TksTableViewItem);
begin
  inherited;
  FFont := TFont.Create;
  FFont.Size := C_TABLEVIEW_DEFAULT_FONT_SIZE;
  FText := '';
  FTextColor := claBlack;
  FVertAlign := TksTableItemAlign.Center;
  FTextAlign := TTextAlign.Leading;
  FTextVertAlign := TTextAlign.Leading;
  FWordWrap := False;
  FTrimming := TTextTrimming.Character;
  FBackground := claNull;
  FIsHtmlText := False;
end;

destructor TksTableViewItemText.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

function TksTableViewItemText.GetFont: TFont;
begin
  Result := FFont;
end;

function TksTableViewItemText.GetText: string;
begin
  Result := FText;
end;

function TksTableViewItemText.GetTextAlign: TTextAlign;
begin
  Result := FTextAlign;
end;

function TksTableViewItemText.GetTextColor: TAlphaColor;
begin
  Result := FTextColor;
end;

function TksTableViewItemText.GetTextVertAlign: TTextAlign;
begin
  Result := FTextVertAlign;
end;

function TksTableViewItemText.GetTrimming: TTextTrimming;
begin
  Result := FTrimming;
end;

procedure TksTableViewItemText.Render(ACanvas: TCanvas);
var
  r: TRectF;
begin
  r := GetObjectRect;
  RenderText(ACanvas, r.Left, r.Top, r.Width, r.Height, FText, FFont,
    FTextColor, FWordWrap, FTextAlign, FTextVertAlign, FTrimming);

  //ACanvas.DrawRect(r, 0, 0, AllCorners, 1);
end;

procedure TksTableViewItemText.SetBackground(const Value: TAlphaColor);
begin
  FBackground := Value;
  Changed;
end;

procedure TksTableViewItemText.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TksTableViewItemText.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TksTableViewItemText.SetTextAlign(const Value: TTextAlign);
begin
  FTextAlign := Value;
end;

procedure TksTableViewItemText.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
end;

procedure TksTableViewItemText.SetTextVertAlign(const Value: TTextAlign);
begin
  FTextVertAlign := Value;
end;

procedure TksTableViewItemText.SetTrimming(const Value: TTextTrimming);
begin
  FTrimming := Value;
end;

procedure TksTableViewItemText.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

// ------------------------------------------------------------------------------

{ TksTableViewItemObjects }

constructor TksTableViewItemObjects.Create(ATableView: TksTableView);
begin
  inherited Create(True);
  FTableView := ATableView;
end;

// ------------------------------------------------------------------------------

{ TksTableViewItemBaseImage }

constructor TksTableViewItemBaseImage.Create(ATableItem: TksTableViewItem);
begin
  inherited;
  ///FBitmap := TBitmap.Create;
  //FBitmap := nil;
  //FBitmap.BitmapScale := GetScreenScale;
  FOwnsBitmap := False;
end;

destructor TksTableViewItemBaseImage.Destroy;
begin
  //FreeAndNil(FBitmap);
  if FBitmap <> nil then
    FBitmap.Free;
  inherited;
end;
function TksTableViewItemBaseImage.GetBitmap: TBitmap;
begin
  case FOwnsBitmap of
    True: Result := FBitmap;
    False: Result := FExternalBitmap;
  end;
end;

{
function TksTableViewItemBaseImage.GetHeight: single;
begin
  Result := inherited;
  if Result = 0 then
    Result := FBitmap.Height;
  Result := Result / GetScreenScale;
end;

function TksTableViewItemBaseImage.GetWidth: single;
begin
  Result := inherited;
  if Result = 0 then
    Result := FBitmap.Width;
  Result := Result / GetScreenScale;
end;   }

procedure TksTableViewItemBaseImage.Render(ACanvas: TCanvas);
begin
  if Bitmap <> nil then
    ACanvas.DrawBitmap(Bitmap, RectF(0, 0, Bitmap.Width, Bitmap.Height), GetObjectRect, 1, True);
end;


procedure TksTableViewItemBaseImage.SetBitmap(const Value: TBitmap);
begin
  if FOwnsBitmap then
  begin
    if FBitmap = nil then
      FBitmap := TBitmap.Create;
    FBitmap.Assign(Value);
  end
  else
    FExternalBitmap := Value;
end;

procedure TksTableViewItemBaseImage.SetOwnsBitmap(const Value: Boolean);
begin
  if FOwnsBitmap <> Value then
    FOwnsBitmap := Value;
end;

// ------------------------------------------------------------------------------

{ TksTableViewItemAccessory }

constructor TksTableViewItemAccessory.Create(ATableItem: TksTableViewItem);
begin
  inherited;
  FAccessory := atNone;
  FAlign := TksTableItemAlign.Trailing;
  FVertAlign := TksTableItemAlign.Center;
end;

function TksTableViewItemAccessory.GetAccessory: TksAccessoryType;
begin
  Result := FAccessory;
end;
  {
function TksTableViewItemAccessory.GetHeight: single;
begin
  Result := inherited;
  Result := Result / AccessoryImages.FImageScale;
end;     }
      {
function TksTableViewItemAccessory.GetItemRect: TRectF;
begin
  Result := FTableItem.ItemRect;
end;    }


function TksTableViewItemAccessory.GetObjectRect: TRectF;
begin
  Result := inherited;
  //OffsetRect(Result, 0 - 4, 0);
end;
     {
function TksTableViewItemAccessory.GetWidth: single;
begin
  Result := inherited;
  if Result = 0 then
    Exit;
  Result := Result / AccessoryImages.FImageScale;
end;    }

procedure TksTableViewItemAccessory.SetAccessory(const Value: TksAccessoryType);
begin
  if FAccessory <> Value then
  begin
    FAccessory := Value;
    Bitmap := AccessoryImages.Images[FAccessory];
    FWidth := Bitmap.Width / AccessoryImages.FImageScale;
    FHeight := Bitmap.Height / AccessoryImages.FImageScale;
  end;
end;

// ------------------------------------------------------------------------------

{ TksTableViewItems }

function TksTableViewItems.AddItem(AText: string;
  const AAccessory: TksAccessoryType = atNone): TksTableViewItem;
begin
  Result := AddItem(AText, '', AAccessory);
end;

function TksTableViewItems.AddItem(AText, ADetail: string;
  const AAccessory: TksAccessoryType): TksTableViewItem;
begin
  Result := AddItem(AText, '', ADetail, AAccessory);
end;

function TksTableViewItems.AddHeader(AText: string): TksTableViewItem;
begin
  Result := TksTableViewItem.Create(FTableView);
  Result.Title.Text := AText;
  Result.Title.Font.Assign(FTableView.TextDefaults.Header.Font);
  Result.Title.TextColor := FTableView.TextDefaults.Header.TextColor;
  Result.SearchIndex := '';
  Result.Height := FTableView.HeaderHeight;
  Result.Purpose := TksTableViewItemPurpose.Header;
  Add(Result);
  FTableView.UpdateItemRects;
  UpdateIndexes;
  FTableView.UpdateScrollingLimits;
end;

function TksTableViewItems.AddItem(AText, ASubTitle, ADetail: string;
  const AAccessory: TksAccessoryType): TksTableViewItem;
begin
  Result := TksTableViewItem.Create(FTableView);
  Result.Title.Text := AText;
  Result.SubTitle.Text := ASubTitle;
  Result.Detail.Text := ADetail;
  Result.SearchIndex := AText;
  Result.Accessory.Accessory := AAccessory;
  if FTableView.CheckMarks <> TksTableViewCheckMarks.cmNone then
    Result.Accessory.Accessory := atCheckBox;
  Result.Height := FTableView.ItemHeight;
  Add(Result);
  FTableView.UpdateItemRects;
  UpdateIndexes;
  FTableView.UpdateScrollingLimits;
end;

constructor TksTableViewItems.Create(ATableView: TksTableView;
  AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  // FItems := TObjectList<TksTableItem>.Create(AOwnsObjects);
  FTableView := ATableView;
end;

procedure TksTableViewItems.DeleteItem(AItem: TksTableViewItem);
var
  ICount: integer;
  ACanDelete: Boolean;
begin
  ACanDelete := True;
  if Assigned(FTableView.OnDeletingItem) then
  begin
    ACanDelete := False;
    FTableView.OnDeletingItem(FTableView, AItem, ACanDelete);
    if ACanDelete = False then
      Exit;
  end;

  for Icount := Count-1 downto 0 do
  begin
    if AItem = Items[ICount] then
    begin
      if Assigned(FTableView.OnDeleteItem) then
        FTableView.OnDeleteItem(FTableView, Items[ICount]);
      Delete(ICount);
      FTableView.UpdateItemRects;
      FTableView.UpdateScrollingLimits;
      FTableView.Invalidate;
      Exit;
    end;
  end;
end;

function TksTableViewItems.GetLastItem: TksTableViewItem;
begin
  Result := nil;
  if Count > 0 then
    Result := Items[Count - 1];
end;

function TksTableViewItems.GetTotalItemHeight: single;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to Count - 1 do
  begin
    Result := Result + Items[ICount].Height;
  end;
end;

procedure TksTableViewItems.UpdateIndexes;
var
  ICount: integer;
begin
  for ICount := 0 to Count - 1 do
    Items[ICount].FAbsoluteIndex := ICount;
end;

// ------------------------------------------------------------------------------

{ TksTableItem }

function TksTableViewItem.AddSwitch(x: single; AIsChecked: Boolean;
  const AAlign: TksTableItemAlign = TksTableItemAlign.Trailing): TksTableViewItemSwitch;
begin
  Result := TksTableViewItemSwitch.Create(Self);
  Result.Width := 50;
  Result.Height := 30;
  Result.PlaceOffset := PointF(x, 0);
  Result.SelectedColor := C_TABLEIEW_DEFAULT_SWITCH_COLOR;
  Result.Checked := AIsChecked;
  Result.Align := AAlign;
  Result.VertAlign := TksTableItemAlign.Center;
  FObjects.Add(Result);
end;

procedure TksTableViewItem.CacheItem(const AForceCache: Boolean = False);
var
  ARect: TRectF;
  ICount: integer;
begin
  if (FUpdating) or (FTableView.UpdateCount > 0) then
    Exit;
  if (FItemRect.Height = 0) or (FItemRect.Width = 0) then
    Exit;
  if AForceCache then
  begin
    FreeAndNil(FBitmap);
    FCached := False;
  end;

  if (FCached = True) or (FCaching) then
    Exit;

  FCaching := True;

  RealignStandardObjects;

  if FBitmap = nil then
    FBitmap := TBitmap.Create(Round(FItemRect.Width * GetScreenScale), Round(FItemRect.Height * GetScreenScale))
  else
    FBitmap.SetSize(Round(FItemRect.Width * GetScreenScale), Round(FItemRect.Height * GetScreenScale));


  FBitmap.BitmapScale := GetScreenScale;

  FBitmap.Clear(claWhite);

  if FTableView.Appearence.AlternatingItemBackground <> claNull then
  begin
    if FIndex mod 2 = 0 then
      FBitmap.Clear($FFFAFAFA);
  end;

  if (FTableView.ShowSelection) and (FCanSelect) then
  begin
    if (FIndex = FTableView.ItemIndex) or (Checked)  then
      FBitmap.Clear(GetColorOrDefault(FTableView.Appearence.SelectedColor, C_TABLEVIEW_DEFAULT_HEADER_COLOR));
  end;

  if (FPurpose <> None) then
    FBitmap.Clear(GetColorOrDefault(FTableView.Appearence.HeaderColor,
      C_TABLEVIEW_DEFAULT_HEADER_COLOR));

  ARect := RectF(0, 0, FBitmap.Width, FBitmap.Height);

  FBitmap.Canvas.BeginScene;
  try
    if Assigned(FTableView.BeforeRowCache) then
      FTableView.BeforeRowCache(FTableView, FBitmap.Canvas, Self, ARect);

    case FTableView.RowIndicators.Outlined of
      False: FIndicator.Stroke.Kind := TBrushKind.None;
      True: FIndicator.Stroke.Kind := TBrushKind.Solid;
    end;
    FIndicator.Render(FBitmap.Canvas);
    FImage.Render(FBitmap.Canvas);
    FTitle.Render(FBitmap.Canvas);
    FSubTitle.Render(FBitmap.Canvas);
    FDetail.Render(FBitmap.Canvas);

    if FTableView.CheckMarks <> TksTableViewCheckMarks.cmNone then
    begin
      case FChecked of
        True:
          FAccessory.Accessory := atCheckBoxChecked;
        False:
          FAccessory.Accessory := atCheckBox;
      end;
    end;
    FAccessory.Render(FBitmap.Canvas);

    for ICount := 0 to FObjects.Count - 1 do
      FObjects[ICount].Render(FBitmap.Canvas);

    if Assigned(FTableView.AfterRowCache) then
      FTableView.AfterRowCache(FTableView, FBitmap.Canvas, Self, ARect);

  finally
    FBitmap.Canvas.EndScene;
    FCached := True;
  end;
  FCaching := False;
end;

procedure TksTableViewItem.Changed;
begin
  FCached := False;
  if FTableView.FUpdateCount = 0 then
    FTableView.Invalidate;
end;

constructor TksTableViewItem.Create(ATableView: TksTableView);
begin
  inherited Create(nil);
  FTableView := ATableView;

  FObjects := TksTableViewItemObjects.Create(FTableView);
  FFont := TFont.Create;
  FFont.Size := C_TABLEVIEW_DEFAULT_FONT_SIZE;
  FIndicator := TksTableViewItemShape.Create(Self);
  FIndicator.VertAlign := TksTableItemAlign.Center;
  FActionButtons := TksTableViewActionButtons.Create(Self);

  FImage := TksTableViewItemImage.Create(Self);
  FImage.VertAlign := TksTableItemAlign.Center;

  FAccessory := TksTableViewItemAccessory.Create(Self);

  FTitle := TksTableViewItemText.Create(Self);
  FTitle.Font.Assign(FTableView.TextDefaults.Title.Font);
  FTitle.TextColor := FTableView.TextDefaults.Title.TextColor;

  FSubTitle := TksTableViewItemText.Create(Self);
  FSubTitle.Font.Assign(FTableView.TextDefaults.SubTitle.Font);
  FSubTitle.TextColor := FTableView.TextDefaults.SubTitle.TextColor;

  FDetail := TksTableViewItemText.Create(Self);
  FDetail.TextAlignment := TTextAlign.Trailing;
  FDetail.Font.Assign(FTableView.TextDefaults.Detail.Font);
  FDetail.TextColor := FTableView.TextDefaults.Detail.TextColor;

  FPurpose := None;
  FTextColor := claBlack;
  FChecked := False;
  Height := C_TABLEVIEW_DEFAULT_ITEM_HEIGHT;
  FCaching := False;
  FUpdating := False;
  FCanSelect := True;
  FTitleWidth := ksWidth60Percent;
  FTagString := '';
  FTagInteger := 0;
end;

destructor TksTableViewItem.Destroy;
begin
  FreeAndNil(FIndicator);
  FreeAndNil(FAccessory);
  FreeAndNil(FTitle);
  FreeAndNil(FSubTitle);
  FreeAndNil(FDetail);
  FreeAndNil(FObjects);
  FreeAndNil(FBitmap);
  FreeAndNil(FFont);
  FreeAndNil(FImage);
  FreeAndNil(FActionButtons);
  inherited;
end;

procedure TksTableViewItem.DoClick(x, y: single);
var
  ABtn: TksTableViewActionButon;
begin
  if FActionButtons.Visible then
  begin
    // check for actionbutton click...
    ABtn := FActionButtons.ButtonFromXY(x, y);

    if ABtn <> nil then
    begin
      if ABtn.IsDeleteButton then
      begin
        FTableView.Items.DeleteItem(Self);
        FTableView.Invalidate;
        //FActionButtons.HideButtons(False);
        Exit;
      end;
      FActionButtons.HideButtons(False);
      // custom button...
      if Assigned(FTableView.OnItemActionButtonClick) then
        FTableView.OnItemActionButtonClick(FTableView, Self, ABtn);
      Exit;
    end;
    FActionButtons.HideButtons(False);
    Exit;
  end;
  FActionButtons.HideButtons(False);
  if FTableView.CheckMarks <> TksTableViewCheckMarks.cmNone then
    Checked := not Checked;
end;

procedure TksTableViewItem.DoSwipe(ADirecton: TksSwipeDirection);
begin
  if AIsSwiping then
    Exit;
  AIsSwiping := True;
  try
    if FActionButtons.Visible then
    begin
      if (FActionButtons.FAlignment = abLeftActionButtons) and (ADirecton = ksSwipeRightToLeft) then Self.fActionButtons.HideButtons(False);
      if (FActionButtons.FAlignment = abRightActionButtons) and (ADirecton = ksSwipeLeftToRight) then Self.fActionButtons.HideButtons(False);
      Exit;
    end;

    FTableView.HideAllActionButtons(True);

    FActionButtons.Clear;
    if Assigned(FTableView.FOnItemSwipe) then
      FTableView.FOnItemSwipe(FTableView, Self, ADirecton, Self.fActionButtons);
   // FActionButtons.Add(nil);

   if (FTableView.DeleteButton.Enabled) and (ADirecton = TksSwipeDirection.ksSwipeRightToLeft) then
   begin
      FActionButtons.AddButton(FTableView.DeleteButton.Text,
                               FTableView.DeleteButton.Color,
                               FTableView.DeleteButton.TextColor,
                               FTableView.DeleteButton.Width).FIsDeleteButton := True;
   end;

    if ADirecton = ksSwipeRightToLeft then FActionButtons.FAlignment := TksTableViewActionButtonAlignment.abRightActionButtons;
    if ADirecton = ksSwipeLeftToRight then FActionButtons.FAlignment := TksTableViewActionButtonAlignment.abLeftActionButtons;
    Self.fActionButtons.ShowButtons;
  finally
    AIsSwiping := False;
  end;

end;


function TksTableViewItem.DrawBitmap(ABmp: TBitmap; x, AWidth, AHeight: single)
  : TksTableViewItemImage;
begin
  Result := DrawBitmap(ABmp, x, 0, AWidth, AHeight);
end;

function TksTableViewItem.DrawBitmap(ABmp: TBitmap;
  x, y, AWidth, AHeight: single): TksTableViewItemImage;
begin
  Result := TksTableViewItemImage.Create(Self);
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.PlaceOffset := PointF(x, y);
  Result.VertAlign := TksTableItemAlign.Center;
  Result.Bitmap := ABmp;
  FObjects.Add(Result);
end;

function TksTableViewItem.DrawRect(x, y, AWidth, AHeight: single;
  AStroke, AFill: TAlphaColor): TksTableViewItemShape;
begin
  Result := TksTableViewItemShape.Create(Self);
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.PlaceOffset := PointF(x, y);
  Result.Stroke.Color := AStroke;
  Result.Fill.Color := AFill;
  Result.VertAlign := TksTableItemAlign.Center;
  FObjects.Add(Result);
end;

function TksTableViewItem.GetAbsoluteIndex: integer;
begin
  Result := FAbsoluteIndex;
end;


function TksTableViewItem.GetCached: Boolean;
begin
  Result := FCached;
end;


function TksTableViewItem.GetHeight: single;
begin
  Result := FHeight;
end;

function TksTableViewItem.GetIndex: integer;
begin
  Result := FIndex;
end;

function TksTableViewItem.GetIndicatorColor: TAlphaColor;
begin
  Result := FIndicator.Fill.Color;
end;

function TksTableViewItem.GetInternalRect: TRectF;
var
  ADefaultAccessoryWidth: integer;
begin
  //ADefaultAccessoryWidth := AccessoryImages.Images[atMore].Width;
  Result := GetItemRect;


  Result.Left := Result.Left + 4;
  Result.Right := Result.Right - 4;

  if FAccessory.Accessory <> atNone then
    Result.Right := Result.Right - FAccessory.Width;
  //Result.Right := Result.Right - (FAccessory.Width + 10);
end;

{
  function TksTableItem.GetInternalRect: TRectF;
  begin
  Result := FItemRect;
  Result.Left := 8;
  Result.Right := Result.Right-(C_SCROLL_BAR_WIDTH);
  if FTableView.ShowAccessory then
  Result.Right := Result.Right - 20;
  end; }

function TksTableViewItem.GetItemRect: TRectF;
begin
  Result := FItemRect;

end;

function TksTableViewItem.GetPurpose: TksTableViewItemPurpose;
begin
  Result := FPurpose;
end;

function TksTableViewItem.GetSearchIndex: string;
begin
  Result := FSearchIndex;
end;

function TksTableViewItem.IsLastItem: Boolean;
begin
  Result := Self = FTableView.Items.LastItem;
end;

function TksTableViewItem.IsVisible(AViewport: TRectF): Boolean;
begin
  Result := (FItemRect.Bottom >= (AViewport.Top)) and
    (FItemRect.Top <= (AViewport.Bottom));
end;

function TksTableViewItem.MatchesSearch(AFilter: string): Boolean;
begin
  Result := True;
  if AFilter <> '' then
    Result := Pos(LowerCase(AFilter), LowerCase(FSearchIndex)) > 0;
end;

procedure TksTableViewItem.RealignStandardObjects;
var
  ARect: TRectF;
  ALeftMargin: single;
  ATitleWidth: single;
begin
  FUpdating := True;
  try
    ARect := GetInternalRect;

    if (FPurpose = None) and (FTableView.RowIndicators.Visible) then
    begin
      FIndicator.PlaceOffset := PointF(ARect.Left, 0);
      FIndicator.Width := FTableView.RowIndicators.Width;
      FIndicator.Height := FTableView.RowIndicators.Height;
      if FIndicator.Height = 0 then
        FIndicator.Height := ItemRect.Height - 16;
      ARect.Left := ARect.Left + FTableView.RowIndicators.Width+4;
    end;

    if FImage.Bitmap <> nil then
    begin
      FImage.Width := FTableView.ItemImageSize;
      FImage.Height := FTableView.ItemImageSize;
      FImage.PlaceOffset := PointF(ARect.Left, 0);
      ARect.Left := ARect.Left + FTableView.ItemImageSize + 4;
    end;

    if FAccessory.Accessory <> atNone then
    begin
      ARect.Right := ARect.Right-4;
      FAccessory.PlaceOffset := Point(-4, 0);
    end;

    FTitle.PlaceOffset := PointF(ARect.Left, 0);
    FTitle.Width := ARect.Width/2;
    FTitle.Height := TextHeight(FTitle.Text, False, False, 0);

    FSubTitle.PlaceOffset := PointF(ARect.Left, 0);
    FSubTitle.Width := ARect.Width/2;
    FSubTitle.Height := TextHeight(FSubTitle.Text, False, False, 0);

    if FSubTitle.Text <> '' then
    begin
      FTitle.PlaceOffset := PointF(FTitle.PlaceOffset.x, -9);
      FSubTitle.PlaceOffset := PointF(FSubTitle.PlaceOffset.x, 9);
    end;



    FDetail.PlaceOffset := PointF(ARect.Right-(ARect.Width/2), 0);
    FDetail.Width := ARect.Width/2;
    FDetail.Height := GetTextHeight(FDetail.Text, FDetail.Font, FDetail.WordWrap);

     //PointF(ARect.Right-(FAccessory.Width+4), 0);
    (*ALeftMargin := ARect.Left;

    [if (FPurpose = None) and (FTableView.RowIndicators.Visible) then
    begin
      FIndicator.PlaceOffset := PointF(ALeftMargin, 0);
      FIndicator.Width := FTableView.RowIndicators.Width;
      FIndicator.Height := FTableView.RowIndicators.Height;
      if FIndicator.Height = 0 then
        FIndicator.Height := ItemRect.Height - 16;
      ALeftMargin := ALeftMargin + FTableView.RowIndicators.Width+4;
    end;

    if FImage.Bitmap <> nil then
    begin
      FImage.Width := FTableView.ItemImageSize;
      FImage.Height := FTableView.ItemImageSize;
      FImage.PlaceOffset := PointF(4, 0);
      ALeftMargin := ALeftMargin + FTableView.ItemImageSize {+ 4};
    end;

    ATitleWidth := ((Ord(FTitleWidth)+1)*10) / 100;

    FTitle.PlaceOffset := PointF(ALeftMargin, 0);

    FTitle.Width := (ARect.Width * ATitleWidth) - ALeftMargin;
    FTitle.Height := TextHeight(FTitle.Text, False, False, 0);

    FSubTitle.PlaceOffset := PointF(ALeftMargin, 0);
    FSubTitle.Width := (ARect.Width * ATitleWidth) - ALeftMargin;
    FSubTitle.Height := TextHeight(FSubTitle.Text, False, False, 0);

    if FSubTitle.Text <> '' then
    begin
      FTitle.PlaceOffset := PointF(FTitle.PlaceOffset.x, -9);
      FSubTitle.PlaceOffset := PointF(FSubTitle.PlaceOffset.x, 9);
    end;

    FDetail.PlaceOffset := PointF(FTitle.GetObjectRect.Right, 0);
    FDetail.Width := (ARect.Width * (1-ATitleWidth));
    FDetail.Height := GetTextHeight(FDetail.Text, FDetail.Font,
      FDetail.WordWrap);
           *)
  finally
    FUpdating := False;
  end;
end;

procedure TksTableViewItem.Render(ACanvas: TCanvas; AScrollPos: single);
var
  ARect: TRectF;
  AButtonRect: TRectF;
  AWidth: single;
  ASeperatorMargin: single;
begin
  CacheItem;
  if FBitmap = nil then
    Exit;
  ARect := FItemRect;
  OffsetRect(ARect, 0, 0 - AScrollPos);
  if FActionButtons.Visible = False then
    ACanvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), ARect, 1, True)
  else
  begin
    AWidth := (FActionButtons.TotalWidth / 100) * FActionButtons.PercentWidth;

    case FActionButtons.FAlignment of
      abLeftActionButtons:
      begin

        OffsetRect(ARect, AWidth, 0);
        AButtonRect := RectF(0, ARect.Top, AWidth, ARect.Bottom);
        FActionButtons.Render(ACanvas, AButtonRect);
      end;
      abRightActionButtons:
      begin
        OffsetRect(ARect, 0-AWidth, 0);
        AButtonRect := RectF(ARect.Right, ARect.Top, FItemRect.Right, ARect.Bottom);
        FActionButtons.Render(ACanvas, AButtonRect);
      end;
    end;
    ACanvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), ARect, 1, True);
  end;

  // seperator...
  ACanvas.Stroke.Color := FTableView.Appearence.SeparatorColor;
  ACanvas.StrokeThickness := 1;
  ACanvas.Stroke.Kind := TBrushKind.Solid;
  if FPurpose = Header then
  begin
    ACanvas.Stroke.Color := $FFD2D2D2;
    ACanvas.StrokeThickness := 0.5;
  end;
  ASeperatorMargin := 0;
  {if (not FTableView.FullWidthSeparator) and (FPurpose = TksTableViewItemPurpose.None) then
    ASeperatorMargin := FTitle.PlaceOffset.X;
  ACanvas.DrawLine(PointF(ASeperatorMargin, ARect.Top), PointF(ARect.Right, ARect.Top), 1);
  if (IsLastItem) or (FPurpose = TksTableViewItemPurpose.Header) then
    ACanvas.DrawLine(PointF(0, ARect.Bottom),
      PointF(ARect.Right, ARect.Bottom), 1);}
end;

procedure TksTableViewItem.SetCached(const Value: Boolean);
begin
  FCached := Value;
end;

procedure TksTableViewItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Changed;
    //if Assigned(FTableView.OnItemCheckmarkChanged) then
    //  FTableView.OnItemCheckmarkChanged(Self, Self, FChecked);
  end;
end;

procedure TksTableViewItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TksTableViewItem.SetHeight(const Value: single);
begin
  FHeight := Value;
end;

procedure TksTableViewItem.SetIndex(const Value: integer);
begin
  FIndex := Value;
end;

procedure TksTableViewItem.SetIndicatorColor(const Value: TAlphaColor);
begin
  FIndicator.Fill.Color := Value;
  FCached := False;
  CacheItem;
end;

procedure TksTableViewItem.SetItemFontStyle(AFontStyle: TFontStyles);
var
  ICount: integer;
begin
  FFont.Style := AFontStyle;
  for ICount := 0 to FObjects.Count - 1 do
  begin
    if (FObjects[ICount] is TksTableViewItemText) then
      (FObjects[ICount] as TksTableViewItemText).Font.Style := AFontStyle;
  end;
  FTitle.Font.Style := AFontStyle;
  FSubTitle.Font.Style := AFontStyle;
  FDetail.Font.Style := AFontStyle;
end;

procedure TksTableViewItem.SetItemRect(const Value: TRectF);
begin
  FItemRect := Value;
  // if FItemRect.Height >  then
  Changed;
end;

procedure TksTableViewItem.SetItemTextColor(AColor: TAlphaColor);
var
  ICount: integer;
begin
  FTextColor := AColor;
  for ICount := 0 to FObjects.Count - 1 do
  begin
    if (FObjects[ICount] is TksTableViewItemText) then
      (FObjects[ICount] as TksTableViewItemText).TextColor := AColor;
  end;
  FTitle.TextColor := AColor;
  FSubTitle.TextColor := AColor;
  FDetail.TextColor := AColor;
  Changed;
end;

procedure TksTableViewItem.SetPurpose(const Value: TksTableViewItemPurpose);
begin
  if FPurpose <> Value then
  begin
    FPurpose := Value;
    Changed;
  end;
end;

procedure TksTableViewItem.SetSearchIndex(const Value: string);
begin
  FSearchIndex := Value;
end;

procedure TksTableViewItem.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
end;

procedure TksTableViewItem.SetTitleWidth(const Value: TksTableViewTextWidth);
begin
  if FTitleWidth <> Value then
  begin
    FTitleWidth := Value;
    Changed;
  end;
end;

// ------------------------------------------------------------------------------

function TksTableViewItem.TextBox(AText: string; ARect: TRectF;
  ATextAlign, ATextLayout: TTextAlign; const ABackground: TAlphaColor)
  : TksTableViewItemText;
begin
  FUpdating := True;
  try
    Result := TextOut(AText, ARect.Left, ARect.Top, ARect.Width,
      TksTableItemAlign.Leading, True);
    Result.Background := ABackground;
    Result.Height := ARect.Height;
    Result.TextAlignment := ATextAlign;
    Result.TextVertAlign := ATextLayout;
  finally
    FUpdating := False;
  end;
  Changed;
end;

function TksTableViewItem.TextBoxHtml(AText: string; ARect: TRectF)
  : TksTableViewItemText;
begin
  FUpdating := True;
  try
    Result := TextOut(AText, ARect.Left, ARect.Top, ARect.Width,
      TksTableItemAlign.Leading, True);
    Result.FIsHtmlText := True;
    Result.Height := ARect.Height;
  finally
    FUpdating := False;
  end;
  Changed;
end;

function TksTableViewItem.TextHeight(AText: string; AWordWrap, AIsHtml: Boolean;
  const AMaxWidth: single): single;
begin
  if AIsHtml then
    Result := GetTextSizeHtml(AText, FFont, AMaxWidth).y
  else
    Result := GetTextHeight(AText, FFont, AWordWrap, AMaxWidth);
end;

function TksTableViewItem.TextOut(AText: string; x: single;
  const AVertAlign: TksTableItemAlign; const AWordWrap: Boolean)
  : TksTableViewItemText;
var
  AWidth: single;
begin
  AWidth := TextWidth(AText, False);
  Result := TextOut(AText, x, AWidth, AVertAlign, AWordWrap);
end;

function TksTableViewItem.TextOut(AText: string; x, y, AWidth: single;
  const AVertAlign: TksTableItemAlign; const AWordWrap: Boolean)
  : TksTableViewItemText;
var
  AHeight: single;
begin
  Result := TksTableViewItemText.Create(Self);

  Result.Font.Assign(Font);
  Result.FPlaceOffset := PointF(x, y);

  AHeight := GetTextHeight(AText, FFont, AWordWrap, AWidth);

  if AWidth = 0 then
    AWidth := GetTextWidth(AText, Font);

  Result.Width := AWidth;
  Result.Height := AHeight;

  Result.VertAlign := AVertAlign;
  Result.TextAlignment := TTextAlign.Leading;
  Result.TextColor := FTextColor;
  Result.Text := AText;
  Result.WordWrap := AWordWrap;
  if SearchIndex = '' then
    SearchIndex := AText;
  FObjects.Add(Result);
  Changed;
end;

function TksTableViewItem.TextOut(AText: string; x, AWidth: single;
  const AVertAlign: TksTableItemAlign; const AWordWrap: Boolean)
  : TksTableViewItemText;
begin
  Result := TextOut(AText, x, 0, AWidth, AVertAlign, AWordWrap);
end;

function TksTableViewItem.TextOutRight(AText: string;
  y, AWidth, AXOffset: single; const AVertAlign: TksTableItemAlign)
  : TksTableViewItemText;
begin
  Result := TextOut(AText, AXOffset, y, AWidth, AVertAlign);
  Result.Align := TksTableItemAlign.Trailing;
  Result.TextAlignment := TTextAlign.Trailing;
end;

function TksTableViewItem.TextWidth(AText: string; AIsHtml: Boolean): single;
begin
  if AIsHtml then
    Result := GetTextSizeHtml(AText, FFont, 0).x
  else
    Result := GetTextWidth(AText, FFont);
end;

// ------------------------------------------------------------------------------

{ TksTableViewAppearence }

constructor TksTableViewAppearence.Create(AListView: TksTableView);
begin
  inherited Create;
  FListView := AListView;
  FBackground := claWhite;
  FItemBackground := claWhite;
  FSeparatorColor := $FFF0F0F0;
  FSelectedColor := C_TABLEVIEW_DEFAULT_SELECTED_COLOR;
  FAlternatingItemBackground := claNull;
end;

procedure TksTableViewAppearence.SetAlternatingItemBackground
  (const Value: TAlphaColor);
begin
  FAlternatingItemBackground := Value;
end;

procedure TksTableViewAppearence.SetBackground(const Value: TAlphaColor);
begin
  FBackground := Value;
end;

procedure TksTableViewAppearence.SetHeaderColor(const Value: TAlphaColor);
begin
  FHeaderColor := Value;
end;

procedure TksTableViewAppearence.SetItemBackground(const Value: TAlphaColor);
begin
  FItemBackground := Value;
end;

procedure TksTableViewAppearence.SetSelectedColor(const Value: TAlphaColor);
begin
  FSelectedColor := Value;
end;

procedure TksTableViewAppearence.SetSeparatorBackground
  (const Value: TAlphaColor);
begin
  FSeparatorColor := Value;
end;

// ------------------------------------------------------------------------------

{ TksTableViewAccessoryImageList }

function TksTableViewAccessoryImageList.GetAccessoryImage
  (AAccessory: TksAccessoryType): TksTableViewAccessoryImage;
begin
  if Count = 0 then
    Initialize;
  Result := Items[Ord(AAccessory)];
end;

constructor TksTableViewAccessoryImageList.Create;
begin
  inherited Create(True);
  FImageMap := TBitmap.Create;
end;

destructor TksTableViewAccessoryImageList.Destroy;
begin
  FreeAndNil(FImageMap);
  inherited;
end;

function TksTableViewAccessoryImageList.GetAccessoryFromResource
  (AStyleName: string; const AState: string = ''): TksTableViewAccessoryImage;
var
  ActiveStyle: TFmxObject;
  AStyleObj: TStyleObject;
  AImgRect: TBounds;
  AIds: TStrings;
  r: TRectF;
  ABitmapLink: TBitmapLinks;
  AImageMap: TBitmap;
begin
  FImageScale := 1;
  if GetScreenScale >= 2 then
    FImageScale := 2;
{$IFDEF MSWINDOWS}
  FImageScale := 1;
{$ENDIF}
  Result := TksTableViewAccessoryImage.Create;
  AIds := TStringList.Create;
  try
    AIds.Text := StringReplace(AStyleName, '.', #13, [rfReplaceAll]);
    ActiveStyle := TStyleManager.ActiveStyle(Nil);

    AStyleObj := TStyleObject(ActiveStyle);

    while AIds.Count > 0 do
    begin
      AStyleObj := TStyleObject(AStyleObj.FindStyleResource(AIds[0]));
      AIds.Delete(0);
    end;

    if AStyleObj <> nil then
    begin
      if FImageMap.IsEmpty then
      begin
        AImageMap := ((AStyleObj as TStyleObject).Source.MultiResBitmap.Bitmaps
          [FImageScale]);

        FImageMap.SetSize(Round(AImageMap.Width), Round(AImageMap.Height));
        FImageMap.Clear(claNull);

        FImageMap.Canvas.BeginScene;
        try
          FImageMap.Canvas.DrawBitmap(AImageMap, RectF(0, 0, AImageMap.Width,
            AImageMap.Height), RectF(0, 0, FImageMap.Width,
            FImageMap.Height), 1, True);
        finally
          FImageMap.Canvas.EndScene;
        end;
      end;

      ABitmapLink := nil;
      if AStyleObj = nil then
        Exit;
      if (AStyleObj.ClassType = TCheckStyleObject) then
      begin
        if AState = 'checked' then
          ABitmapLink := TCheckStyleObject(AStyleObj).ActiveLink
        else
          ABitmapLink := TCheckStyleObject(AStyleObj).SourceLink

      end;

      if ABitmapLink = nil then
        ABitmapLink := AStyleObj.SourceLink;

{$IFDEF XE8_OR_NEWER}
      AImgRect := ABitmapLink.LinkByScale(FImageScale, True).SourceRect;
{$ELSE}
      AImgRect := ABitmapLink.LinkByScale(FImageScale).SourceRect;
{$ENDIF}
      Result.SetSize(Round(AImgRect.Width), Round(AImgRect.Height));
      Result.Clear(claNull);
      Result.Canvas.BeginScene;

      r := AImgRect.Rect;

      Result.Canvas.DrawBitmap(FImageMap, r, RectF(0, 0, Result.Width,
        Result.Height), 1, True);
      Result.Canvas.EndScene;
    end;
  finally
{$IFDEF NEXTGEN}
    FreeAndNil(AIds);
{$ELSE}
    AIds.Free;
{$ENDIF}
  end;
end;

procedure TksTableViewAccessoryImageList.Initialize;
var
  ICount: TksAccessoryType;
begin
  for ICount := Low(TksAccessoryType) to High(TksAccessoryType) do
  begin
    case ICount of
      atNone: Add(GetAccessoryFromResource('none'));
      atMore: Add(GetAccessoryFromResource('listviewstyle.accessorymore'));
      atCheckmark: Add(GetAccessoryFromResource('listviewstyle.accessorycheckmark'));
      atDetail: Add(GetAccessoryFromResource('listviewstyle.accessorydetail'));
      atBack: Add(GetAccessoryFromResource('backtoolbutton.icon'));
      atRefresh: Add(GetAccessoryFromResource('refreshtoolbutton.icon'));
      atAction: Add(GetAccessoryFromResource('actiontoolbutton.icon'));
      atPlay: Add(GetAccessoryFromResource('playtoolbutton.icon'));
      atRewind: Add(GetAccessoryFromResource('rewindtoolbutton.icon'));
      atForward: Add(GetAccessoryFromResource('forwardtoolbutton.icon'));
      atPause: Add(GetAccessoryFromResource('pausetoolbutton.icon'));
      atStop: Add(GetAccessoryFromResource('stoptoolbutton.icon'));
      atAdd: Add(GetAccessoryFromResource('addtoolbutton.icon'));
      atPrior: Add(GetAccessoryFromResource('priortoolbutton.icon'));
      atNext: Add(GetAccessoryFromResource('nexttoolbutton.icon'));
      atArrowUp: Add(GetAccessoryFromResource('arrowuptoolbutton.icon'));
      atArrowDown: Add(GetAccessoryFromResource('arrowdowntoolbutton.icon'));
      atArrowLeft: Add(GetAccessoryFromResource('arrowlefttoolbutton.icon'));
      atArrowRight: Add(GetAccessoryFromResource('arrowrighttoolbutton.icon'));
      atReply: Add(GetAccessoryFromResource('replytoolbutton.icon'));
      atSearch: Add(GetAccessoryFromResource('searchtoolbutton.icon'));
      atBookmarks: Add(GetAccessoryFromResource('bookmarkstoolbutton.icon'));
      atTrash: Add(GetAccessoryFromResource('trashtoolbutton.icon'));
      atOrganize: Add(GetAccessoryFromResource('organizetoolbutton.icon'));
      atCamera: Add(GetAccessoryFromResource('cameratoolbutton.icon'));
      atCompose: Add(GetAccessoryFromResource('composetoolbutton.icon'));
      atInfo: Add(GetAccessoryFromResource('infotoolbutton.icon'));
      atPagecurl: Add(GetAccessoryFromResource('pagecurltoolbutton.icon'));
      atDetails: Add(GetAccessoryFromResource('detailstoolbutton.icon'));
      atRadioButton: Add(GetAccessoryFromResource('radiobuttonstyle.background'));
      atRadioButtonChecked: Add(GetAccessoryFromResource('radiobuttonstyle.background', 'checked'));
      atCheckBox: Add(GetAccessoryFromResource('checkboxstyle.background'));
      atCheckBoxChecked: Add(GetAccessoryFromResource('checkboxstyle.background', 'checked'));
      atUserDefined1: Add(GetAccessoryFromResource('userdefined1'));
      atUserDefined2: Add(GetAccessoryFromResource('userdefined2'));
      atUserDefined3: Add(GetAccessoryFromResource('userdefined3'));
    end;
  end;
end;

// ------------------------------------------------------------------------------

{ TksAccessoryImage }

procedure TksTableViewAccessoryImage.DrawToCanvas(ACanvas: TCanvas;
  ADestRect: TRectF);
begin
  ACanvas.DrawBitmap(Self, RectF(0, 0, Width, Height), ADestRect, 1, True);
end;

procedure TksTableViewAccessoryImage.SetBitmap(ASource: TBitmap);
begin
  Assign(ASource);
end;

// ------------------------------------------------------------------------------

{ TksTableView }

procedure TksTableView.BeginUpdate;
begin
  KillAllTimers;
  FUpdateCount := FUpdateCount + 1;
end;

procedure TksTableView.AniCalcStart(Sender: TObject);
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(Self, True);

  FScrolling := True;
end;

procedure TksTableView.UpdateItemRects;
var
  ICount: integer;
  AYPos: single;
  AWidth: single;
  AItem: TksTableViewItem;
begin
  UpdateFilteredItems;
  AYPos := 0;
  AWidth := Width;
  for ICount := 0 to FFilteredItems.Count - 1 do
  begin
    AItem := FFilteredItems[ICount];
    AItem.ItemRect := RectF(0, AYPos, AWidth, AYPos + AItem.Height);

    AItem.Index := ICount;
    AYPos := AYPos + AItem.Height;
  end;

end;

procedure TksTableView.UpdateScrollingLimits;
var
  Targets: array of TAniCalculations.TTarget;
begin
  if FAniCalc <> nil then
  begin
    SetLength(Targets, 2);
    Targets[0].TargetType := TAniCalculations.TTargetType.Min;
    Targets[0].Point := TPointD.Create(0, 0);
    Targets[1].TargetType := TAniCalculations.TTargetType.Max;
    Targets[1].Point := TPointD.Create(0,
      Max((GetTotalItemHeight - Height) + GetSearchHeight, 0));
    FAniCalc.SetTargets(Targets);
  end;
end;

procedure TksTableView.AniCalcStop(Sender: TObject);
begin
  FScrolling := False;
  FSwipeDirection := ksSwipeUnknown;
  CacheItems(False);

  if Scene <> nil then
    Scene.ChangeScrollingState(nil, False);
end;

procedure TksTableView.AniCalcChange(Sender: TObject);
var
  NewViewPos: single;
begin
  NewViewPos := FAniCalc.ViewportPosition.y;
  if FScrolling then
  begin
    ScrollViewPos := NewViewPos;;
  end;
end;

procedure TksTableView.CacheItems(AForceRedraw: Boolean);
var
  ICount: integer;
  ATopItem: TksTableViewItem;
  AItems: TksTableViewItems;
  AStartPos: integer;
begin
  ATopItem := TopItem;
  if ATopItem = nil then
    Exit;
  AItems := FFilteredItems;

  AStartPos := Max(ATopItem.Index, 0);
  for ICount := AStartPos to (AStartPos + C_TABLEVIEW_PAGE_SIZE) do
  begin
    if ICount > AItems.Count - 1 then
      Exit;
    AItems[ICount].CacheItem(AForceRedraw);
    if FScrolling then
      Break;
  end;
end;

procedure TksTableView.ClearItems;
begin
  FItems.Clear;
  FFilteredItems.Clear;
  Invalidate;
end;

constructor TksTableView.Create(AOwner: TComponent);
begin
  inherited;
  TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService);

  FItems := TksTableViewItems.Create(Self, True);
  FFilteredItems := TksTableViewItems.Create(Self, False);
  FBackgroundText := TksTableViewBackgroundText.Create;
  FRowIndicators := TksListViewRowIndicators.Create;
  FDeleteButton := TksDeleteButton.Create;
  FAppearence := TksTableViewAppearence.Create(Self);
  FSearchBox := TSearchBox.Create(Self);
  FTextDefaults := TksTableViewTextDefaults.Create;

  FSearchBox.Visible := False;
  FSearchBox.Align := TAlignLayout.Top;
  FSearchBox.OnTyping := DoFilterChanged;
  FSearchBox.OnChange := DoFilterChanged;
  Width := 200;
  Height := 300;
  ClipChildren := True;

  FAniCalc := TAniCalculations.Create(nil);
  FAniCalc.Animation := True;
  FAniCalc.Averaging := True;
  FAniCalc.OnChanged := AniCalcChange;
  FAniCalc.Interval := 8;
  FAniCalc.OnStart := AniCalcStart;
  FAniCalc.OnStop := AniCalcStop;
  FAniCalc.BoundsAnimation := True;
  FAniCalc.TouchTracking := [ttVertical];
  //FAniCalc.DecelerationRate := 1;


  UpdateScrollingLimits;
  FSearchVisible := False;
  FShowAccessory := True;
  FItemIndex := -1;
  FItemHeight := C_TABLEVIEW_DEFAULT_ITEM_HEIGHT;
  FHeaderHeight := C_TABLEVIEW_DEFAULT_HEADER_HEIGHT;
  FItemImageSize := C_TABLEVIEW_DEFAULT_IMAGE_SIZE;
  FKeepSelection := False;
  FPullToRefresh := True;
  FNeedsRefresh := False;
  FMouseDown := False;
  FCheckMarks := TksTableViewCheckMarks.cmNone;
  FShowSelection := True;
  FStickyHeaders := True;
  FFullWidthSeparator := True;

  FUpdateCount := 0;
  AddObject(FSearchBox);
end;

function TksTableView.CreateTimer(AInterval: integer; AProc: TTimerProc): TFmxHandle;
begin
  Result := 0;
  if FTimerService <> nil then
    Result := FTimerService.CreateTimer(AInterval, AProc);
end;

destructor TksTableView.Destroy;
begin
  FreeAndNil(FRowIndicators);
  FreeAndNil(FBackgroundText);
  FreeAndNil(FFilteredItems);
  FreeAndNil(FItems);
  FreeAndNil(FAniCalc);
  FreeAndNil(FAppearence);
  FreeAndNil(FDeleteButton);
  FreeAndNil(FTextDefaults);
  inherited;
end;

procedure TksTableView.DoDeselectItem;
begin
  if FTimerService = nil then
    Exit;
  KillTimer(FDeselectTimer);
  if ItemIndex <> -1 then
  begin
    ItemIndex := -1;
    Invalidate;
  end;
end;

procedure TksTableView.DeselectItem(const ADelay: integer);
begin
  if ADelay > 0 then
  begin
    KillTimer(FDeselectTimer);
    FDeselectTimer := CreateTimer(ADelay, DoDeselectItem)
  end
  else
    DoDeselectItem;
end;

procedure TksTableView.DoFilterChanged(Sender: TObject);
begin
  UpdateFilteredItems;
  UpdateScrollingLimits;
  UpdateItemRects;
  Repaint;
end;

procedure TksTableView.DoMouseLeave;
begin
  inherited;
  if (FAniCalc <> nil) then
    FAniCalc.MouseLeave;
  FMouseDown := False;
end;

procedure TksTableView.DoPullToRefresh;
begin
  if Assigned(FOnPullRefresh) then
  begin
    KillAllTimers;
    FOnPullRefresh(Self);
  end;
end;

procedure TksTableView.DoSelectItem;
begin
  KillTimer(FSelectTimer);
  MouseUp(TMouseButton.mbLeft, [], FMouseCurrentPos.x, FMouseCurrentPos.y);
  if FMouseDownItem = nil then
    Exit;
  if FMouseDownItem.FActionButtons.Visible then
  begin
    FMouseDownItem.DoClick(FMouseDownPoint.x, (FMouseDownPoint.y - FMouseDownItem.ItemRect.Top) + ScrollViewPos);

    Exit;
  end;


  if (FCheckMarks <> TksTableViewCheckMarks.cmMultiSelect) and (FMouseDownItem.FActionButtons.Visible = False) then
  begin
    UncheckAll;
    ItemIndex := FMouseDownItem.Index;

  end;

  FMouseDownItem.DoClick(FMouseDownPoint.x, (FMouseDownPoint.y - FMouseDownItem.ItemRect.Top) + ScrollViewPos);
  if Assigned(FItemClickEvent) then
    FItemClickEvent(Self, FMouseDownPoint.x, FMouseDownPoint.y, FMouseDownItem,
      FMouseDownItem.ID, nil);
  HideAllActionButtons(False);
end;

procedure TksTableView.EndUpdate;
begin
  if FUpdateCount = 0 then
    Exit;
  FUpdateCount := FUpdateCount - 1;
  if FUpdateCount = 0 then
  begin
    CacheItems(False);
    Invalidate;
  end;
end;

procedure TksTableView.UncheckAll;
var
  ICount: integer;
begin
  BeginUpdate;
  for ICount := 0 to FItems.Count - 1 do
  begin
    Items[ICount].Checked := False;
  end;
  EndUpdate;
end;

procedure TksTableView.UpdateFilteredItems;
var
  ICount: integer;
  ASearchText: string;
begin
  FFilteredItems.Clear;
  ASearchText := Trim(FSearchBox.Text);
  for ICount := 0 to FItems.Count - 1 do
  begin
    if FItems[ICount].MatchesSearch(ASearchText) then
    begin
      FItems[ICount].Index := FFilteredItems.Count;
      FFilteredItems.Add(FItems[ICount]);
    end;
  end;
end;

function TksTableView.GetItemFromYPos(AYPos: single): TksTableViewItem;
var
  ICount: integer;
  AFiltered: TksTableViewItems;
begin
  Result := nil;
  AFiltered := FFilteredItems;
  for ICount := 0 to AFiltered.Count - 1 do
  begin
    if PtInRect(AFiltered[ICount].ItemRect, PointF(4, AYPos + GetScrollViewPos))
    then
    begin
      Result := AFiltered[ICount];
      Exit;
    end;
  end;
end;

function TksTableView.GetItemIndex: integer;
begin
  Result := FItemIndex;
end;

function TksTableView.GetMouseDownBox: TRectF;
var
  pt: TPointF;
  v: single;
begin
  v := C_TABlEVIEW_SCROLL_THRESHOLD;
  pt := FMouseDownPoint;
  Result := RectF(pt.x - v, pt.y - v, pt.x + v, pt.y + v);
end;

function TksTableView.GetScrollViewPos: single;
begin
  Result := FScrollPos + GetSearchHeight;

  // if FSearchVisible then
  // Result := Result - FSearchBox.Height;
end;

function TksTableView.GetSearchHeight: single;
begin
  Result := 0;
  if FSearchVisible then
    Result := FSearchBox.Height;
end;

function TksTableView.GetSelectedItem: TksTableViewItem;
begin
  Result := nil;
  if FItemIndex > -1 then
    Result := FilteredItems[FItemIndex];
end;

function TksTableView.GetShowAccessory: Boolean;
begin
  Result := FShowAccessory;
end;

function TksTableView.GetTopItem: TksTableViewItem;
var
  ICount: integer;
  AViewport: TRectF;
  AItems: TksTableViewItems;
begin
  Result := nil;
  AViewport := ViewPort;
  AItems := FFilteredItems;
  for ICount := 0 to AItems.Count - 1 do
  begin
    if AItems[ICount].IsVisible(AViewport) then
    begin
      Result := AItems[ICount];
      Exit;
    end;
  end;
end;

function TksTableView.GetTotalItemHeight: single;
begin
  Result := FFilteredItems.GetTotalItemHeight;
end;

function TksTableView.GetViewPort: TRectF;
begin
  Result := RectF(0, 0, Width, Height);
  OffsetRect(Result, 0, FScrollPos);
end;

function TksTableView.GetVisibleItems: TList<TksTableViewItem>;
var
  ICount: integer;
  ATopItem: TksTableViewItem;
  AViewport: TRectF;
begin
  Result := TList<TksTableViewItem>.Create;
  ATopItem := TopItem;
  AViewport := ViewPort;
  for ICount := ATopItem.Index to FItems.Count - 1 do
  begin
    if Items[ICount].IsVisible(AViewport) then
      Result.Add(Items[ICount])
    else
      Break;
  end;
end;

procedure TksTableView.HideAllActionButtons(ASync: Boolean);
var
  ICount: integer;
begin
  for ICount := 0 to FItems.Count-1 do
  begin
    if FItems[ICount].FActionButtons.Visible then
    begin
      FItems[Icount].FActionButtons.HideButtons(ASync);
      //Exit;
    end;
  end;
end;

procedure TksTableView.Invalidate;
begin
  InvalidateRect(LocalRect);
end;

procedure TksTableView.KillAllTimers;
begin
  KillTimer(FSelectTimer);
  KillTimer(FDeselectTimer);
end;

procedure TksTableView.KillTimer(ATimer: TFmxHandle);
begin
  if FTimerService <> nil then
  begin
    FTimerService.DestroyTimer(ATimer);
    ATimer := 0;
  end;
end;

procedure TksTableView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
begin
  if UpdateCount > 0 then Exit;
  inherited;
  if (FUpdateCount > 0) or (AIsSwiping) then
    Exit;

  FMouseDown := True;

  FSwipeDirection := ksSwipeUnknown;
  FMouseDownPoint := PointF(x, y);
  FMouseCurrentPos := FMouseDownPoint;

  FAniCalc.MouseDown(x, y);

  FMouseDownItem := GetItemFromYPos(y);
  if FMouseDownItem <> nil then
  begin
    if FMouseDownItem.Purpose = None then
    begin
      KillTimer(FSelectTimer);
      FSelectTimer := CreateTimer(400, DoSelectItem);
    end;
  end;
end;

procedure TksTableView.MouseMove(Shift: TShiftState; x, y: single);
var
  AMouseDownRect: TRectF;
begin
  if UpdateCount > 0 then Exit;
  FMouseCurrentPos := PointF(x, y);
  inherited;

  if not (ssLeft in Shift) then
    FMouseDownItem := nil;

  AMouseDownRect := GetMouseDownBox;

  if (ssLeft in Shift) and
     (PtInRect(AMouseDownRect, PointF(x, y)) = False) and
     (FSwipeDirection = ksSwipeUnknown) then
  begin
    FScrolling := True;
    if FSwipeDirection = ksSwipeUnknown then
    begin
      KillTimer(FSelectTimer);
      FSelectTimer := 0;
      if x < AMouseDownRect.Left then FSwipeDirection := ksSwipeRightToLeft;
      if x > AMouseDownRect.Right then FSwipeDirection := ksSwipeLeftToRight;
      if y < AMouseDownRect.Top then FSwipeDirection := ksSwipeBottomToTop;
      if y > AMouseDownRect.Bottom then FSwipeDirection := ksSwipeTopToBottom;
      FAniCalc.MouseDown(x, y);
    end;
  end;
  if FSwipeDirection = ksSwipeUnknown then
    Exit;

  if (FSwipeDirection in [ksSwipeLeftToRight, ksSwipeRightToLeft]) and (FMouseDownItem <> nil) then
  begin
    //y := FMouseDownPoint.y;
    if FSwipeDirection <> TksSwipeDirection.ksSwipeUnknown then
      FMouseDownItem.DoSwipe(FSwipeDirection);
    Exit;
  end;
  if FSwipeDirection in [ksSwipeTopToBottom, ksSwipeBottomToTop] then
    x := FMouseDownPoint.x;

  if (FScrolling) and (ssLeft in Shift) then
    FAniCalc.MouseMove(x, y);
end;

procedure TksTableView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
begin
  if UpdateCount > 0 then Exit;
  inherited;
  if PtInRect(GetMouseDownBox, PointF(x,y)) then
  begin
    if FSelectTimer <> 0 then
    begin
      KillTimer(FSelectTimer);
      FSelectTimer := 0;
      DoSelectItem;
    end;
  end;

  if FScrolling then
    FAniCalc.MouseUp(x, y);
  FMouseDown := False;
  if (FItemIndex > -1) and (FKeepSelection = False) then
    DeselectItem(200);
end;

procedure TksTableView.Paint;
var
  ICount: integer;
  AItems: TksTableViewItems;
  AViewport: TRectF;
  AItem: TksTableViewItem;
  AItemsDrawn: Boolean;
  ARect: TRectF;
begin
  if not FPainting then
  begin
    FPainting := True;
    try
      Canvas.Clear(claWhite);
      if (FPullToRefresh) and (ScrollViewPos < 0) then
      begin
        Canvas.Stroke.Thickness := 1/GetScreenScale;
        Canvas.Stroke.Color := claDimgray;
        Canvas.DrawLine(PointF(0, 0-ScrollViewPos), PointF(Width, 0-ScrollViewPos), 1);
        // pull to refresh...
        if (FMouseDown) then
          FNeedsRefresh := (ScrollViewPos <= -50);

        if (FNeedsRefresh) and (ScrollViewPos <= -25) then
        begin
          Canvas.Fill.Color := claSilver;
          Canvas.Font.Size := 16;
          Canvas.FillText(RectF(0, 0, Width, 50), 'release to refresh', False, 1, [], TTextAlign.Center);
          FNeedsRefresh := True;
        end;
      end;

      AItemsDrawn := False;
      AItems := FilteredItems;
      AViewport := ViewPort;

      for ICount := 0 to AItems.Count - 1 do
      begin
        AItem := AItems[ICount];

        if (AItem.IsVisible(AViewport))
        { or (AItem.Purpose = TksTableItemPurpose.ksHeader) } then
        begin
          AItemsDrawn := True;
          AItems[ICount].Render(Canvas, AViewport.Top);
        end
        else
        begin
          if AItemsDrawn then
            Break
        end;
      end;

      if FStickyHeaders then
      begin
        for ICount := 0 to AItems.Count - 1 do
        begin
          AItem := AItems[ICount];
          if (AItem.Purpose = Header) then
          begin
            if AItem.ItemRect.Top < AViewport.Top then

              AItem.Render(Canvas, Max(0, Round(AItem.ItemRect.Top)))
            else
              AItem.Render(Canvas, AViewport.Top);
          end;
        end;
      end;

      if (FBackgroundText.Enabled) and (AItems.Count = 0) then
      begin
        if FBackgroundText.Text <> '' then
        begin
          Canvas.Font.Assign(FBackgroundText.Font);
          Canvas.Fill.Color := FBackgroundText.TextColor;
          Canvas.Fill.Kind := TBrushKind.Solid;

          ARect := LocalRect;
          if ScrollViewPos < 0  then
            OffsetRect(ARect, 0, 0-ScrollViewPos);
          Canvas.FillText(ARect, FBackgroundText.Text, False, 1, [], TTextAlign.Center);
        end;
      end;
    finally
      FPainting := False;
    end;
  end;
end;


procedure TksTableView.Resize;
begin
  inherited;
  UpdateItemRects;
  CacheItems(True);
end;

procedure TksTableView.SetCheckMarks(const Value: TksTableViewCheckMarks);
begin
  if FCheckMarks <> Value then
  begin
    FCheckMarks := Value;
    if FCheckMarks <> TksTableViewCheckMarks.cmMultiSelect then
      UncheckAll;
  end;
end;

procedure TksTableView.SetFullWidthSeparator(const Value: Boolean);
begin
  if FFullWidthSeparator <> Value then
  begin
    FFullWidthSeparator := Value;
    //Invalidate;
  end;
end;

procedure TksTableView.SetHeaderHeight(const Value: integer);
begin
  FHeaderHeight := Value;
end;

procedure TksTableView.SetItemImageSize(const Value: integer);
begin
  FItemImageSize := Value;
end;

procedure TksTableView.SetItemIndex(const Value: integer);
var
  ASelected: TksTableViewItem;
  ANewSelected: TksTableViewItem;
begin
  if Value <> FItemIndex then
  begin
    ASelected := SelectedItem;
    FItemIndex := Value;
    ANewSelected := SelectedItem;
    if ASelected <> nil then
      ASelected.CacheItem(True);
    if ANewSelected <> nil then
      ANewSelected.CacheItem(True);
    Invalidate;
    if FMouseDown = False then
    begin
      if (FKeepSelection = False) and (FItemIndex > -1) then
        DeselectItem(250);
    end;
  end;
end;

procedure TksTableView.SetKeepSelection(const Value: Boolean);
begin
  FKeepSelection := Value;
end;

procedure TksTableView.SetKsItemHeight(const Value: integer);
begin
  FItemHeight := Value;
end;

procedure TksTableView.SetScrollViewPos(const Value: single);
begin

  if not SameValue(FScrollPos, Value, 1/GetScreenScale) then
  //if Round(FScrollPos) <> Round(Value) then

  begin
    FScrollPos := Value - GetSearchHeight;
    HideAllActionButtons(True);
    Repaint; //Invalidate;
    if (Round(FScrollPos) = 0) and (FNeedsRefresh) then
    begin
      FNeedsRefresh := False;
      DoPullToRefresh;
    end;
  end;
 // frmMain.lblHeader.Text := FloatToStr(Value);
end;

procedure TksTableView.SetSearchVisible(const Value: Boolean);
var
  AScrollPos: single;
begin
  if Value <> FSearchVisible then
  begin
    AScrollPos := ScrollViewPos;
    FSearchVisible := Value;
    FSearchBox.Visible := FSearchVisible;
    UpdateScrollingLimits;
    TAnimator.AnimateFloatWait(Self, 'ScrollPos', AScrollPos);
    UpdateItemRects;
    Invalidate;
  end;
end;

procedure TksTableView.SetShowAccessory(const Value: Boolean);
begin

end;

procedure TksTableView.SetShowSelection(const Value: Boolean);
begin
  if FShowSelection <> Value then
  begin
    FShowSelection := Value;
    Invalidate;
  end;
end;

procedure TksTableView.SetTextDefaults(const Value: TksTableViewTextDefaults);
begin
  FTextDefaults := Value;
end;

{ TksTableViewBackgroundText }

constructor TksTableViewBackgroundText.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.Size := 18;
  FTextColor := claSilver;
  FText := '';
  FEnabled := True;

end;

destructor TksTableViewBackgroundText.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TksTableViewBackgroundText.SetFont(const Value: TFont);
begin
  FFont.Assign(FFont);
end;

procedure TksTableViewBackgroundText.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TksTableViewBackgroundText.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
end;

procedure TksTableViewBackgroundText.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

{ TksTableViewItemShape }

constructor TksTableViewItemShape.Create(ATableItem: TksTableViewItem);
begin
  inherited;
  FFill := TBrush.Create(TBrushKind.Solid, claWhite);
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, claBlack);
  FCornerRadius := 0;
  FShape := ksRectangle;
end;

destructor TksTableViewItemShape.Destroy;
begin
  FreeAndNil(FFill);
  FreeAndNil(FStroke);
  inherited;
end;

procedure TksTableViewItemShape.Render(ACanvas: TCanvas);
var
  ARect: TRectF;
  AShadowWidth: single;
  ABmp: TBitmap;
begin
  if (Width = 0) or (Height = 0) then
    Exit;

  ABmp := TBitmap.Create;
  try
    AShadowWidth := 0;

    if FTableItem.FTableView.RowIndicators.Shadow then
      AShadowWidth := 2 / GetScreenScale;

    ABmp.SetSize(Round(Width * GetScreenScale), Round(Height * GetScreenScale));
    ABmp.BitmapScale := GetScreenScale;
    ABmp.Clear(claNull);
    ABmp.Canvas.BeginScene;
    try
      ARect := RectF(0, 0, (ABmp.Width / GetScreenScale) - AShadowWidth, (ABmp.Height / GetScreenScale) - AShadowWidth);

      if AShadowWidth > 0 then
      begin
        OffsetRect(ARect, AShadowWidth, AShadowWidth);
        ABmp.Canvas.Fill.Color := claDimgray;
        ABmp.Canvas.FillRect(ARect, FCornerRadius, FCornerRadius, AllCorners, 1);
        OffsetRect(ARect, 0-AShadowWidth, 0-AShadowWidth);
      end;
      ABmp.Canvas.Fill.Assign(FFill);
      ABmp.Canvas.Stroke.Assign(FStroke);

      FFill.Color := GetColorOrDefault(FFill.Color, claWhite);
      if FShape in [ksRectangle, ksRoundRect] then
      begin
        ABmp.Canvas.FillRect(ARect, FCornerRadius, FCornerRadius, AllCorners, 1);
        ABmp.Canvas.DrawRect(ARect, FCornerRadius, FCornerRadius, AllCorners, 1);
      end;
      if FShape = ksEllipse then
      begin
        ABmp.Canvas.FillEllipse(ARect, 1);
        ABmp.Canvas.DrawEllipse(ARect, 1);
      end;
    finally
      ABmp.Canvas.EndScene;
    end;
    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ObjectRect, 1, True);
    ACanvas.Stroke.Color := clablack;
    ACanvas.DrawRect(ObjectRect, 0, 0, AllCorners, 1);
  finally
    ABmp.Free;
  end;
  inherited;
end;

procedure TksTableViewItemShape.SetCornerRadius(const Value: single);
begin
  FCornerRadius := Value;
end;

procedure TksTableViewItemShape.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TksTableViewItemShape.SetShape(const Value: TksTableViewShape);
begin
  FShape := Value;
end;

procedure TksTableViewItemShape.SetStroke(const Value: TStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{ TksListViewRowIndicators }

constructor TksListViewRowIndicators.Create;
begin
  FWidth := C_TABLEVIEW_DEFAULT_INDICATOR_WIDTH;
  FHeight := C_TABLEVIEW_DEFAULT_INDICATOR_HEIGHT;
  FVisible := False;
  FOutlined := True;
  FShadow := True;
end;

{ TksTableViewActionButtons }

function TksTableViewActionButtons.AddButton(AText: string; AColor, ATextColor: TAlphaColor; AWidth: integer): TksTableViewActionButon;
begin
  Result := TksTableViewActionButon.Create(False);
  Result.Text := AText;
  Result.Color := AColor;
  Result.Width := AWidth;
  Result.TextColor := ATextColor;
  Add(Result);
end;

function TksTableViewActionButtons.ButtonFromXY(x, y: single): TksTableViewActionButon;
var
  ARect: TRectF;
  ICount: integer;
  XPos: single;
begin
  Result := nil;
  ARect := RectF(0, 0, TotalWidth, FTableItem.Height);
  if FAlignment = TksTableViewActionButtonAlignment.abRightActionButtons then
    x := x - (FTableItem.ItemRect.Width -  TotalWidth);
  //  OffsetRect(ARect, FTableItem.ItemRect.Width - TotalWidth, 0);

  XPos := 0;
  for ICount := 0 to Count-1 do
  begin
    if (x >= XPos) and (x <= XPos+Items[ICount].Width) then
    begin
      Result := Items[ICount];
      Exit;
    end;
    XPos := XPos + Items[Icount].Width;
  end;
end;

constructor TksTableViewActionButtons.Create(AOwner: TksTableViewItem);
begin
  inherited Create(True);
  FTableItem := AOwner;
  FAlignment := abRightActionButtons;
  FPercentWidth := 0;
  FAnimating := False;
end;

function TksTableViewActionButtons.GetVisible: Boolean;
begin
  Result := FPercentWidth > 0;
end;

procedure TksTableViewActionButtons.HideButtons(ASync: Boolean);
var
  ICount: integer;
  ATask: ITask;
begin
  if Visible = False then
    Exit;
  if FAnimating then
    Exit;
  FAnimating := True;
  if ASync then
  begin
    ATask := TTask.Create (procedure ()
    var
      i: integer;
    begin
      for i := 100 downto 0 do
      begin
        FPercentWidth := i;
        FTableItem.FTableView.Repaint;
        Sleep(1);
        Application.ProcessMessages;
        if i = 0 then
          FAnimating := False;
      end;
    end);
    ATask.Start;
    Exit;
  end;

  try
    for ICount := 100 downto 0 do
    begin
      FPercentWidth := ICount;
      FTableItem.FTableView.Invalidate;
      {$IFDEF NEXTGEN}
      if ICount mod 7 = 0 then
        Application.ProcessMessages;
      {$ELSE}
      Application.ProcessMessages;
      {$ENDIF}
    end;
    Application.ProcessMessages;
  finally
    FAnimating := False;
  end;
end;

procedure TksTableViewActionButtons.Render(ACanvas: TCanvas; ARect: TRectF);
var
  ICount: integer;
  AXPos: single;
  ABtnRect: TRectF;
  AScale: single;
begin
  AXPos := ARect.Left;
  AScale := ARect.Width / TotalWidth;

  for ICount := 0 to Count-1 do
  begin
    ABtnRect := RectF(AXPos, ARect.Top, AXPos+(Items[ICount].Width * AScale), ARect.Bottom);
    ACanvas.Fill.Color := Items[ICount].Color;
    ACanvas.FillRect(ABtnRect, 0, 0, AllCorners, 1);
    ACanvas.Font.Size := 14;
    ACanvas.Fill.Color := Items[Icount].TextColor;
    ACanvas.FillText(ABtnRect, ITems[Icount].Text, False, 1, [], TTextAlign.Center);
    AXPos := AXPos + (Items[ICount].Width * AScale);

  end;
end;

procedure TksTableViewActionButtons.SetPercentWidth(const Value: integer);
begin
  if PercentWidth <> Value then
  begin
    FPercentWidth := Value;
    FTableItem.FTableView.Invalidate;
    Application.ProcessMessages;
  end;
end;

procedure TksTableViewActionButtons.ShowButtons;
var
  ICount: integer;
begin
  if FAnimating then
    Exit;
  FAnimating := True;
  try
    for ICount := 1 to 100 do
    begin
      FPercentWidth := ICount;
      FTableItem.FTableView.Invalidate;

      {$IFDEF NEXTGEN}
      if ICount mod 7 = 0 then
        Application.ProcessMessages;
      {$ELSE}
      Application.ProcessMessages;
      {$ENDIF}
    end;
    Application.ProcessMessages;
  finally
    FAnimating := False;
  end;
end;



function TksTableViewActionButtons.TotalWidth: integer;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to Count-1 do
    Result := Result + Items[Icount].Width;
end;

{ TksTableViewActionButon }

constructor TksTableViewActionButon.Create(AIsDelete: Boolean);
begin
  FWidth := 80;
  FTextColor := claWhite;
  FIsDeleteButton := AIsDelete;
end;

{ TksDeleteButton }

constructor TksDeleteButton.Create;
begin
  FEnabled := False;
  FText := 'Delete';
  FColor := claRed;
  FTextColor := claWhite;
  FWidth := 60;
end;

{ TksTableViewTextDefaults }

constructor TksTableViewTextDefaults.Create;
begin
  FTitle := TksTableViewTextDefault.Create;
  FSubtitle := TksTableViewTextDefault.Create;
  FDetail := TksTableViewTextDefault.Create;
  FHeader := TksTableViewTextDefault.Create;

  FSubtitle.TextColor := claDimgray;
  FDetail.TextColor := claDodgerblue;
end;

destructor TksTableViewTextDefaults.Destroy;
begin
  FreeAndNil(FTitle);
  FreeAndNil(FSubtitle);
  FreeAndNil(FDetail);
  FreeAndNil(FHeader);
  inherited;
end;

procedure TksTableViewTextDefaults.SetDetail(const Value: TksTableViewTextDefault);
begin
  FDetail.Assign(Value);
end;

procedure TksTableViewTextDefaults.SetHeader(const Value: TksTableViewTextDefault);
begin
  FHeader.Assign(Value);
end;

procedure TksTableViewTextDefaults.SetSubTitle(const Value: TksTableViewTextDefault);
begin
  FSubtitle.Assign(Value);
end;

procedure TksTableViewTextDefaults.SetTitle(const Value: TksTableViewTextDefault);
begin
  FTitle.Assign(Value);
end;

{ TksTableViewTextDefault }

procedure TksTableViewTextDefault.Assign(Source: TPersistent);
begin
  FFont.Assign((Source as TksTableViewTextDefault).Font);
  FTextColor := (Source as TksTableViewTextDefault).TextColor;
end;

constructor TksTableViewTextDefault.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.Size := C_TABLEVIEW_DEFAULT_FONT_SIZE;
  FTextColor := claBlack;
end;

destructor TksTableViewTextDefault.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TksTableViewTextDefault.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TksTableViewTextDefault.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
end;

{ TksTableViewItemSwitch }

constructor TksTableViewItemSwitch.Create(ATableItem: TksTableViewItem);
begin
  inherited;
  FChecked := False;
  FSelectedColor := C_TABLEIEW_DEFAULT_SWITCH_COLOR;
end;

procedure TksTableViewItemSwitch.Render(ACanvas: TCanvas);
begin
  DrawSwitch(ACanvas, ObjectRect, FChecked, FSelectedColor);
end;

procedure TksTableViewItemSwitch.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Changed;;
  end;
end;

procedure TksTableViewItemSwitch.SetSelectedColor(const Value: TAlphaColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    Changed;
  end;
end;

initialization

  AccessoryImages := TksTableViewAccessoryImageList.Create;
  ATextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  AIsSwiping := False;

finalization

FreeAndNil(AccessoryImages);
FreeAndNil(ATextLayout);

end.
