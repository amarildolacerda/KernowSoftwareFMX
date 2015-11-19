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
  System.UIConsts, System.Rtti, FMX.DateTimeCtrls,
  FMX.Styles, FMX.Styles.Objects, FMX.Edit, FMX.SearchBox, FMX.ListBox;

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
  C_TABLEVIEW_DEFAULT_WIDTH = 200;
  C_TABLEVIEW_DEFAULT_HEIGHT = 300;

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
  TksTableViewItemSwitch = class;
  TksTableViewItemTable = class;
  TksTableViewItemEmbeddedBaseEdit = class;
  TksTableViewItemEmbeddedEdit = class;

  TksTableItemAlign = (Leading, Center, Trailing);
  TksSwipeDirection = (ksSwipeUnknown, ksSwipeLeftToRight, ksSwipeRightToLeft, ksSwipeTopToBottom, ksSwipeBottomToTop);
  TksTableViewShape = (ksRectangle, ksRoundRect, ksEllipse);
  TksTableViewItemPurpose = (None, Header);
  TksTableViewCheckMarks = (cmNone, cmSingleSelect, cmMultiSelect);
  TksTableViewActionButtonAlignment = (abLeftActionButtons, abRightActionButtons);
  {TksTableViewTextWidth = (ksWidth10Percent,
                           ksWidth20Percent,
                           ksWidth30Percent,
                           ksWidth40Percent,
                           ksWidth50Percent,
                           ksWidth60Percent,
                           ksWidth70Percent,
                           ksWidth80Percent,
                           ksWidth90Percent); }
  TksTableItemSelector = (NoSelector, DateSelector, ItemPicker);


  TksTableViewRowCacheEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARow: TksTableViewItem; ARect: TRectF) of object;
  TksTableViewDeletingItemEvent = procedure(Sender: TObject; AItem: TksTableViewItem; var ACanDelete: Boolean) of object;
  TksTableViewDeleteItemEvent = procedure(Sender: TObject; AItem: TksTableViewItem) of object;
  TksTableViewItemClickEvent = procedure(Sender: TObject; x, y: single; AItem: TksTableViewItem; AId: string; ARowObj: TksTableViewItemObject) of object;
  TksItemSwipeEvent = procedure(Sender: TObject; ARow: TksTableViewItem; ASwipeDirection: TksSwipeDirection; AButtons: TksTableViewActionButtons) of object;
  TksItemActionButtonClickEvent = procedure(Sender: TObject; ARow: TksTableViewItem; AButton: TksTableViewActionButon) of object;
  TksTableViewItemSwitchEvent = procedure(Sender: TObject; AItem: TksTableViewItem; ASwitch: TksTableViewItemSwitch; ARowID: string) of object;

  TksItemChecMarkChangedEvent = procedure(Sender: TObject; ARow: TksTableViewItem; AChecked: Boolean) of object;
  TksTableViewSelectDateEvent = procedure(Sender: TObject; AItem: TksTableViewItem; ASelectedDate: TDateTime; var AAllow: Boolean) of object;
  TksTableViewSelectPickerItem = procedure(Sender: TObject; AItem: TksTableViewItem; ASelected: string; var AAllow: Boolean) of object;
  TksTableViewEmbeddedEditChange = procedure(Sender: TObject; ARow: TksTableViewItem; AEdit: TksTableViewItemEmbeddedBaseEdit; AText: string) of object;

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
    FOffsetX: single;
    FOffsetY: single;
    // FPadding: TRectF;
    procedure SetHeight(const Value: single);
    procedure SetWidth(const Value: single);
    procedure SetHitTest(const Value: Boolean);
    procedure SetOffsetX(const Value: single);
    procedure SetOffsetY(const Value: single);
  protected
    function ConsumesClick: Boolean; virtual;
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
    procedure MouseDown(x, y: single); virtual;
    procedure MouseUp(x, y: single); virtual;
    property Align: TksTableItemAlign read GetAlign write SetAlign;
    property Height: single read FHeight write SetHeight;
    property HitTest: Boolean read FHitTest write SetHitTest default True;
    property ID: string read GetID write SetID;
    //property PlaceOffset: TPointF read FPlaceOffset write SetPlaceOffset;
    property OffsetX: single read FOffsetX write SetOffsetX;
    property OffsetY: single read FOffsetY write SetOffsetY;
    property TableItem: TksTableViewItem read FTableItem;
    property VertAlign: TksTableItemAlign read GetVertAlign write SetVertAlign;
    property Width: single read FWidth write SetWidth;
  end;

  TksTableViewItemEmbeddedControl = class(TksTableViewItemObject)
  private
    FFocused: Boolean;
    procedure SimulateClick(x, y: single);
    procedure DoExitControl(Sender: TObject);
  protected
    FControl: TStyledControl;
    function CreateControl: TStyledControl; virtual; abstract;
    procedure InitializeControl; virtual;
    procedure ShowControl;
    procedure HideControl;
    function ConsumesClick: Boolean; override;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    destructor Destroy; override;
    procedure MouseDown(x, y: single); override;
  end;

  TksTableViewItemEmbeddedBaseEdit = class(TksTableViewItemEmbeddedControl)
  private
    FText: string;
    procedure SetText(const Value: string);
    function GetCustomEdit: TCustomEdit;
    procedure DoEditChange(Sender: TObject);
  protected
    procedure Render(ACanvas: TCanvas); override;
    property CustomEdit: TCustomEdit read GetCustomEdit;
    procedure InitializeControl; override;
  public
    property Text: string read FText write SetText;
  end;

  TksTableViewItemEmbeddedEdit = class(TksTableViewItemEmbeddedBaseEdit)
  private
    function GetEditControl: TEdit;
  protected
    function CreateControl: TStyledControl; override;
  public
    property Edit: TEdit read GetEditControl;
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
    procedure FontChanged(Sender: TObject);
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

  TksTableViewShadow = class(TPersistent)
  private
    FColor: TAlphaColor;
    FOffset: integer ;
    FVisible: Boolean;
  public
    constructor Create; virtual;
    procedure Assign(ASource: TPersistent); override;
    procedure SetVisible(const Value: Boolean);
    property Color: TAlphaColor read FColor write FColor default claSilver;
    property Offset: integer read FOffset write FOffset default 2;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TksListItemRowTableCell = class(TPersistent)
  private
    [weak]FTable: TksTableViewItemTable;
    FRow, FCol: integer;
    FTextSettings: TTextSettings;
    FFill: TBrush;
    FStroke: TStrokeBrush;
    FText: string;
    FWidth: single;
    FHeight: single;
    FPadding: TBounds;
    FVisible: Boolean;
    FSides: TSides;
    procedure SetText(const Value: string);
    procedure SetTextSettings(const Value: TTextSettings);
    procedure SetVisible(const Value: Boolean);
    procedure Changed;
    function GetShadow: TksTableViewShadow;
  public
    constructor Create(ATable: TksTableViewItemTable); virtual;
    destructor Destroy; override;
    function IsFixedCell: Boolean;
    procedure DrawToCanvas(x, y: single; ACanvas: TCanvas; ACol, ARow: integer; AShadow: TksTableViewShadow; AText: Boolean);
    property Fill: TBrush read FFill;
    property Stroke: TStrokeBrush read FStroke;
    property TextSettings: TTextSettings read FTextSettings write SetTextSettings;
    property Text: string read FText write SetText;
    property Width: single read FWidth write FWidth;
    property Height: single read FHeight write FHeight;
    property Padding: TBounds read FPadding write FPadding;
    property Shadow: TksTableViewShadow read GetShadow;
    property Sides: TSides read FSides write FSides;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TksListItemRowTableRow = array of TksListItemRowTableCell;

  TksListItemRowTableBanding = class(TPersistent)
  private
    FActive: Boolean;
    FColor2: TAlphaColor;
    FColor1: TAlphaColor;
    procedure SetActive(const Value: Boolean);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property Color1: TAlphaColor read FColor1 write FColor1 default claNull;
    property Color2: TAlphaColor read FColor2 write FColor2 default claNull;
    property Active: Boolean read FActive write SetActive;
  end;

  TksTableViewItemTable = class(TksTableViewItemObject)
  private
    FBackground: TAlphaColor;
    FBorderColor: TAlphaColor;
    FRows: array of TksListItemRowTableRow;
    FRowCount: integer;
    FColCount: integer;
    FDefaultRowHeight: single;
    FDefaultColWidth: single;
    FShadow: TksTableViewShadow;
    FFixedCellColor: TAlphaColor;
    FFixedRows: integer;
    FFixedCols: integer;
    FBanding: TksListItemRowTableBanding;
    procedure SetColCount(const Value: integer);
    procedure SetRowCount(const Value: integer);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetDefaultColWidth(const Value: single);
    procedure SetDefaultRowHeight(const Value: single);

    procedure ResizeTable;
    function GetColWidths(ACol: integer): single;
    procedure SetColWidths(ACol: integer; const Value: single);
    function GetCells(ACol, ARow: integer): TksListItemRowTableCell;
    function GetTableSize: TSizeF;
    procedure RenderTableContents(ACanvas: TCanvas; AText: Boolean);
    procedure SetFixedCellColor(const Value: TAlphaColor);
    procedure SetBanding(const Value: TksListItemRowTableBanding);
  protected
    procedure Render(ACanvas: TCanvas); override;
  public
    constructor Create(ARow: TKsTableViewItem); override;
    destructor Destroy; override;
    procedure Clear;
    procedure MergeRowCells(x, y, AMergeCount: integer);
    procedure SetRowColor(ARow: integer; AColor: TAlphaColor);
    procedure SetColColor(ACol: integer; AColor: TAlphaColor);
    procedure SetRowFont(ARow: integer; AFontName: TFontName; AColor: TAlphaColor; ASize: integer; AStyle: TFontStyles);
    procedure SetColFont(ACol: integer; AFontName: TFontName; AColor: TAlphaColor; ASize: integer; AStyle: TFontStyles);
    property Background: TAlphaColor read FBackground write SetBackgroundColor default claWhite;
    property Banding: TksListItemRowTableBanding read FBanding write SetBanding;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default claBlack;
    property FixCellColor: TAlphaColor read FFixedCellColor write SetFixedCellColor default claGainsboro;
    property FixedRows: integer read FFixedRows write FFixedRows default 1;
    property FixedCols: integer read FFixedCols write FFixedCols default 1;
    property Cells[ACol, ARow: integer]: TksListItemRowTableCell read GetCells;
    property ColCount: integer read FColCount write SetColCount;
    property RowCount: integer read FRowCount write SetRowCount;
    property DefaultRowHeight: single read FDefaultRowHeight write SetDefaultRowHeight;
    property DefaultColWidth: single read FDefaultColWidth write SetDefaultColWidth;
    property ColWidths[ACol: integer]: single read GetColWidths write SetColWidths;
    property Shadow: TksTableViewShadow read FShadow;
    property TableSize: TSizeF read GetTableSize;
  end;


  TksTableViewItemBaseImage = class(TksTableViewItemObject)
  strict private
    FBitmap: TBitmap;
    FShadow: TksTableViewShadow;
    [weak]FExternalBitmap: TBitmap;
  private
    FOwnsBitmap: Boolean;
    procedure SetBitmap(const Value: TBitmap);
    function GetBitmap: TBitmap;
    procedure SetOwnsBitmap(const Value: Boolean);
    procedure SetShadow(const Value: TksTableViewShadow);
  protected
    procedure Render(ACanvas: TCanvas); override;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Shadow: TksTableViewShadow read FShadow write SetShadow;
    property OwnsBitmap: Boolean read FOwnsBitmap write SetOwnsBitmap default False;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    destructor Destroy; override;
  end;

  TksTableViewItemImage = class(TksTableViewItemBaseImage)
  public
    property Bitmap;
    property Shadow;
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
    function ConsumesClick: Boolean; override;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    procedure MouseDown(x, y: single); override;
    procedure MouseUp(x, y: single); override;
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
    FData: TDictionary<string, TValue>;

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
    FSelectionValue: Variant;
    FFont: TFont;
    FTextColor: TAlphaColor;
    FActionButtons: TksTableViewActionButtons;
    FCanSelect: Boolean;
    //FTitleWidth: TksTableViewTextWidth;
    FTagString: string;
    FTagInteger: integer;
    FSelector: TksTableItemSelector;
    FPickerItems: TStrings;

    function MatchesSearch(AFilter: string): Boolean;
    function IsVisible(AViewport: TRectF): Boolean;
    function GetHeight: single;

    function GetItemRect: TRectF;
    function GetInternalRect: TRectF;

    function GetIndex: integer;
    function GetAbsoluteIndex: integer;
    function GetSearchIndex: string;
    function GetCached: Boolean;
    procedure SetSearchIndex(const Value: string);
    procedure SetItemRect(const Value: TRectF);
    procedure SetIndex(const Value: integer);

    procedure Changed;
    procedure RealignStandardObjects;
    procedure SetHeight(const Value: single);

    procedure SetCached(const Value: Boolean);
    function GetPurpose: TksTableViewItemPurpose;
    procedure SetPurpose(const Value: TksTableViewItemPurpose);
    procedure SetFont(const Value: TFont);
    procedure SetTextColor(const Value: TAlphaColor);

    procedure SetChecked(const Value: Boolean);
    procedure DoClick(x, y: single);
    function GetIndicatorColor: TAlphaColor;
    procedure SetIndicatorColor(const Value: TAlphaColor);
    procedure DoSwipe(ADirecton: TksSwipeDirection);
    //procedure SetTitleWidth(const Value: TksTableViewTextWidth);
    procedure SetPickerItems(const Value: TStrings);
    procedure PickerItemsChanged(Sender: TObject);
    function GetItemData(const AIndex: string): TValue;
    procedure SetItemData(const AIndex: string; const Value: TValue);
    function GetHasData(const AIndex: string): Boolean;
  protected
    procedure Render(ACanvas: TCanvas; AScrollPos: single);
    procedure CacheItem(const AForceCache: Boolean = False);

  public
    constructor Create(ATableView: TksTableView); reintroduce;
    destructor Destroy; override;
    function ObjectAtPos(x, y: single): TksTableViewItemObject;
    function IsLastItem: Boolean;

    // image functions...
    function DrawBitmap(ABmp: TBitmap; x, AWidth, AHeight: single): TksTableViewItemImage; overload;
    function DrawBitmap(ABmp: TBitmap; x, y, AWidth, AHeight: single): TksTableViewItemImage overload;

    // text functions...
    function TextWidth(AText: string; AIsHtml: Boolean): single;
    function TextHeight(AText: string; AWordWrap, AIsHtml: Boolean; ATrimming: TTextTrimming; const AMaxWidth: single): single;

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


    function AddEdit(AX, AY, AWidth: single; AText: string): TksTableViewItemEmbeddedEdit;
    function AddSwitch(x: single; AIsChecked: Boolean; const AAlign: TksTableItemAlign = TksTableItemAlign.Trailing): TksTableViewItemSwitch;
    function AddTable(AX, AY, AColWidth, ARowHeight: single; AColCount, ARowCount: integer): TksTableViewItemTable;


    property AbsoluteIndex: integer read GetAbsoluteIndex;
    property Accessory: TksTableViewItemAccessory read FAccessory;
    property CanSelect: Boolean read FCanSelect write FCanSelect default True;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Data[const AIndex: string]: TValue read GetItemData write SetItemData;
    property Font: TFont read FFont write SetFont;
    property HasData[const AIndex: string]: Boolean read GetHasData;

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
    property PickerItems: TStrings read FPickerItems write SetPickerItems;
    property Purpose: TksTableViewItemPurpose read GetPurpose write SetPurpose default None;
    property Selector: TksTableItemSelector read FSelector write FSelector;

    property TagString: string read FTagString write FTagString;
    property TagInteger: integer read FTagInteger write FTagInteger default 0;
    //property TitleWidth: TksTableViewTextWidth read FTitleWidth write SetTitleWidth default ksWidth60Percent;
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
    function AddHeader(AText: string): TksTableViewItem;
    function AddItem(AText: string; const AAccessory: TksAccessoryType = atNone): TksTableViewItem; overload;
    function AddItem(AText, ADetail: string; const AAccessory: TksAccessoryType = atNone): TksTableViewItem; overload;
    function AddItem(AText, ASubTitle, ADetail: string; const AAccessory: TksAccessoryType = atNone): TksTableViewItem; overload;

    function AddDateSelector(AText: string; ADate: TDateTime): TksTableViewItem;

    function AddItemSelector(AText, ASelected: string; AItems: TStrings): TksTableViewItem; overload;
    function AddItemSelector(AText, ASelected: string; AItems: array of string): TksTableViewItem; overload;
    function AddItemWithSwitch(AText: string; AChecked: Boolean; AID: string): TksTableViewItem;


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
    FItemBackground: TBrush;
    FAlternatingItemBackground: TAlphaColor;
    FSeparatorColor: TAlphaColor;
    FHeaderColor: TAlphaColor;
    FSelectedColor: TAlphaColor;
    procedure SetBackground(const Value: TAlphaColor);
    procedure SetItemBackground(const Value: TBrush);
    procedure SetAlternatingItemBackground(const Value: TAlphaColor);
    procedure SetSeparatorBackground(const Value: TAlphaColor);
    procedure SetHeaderColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
  public
    constructor Create(AListView: TksTableView);
    destructor Destroy; override;
  published
    property Background: TAlphaColor read FBackground write SetBackground default claWhite;
    property HeaderColor: TAlphaColor read FHeaderColor write SetHeaderColor default claNull;
    property SeparatorColor: TAlphaColor read FSeparatorColor write SetSeparatorBackground default $FFF0F0F0;
    property ItemBackground: TBrush read FItemBackground write SetItemBackground;
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
    procedure Assign(Source: TPersistent); override;
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

  TksTableViewPullToRefresh = class(TPersistent)
  private
    FEnabled: Boolean;
    FPullText: string;
    FReleaseText: string;
    FFont: TFont;
    FTextColor: TAlphaColor;
    procedure SetEnabled(const Value: Boolean);
    procedure SetFont(const Value: TFont);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Font: TFont read FFont write SetFont;
    property PullText: string read FPullText write FPullText;
    property ReleaseText: string read FReleaseText write FReleaseText;
    property TextColor: TAlphaColor read FTextColor write FTextColor default claSilver;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidAndroid)]
  TksTableView = class(TControl)
  strict private
    FFullWidthSeparator: Boolean;
  private
    FCombo: TComboBox;
    FDateSelector: TDateEdit;
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
    FMouseDownObject: TksTableViewItemObject;
    [weak]FFocusedControl: TksTableViewItemEmbeddedControl;

    // events...
    FItemClickEvent: TksTableViewItemClickEvent;
    FOnPullRefresh: TNotifyEvent;
    FPullToRefresh: TksTableViewPullToRefresh;
    FNeedsRefresh: Boolean;
    FCheckMarks: TksTableViewCheckMarks;
    FOnItemSwipe: TksItemSwipeEvent;
    FOnItemActionButtonClick: TksItemActionButtonClickEvent;
    FOnDeleteItem: TksTableViewDeleteItemEvent;
    FOnDeletingItem: TksTableViewDeletingItemEvent;
    FBeforeRowCache: TksTableViewRowCacheEvent;
    FAfterRowCache: TksTableViewRowCacheEvent;
    FOnEmbeddedEditChange: TksTableViewEmbeddedEditChange;
    FOnItemChecMarkChanged: TksItemChecMarkChangedEvent;
    FOnSelectDate: TksTableViewSelectDateEvent;
    FOnSelectPickerItem: TksTableViewSelectPickerItem;
    FOnSwitchClicked: TksTableViewItemSwitchEvent;

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
    procedure ComboClosePopup(Sender: TObject);
    procedure DoSwitchClicked(AItem: TksTableViewItem; ASwitch: TksTableViewItemSwitch);
    procedure SetPullToRefresh(const Value: TksTableViewPullToRefresh);
    procedure HideFocusedControl;
    procedure DoEmbeddedEditChange(AItem: TksTableViewItem; AEmbeddedEdit: TksTableViewItemEmbeddedBaseEdit);
  protected
    function GetTotalItemHeight: single;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; x, y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure DoMouseLeave; override;
    procedure Resize; override;
    function GetMouseDownBox: TRectF;
    procedure SelectDate(ARow: TksTableViewItem; ASelected: TDateTime; AOnSelectDate: TNotifyEvent);
    procedure SelectItem(ARow: TksTableViewItem; AItems: TStrings; ASelected: string; AOnSelectItem: TNotifyEvent);
    procedure DoSelectDate(Sender: TObject);
    procedure DoSelectPickerItem(Sender: TObject);

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
    property PullToRefresh: TksTableViewPullToRefresh read FPullToRefresh write SetPullToRefresh;
    property RotationAngle;
    property RotationCenter;
    property RowIndicators: TksListViewRowIndicators read FRowIndicators write FRowIndicators;
    property Scale;
    property SearchVisible: Boolean read FSearchVisible write SetSearchVisible default False;
    property ShowAccessory: Boolean read GetShowAccessory write SetShowAccessory default True;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
    property Size;
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
    property OnEmbeddedEditChange: TksTableViewEmbeddedEditChange read FOnEmbeddedEditChange write FOnEmbeddedEditChange;
    property OnItemActionButtonClick: TksItemActionButtonClickEvent read FOnItemActionButtonClick write FOnItemActionButtonClick;
    property OnItemCheckmarkChanged: TksItemChecMarkChangedEvent read FOnItemChecMarkChanged write FOnItemChecMarkChanged;
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
    property OnSelectDate: TksTableViewSelectDateEvent read FOnSelectDate write FOnSelectDate;
    property OnSelectPickerItem: TksTableViewSelectPickerItem read FOnSelectPickerItem write FOnSelectPickerItem;
    property OnSwitchClick: TksTableViewItemSwitchEvent read FOnSwitchClicked write FOnSwitchClicked;

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

uses SysUtils, FMX.Platform, Math, FMX.TextLayout, System.Math.Vectors,
  FMX.Ani, System.Threading, FMX.Forms;

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
  AHorzAlign: TTextAlign; AVertAlign: TTextAlign; ATrimming: TTextTrimming); overload;
begin
  if AText = '' then
    Exit;
  ATextLayout.BeginUpdate;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := AWordWrap;
  ATextLayout.Font.Assign(AFont);
  ATextLayout.Color := ATextColor;
  ATextLayout.HorizontalAlign := AHorzAlign;
  ATextLayout.VerticalAlign := AVertAlign;
  ATextLayout.Trimming := ATrimming;
  if AWordWrap  then
    ATextLayout.Trimming := TTextTrimming.None;
  ATextLayout.TopLeft := PointF(x, y);
  ATextLayout.MaxSize := PointF(AWidth, AHeight);
  ATextLayout.EndUpdate;
  ATextLayout.RenderLayout(ACanvas);
end;

procedure RenderText(ACanvas: TCanvas; ARect: TRectF;
  AText: string; AFont: TFont; ATextColor: TAlphaColor; AWordWrap: Boolean;
  AHorzAlign: TTextAlign; AVertAlign: TTextAlign; ATrimming: TTextTrimming); overload;
begin
  RenderText(ACanvas, ARect.Left, ARect.Top, ARect.Width, ARect.Height, AText, AFont, ATextColor, AWordWrap, AHorzAlign, AVertAlign, ATrimming);
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

function GetTextHeight(AText: string; AFont: TFont; AWordWrap: Boolean; ATrimming: TTextTrimming;
  const AWidth: single = 0): single;
var
  APoint: TPointF;
begin
  Result := 0;
  if AText = '' then
    Exit;

  ATextLayout.BeginUpdate;
  // Setting the layout MaxSize
  APoint.x := MaxSingle;
  if AWidth > 0 then
    APoint.x := AWidth;
  APoint.y := 100;

  ATextLayout.Font.Assign(AFont);
  ATextLayout.MaxSize := APoint;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := AWordWrap;
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
  s := 4;
  ABmp := TBitmap.Create(Round(ARect.Width * s), Round(ARect.Height * s));
  try
    ABmp.Clear(claNull);
    ABmp.BitmapScale := s;

    ABmp.Canvas.BeginScene;
    ABmp.Canvas.StrokeThickness := s;

    r := RectF(0, 0, ABmp.Height, ABmp.Height);
    InflateRect(r, -s, -s);
    //if not AChecked then
    ASwitchRect := r;

    ABmp.Canvas.Stroke.Color := claSilver;

    //ABmp.Canvas.Fill.Color := claNull;
    ABmp.Canvas.Fill.Color := claWhite;

    if AChecked then
    begin
      Abmp.Canvas.Fill.Color := ASelectedColor;
      //ABmp.Canvas.Stroke.Color := claDimgray;
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

    //if AChecked then
    //  Abmp.Canvas.Stroke.Color := ABmp.Canvas.Fill.Color;

    //ABmp.Canvas.FillRect(RectF(0  + (r.Width/2), 0, ABmp.Width - (r.Width/2), ABmp.Height), 0, 0,  AllCorners, 1, ABmp.Canvas.Fill);

    r := RectF(ABmp.Height/2, 0, ABmp.Width-(ABmp.Height/2), ABmp.Height);

    //
    ABmp.Canvas.StrokeThickness := s;
    r.Top := r.Top + s;
    r.Bottom := r.Bottom - s;
    r.Left := r.Left - (GetScreenScale*4);
    r.Right := r.Right + (GetScreenScale*4);

    ABmp.Canvas.FillRect(r, 0, 0, AllCorners, 1, ABmp.Canvas.Fill);
    ABmp.Canvas.DrawRectSides(r, 0, 0, AllCorners, 1, [TSide.Top, TSide.Bottom], ABmp.Canvas.Stroke);
    ABmp.Canvas.StrokeThickness := s{*2};

    ABmp.Canvas.Fill.Color := claWhite;

    //if AChecked then
    //begin

      //InflateRect(ASwitchRect, s/2, s/2);
    //  ABmp.Canvas.FillEllipse(ASwitchRect, 1);
    //end
    //else
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
  FTableItem.FTableView.Invalidate;
end;

constructor TksTableViewItemObject.Create(ATableItem: TksTableViewItem);
begin
  inherited Create;
  FTableItem := ATableItem;
  FHitText := True;
  FOffsetX := 0;
  FOffsetY := 0;
end;

procedure TksTableViewItemObject.MouseDown(x, y: single);
begin
  //
end;

procedure TksTableViewItemObject.MouseUp(x, y: single);
begin
  //
end;

function TksTableViewItemObject.ConsumesClick: Boolean;
begin
  Result := False;
end;

function TksTableViewItemObject.GetAlign: TksTableItemAlign;
begin
  Result := FAlign;
end;

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

  OffsetRect(Result, FPlaceOffset.x + FOffsetX, FPlaceOffset.y + FOffsetY);
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

procedure TksTableViewItemObject.SetOffsetX(const Value: single);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

procedure TksTableViewItemObject.SetOffsetY(const Value: single);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
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
  FFont.OnChanged := FontChanged;
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

procedure TksTableViewItemText.FontChanged(Sender: TObject);
begin
  Height := GetTextHeight(FText, FFont, FWordWrap, FTrimming)
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
  if FBackground <> claNull then
  begin
    ACanvas.Fill.Color := FBackground;
    ACanvas.FillRect(r, 0, 0, AllCorners, 1);
  end;
  RenderText(ACanvas, r.Left, r.Top, r.Width, r.Height, FText, FFont,
    FTextColor, FWordWrap, FTextAlign, FTextVertAlign, FTrimming);
end;

procedure TksTableViewItemText.SetBackground(const Value: TAlphaColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
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
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    Changed;
  end;
end;

procedure TksTableViewItemText.SetTextColor(const Value: TAlphaColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

procedure TksTableViewItemText.SetTextVertAlign(const Value: TTextAlign);
begin
  if FTextVertAlign <> Value then
  begin
    FTextVertAlign := Value;
    Changed;
  end;
end;

procedure TksTableViewItemText.SetTrimming(const Value: TTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    Changed;
  end;
end;

procedure TksTableViewItemText.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed;
  end;
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
  FShadow := TksTableViewShadow.Create;
  FShadow.Visible := False;
  FOwnsBitmap := False;
end;

destructor TksTableViewItemBaseImage.Destroy;
begin
  FreeAndNil(FShadow);
  if FBitmap <> nil then
    FBitmap.Free;
  inherited;
end;
function TksTableViewItemBaseImage.GetBitmap: TBitmap;
begin
  if FOwnsBitmap then
    Result := FBitmap
  else
    Result := FExternalBitmap;
end;

procedure TksTableViewItemBaseImage.Render(ACanvas: TCanvas);
var
  AShadowRect: TRectF;
  AShadowBmp: TBitmap;

begin
  if Bitmap <> nil then
  begin
    if FShadow.Visible then
    begin
      AShadowBmp := TBitmap.Create;
      try
        AShadowRect := GetObjectRect;
        OffsetRect(AShadowRect, FShadow.Offset, FShadow.Offset);
        AShadowBmp.Assign(Bitmap);
        AShadowBmp.ReplaceOpaqueColor(FShadow.Color);
        ACanvas.DrawBitmap(AShadowBmp, RectF(0, 0, AShadowBmp.Width, AShadowBmp.Height), AShadowRect, 1, True);
       // Bitmap.ReplaceOpaqueColor();
       // ACanvas.FillRect(AShadowRect, 0, 0, AllCorners, 1);
      finally
        AShadowBmp.Free;
      end;
    end;
    ACanvas.DrawBitmap(Bitmap, RectF(0, 0, Bitmap.Width, Bitmap.Height), GetObjectRect, 1, True);
  end;
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

procedure TksTableViewItemBaseImage.SetShadow(const Value: TksTableViewShadow);
begin
  FShadow.Assign(Value);
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

function TksTableViewItems.AddDateSelector(AText: string; ADate: TDateTime): TksTableViewItem;
begin
  Result := Additem(AText, '', FormatDateTime('ddd, dd mmmm, yyyy', ADate), atMore);
  Result.Selector := DateSelector;
  Result.FSelectionValue := ADate;
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
  UpdateIndexes;
  FTableView.UpdateItemRects;
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
  UpdateIndexes;

  FTableView.UpdateItemRects;
  FTableView.UpdateScrollingLimits;
end;

function TksTableViewItems.AddItemSelector(AText, ASelected: string; AItems: TStrings): TksTableViewItem;
begin
  Result := AddItem(AText, '', ASelected, atMore);
  Result.Selector := ItemPicker;
  Result.PickerItems.Assign(AItems);
  Result.FSelectionValue := ASelected;
end;

function TksTableViewItems.AddItemWithSwitch(AText: string; AChecked: Boolean; AID: string): TksTableViewItem;
var
  ASwitch: TksTableViewItemSwitch;
begin
  Result := AddItem(AText);
  Result.CanSelect := False;
  ASwitch := Result.AddSwitch(0, AChecked, TksTableItemAlign.Trailing);
  ASwitch.ID := AID;
  Result.CanSelect := False;
end;

function TksTableViewItems.AddItemSelector(AText, ASelected: string; AItems: array of string): TksTableViewItem;
var
  AStrings: TStrings;
  ICount: integer;
begin
  AStrings := TStringList.Create;
  try
    for ICount := Low(AItems) to High(AItems) do
      AStrings.Add(AItems[ICount]);
    Result := AddItemSelector(AText, ASelected, AStrings);
  finally
    FreeAndNil(AStrings);
  end;
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

function TksTableViewItem.AddEdit(AX, AY, AWidth: single; AText: string): TksTableViewItemEmbeddedEdit;
begin
  Result := TksTableViewItemEmbeddedEdit.Create(Self);
  Result.Width := AWidth;
  Result.FPlaceOffset := PointF(AX, AY);
  Result.VertAlign := TksTableItemAlign.Center;
  Result.Text := AText;
  FObjects.Add(Result);
end;

function TksTableViewItem.AddSwitch(x: single; AIsChecked: Boolean;
  const AAlign: TksTableItemAlign = TksTableItemAlign.Trailing): TksTableViewItemSwitch;
begin
  Result := TksTableViewItemSwitch.Create(Self);
  Result.Width := 50;
  Result.Height := 30;
  Result.FPlaceOffset := PointF(-8, 0);
  Result.SelectedColor := C_TABLEIEW_DEFAULT_SWITCH_COLOR;
  Result.Checked := AIsChecked;
  Result.Align := AAlign;
  Result.VertAlign := TksTableItemAlign.Center;
  FObjects.Add(Result);
end;

function TksTableViewItem.AddTable(AX, AY, AColWidth, ARowHeight: single; AColCount, ARowCount: integer): TksTableViewItemTable;
begin
  Result := TksTableViewItemTable.Create(Self);
  Result.DefaultRowHeight := ARowHeight;
  Result.DefaultColWidth := AColWidth;
  Result.ColCount := AColCount;
  Result.RowCount := ARowCount;
  Result.FPlaceOffset := PointF(AX, AY);
  Result.ResizeTable;

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

  FBitmap.Clear(FTableView.Appearence.ItemBackground.Color);

  //if FTableView.Appearence.ItemBackground. then
  //FBitmap.

  if FTableView.Appearence.AlternatingItemBackground <> claNull then
  begin
    if FIndex mod 2 = 0 then
      FBitmap.Clear(FTableView.Appearence.AlternatingItemBackground);
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
    if (FTableView.Appearence.ItemBackground.Kind <> TBrushKind.Solid) and
       (FPurpose = None) and
       (FTableView.ItemIndex <> FIndex) then
    begin
      FBitmap.Canvas.Fill.Assign(FTableView.Appearence.ItemBackground);
      FBitmap.Canvas.FillRect(ARect, 0, 0, AllCorners, 1);
    end;
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

procedure TksTableViewItem.PickerItemsChanged(Sender: TObject);
begin
  FSelector := TksTableItemSelector.NoSelector;
  if FPickerItems.Count > 0 then
    FSelector := TksTableItemSelector.ItemPicker;
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

  FPickerItems := TStringList.Create;
  (FPickerItems as TStringList).OnChange := PickerItemsChanged;

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
  //FTitleWidth := ksWidth60Percent;
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
  FreeAndNil(FPickerItems);
  inherited;
end;

procedure TksTableViewItem.DoClick(x, y: single);
var
  ABtn: TksTableViewActionButon;
  AObj: TksTableViewItemObject;
begin
  AObj := ObjectAtPos(x, y + ItemRect.Top);
  if AObj <> nil then
  begin
    AObj.MouseDown(x-AObj.ObjectRect.Left, y-AObj.ObjectRect.Top);
    if AObj.ConsumesClick then
      Exit;
  end;

  if FSelector = DateSelector then
  begin
    FTableView.SelectDate(Self, FSelectionValue, FTableView.DoSelectDate);
    Exit;
  end;

  if FSelector = ItemPicker then
  begin
    FTableView.SelectItem(Self, FPickerItems, FSelectionValue, FTableView.DoSelectPickerItem);
    Exit;
  end;

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
  Result.FPlaceOffset := PointF(x, y);
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
  Result.FPlaceOffset := PointF(x, y);
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

function TksTableViewItem.GetItemData(const AIndex: string): TValue;
begin
  if (FData <> nil) and not FData.TryGetValue(AIndex, Result) then
    Result := TValue.Empty;
end;

function TksTableViewItem.GetHasData(const AIndex: string): Boolean;
begin
  Result := (FData <> nil) and FData.ContainsKey(AIndex);
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
begin
  Result := GetItemRect;

  Result.Left := Result.Left + 8;
  Result.Right := Result.Right - 4
  ;

  if FAccessory.Accessory <> atNone then
    Result.Right := Result.Right - FAccessory.Width;
  //Result.Right := Result.Right - (FAccessory.Width + 10);
end;

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

function TksTableViewItem.ObjectAtPos(x, y: single): TksTableViewItemObject;
var
  ICount: integer;
  AObj: TksTableViewItemObject;
begin
  Result := nil;
  for ICount := 0 to FObjects.Count-1 do
  begin
    AObj := FObjects[ICount];
    if PtInRect(AObj.ObjectRect, PointF(x, y-ItemRect.Top)) then
    begin
      Result := AObj;
      Exit;
    end;
  end;
end;

procedure TksTableViewItem.RealignStandardObjects;
var
  ARect: TRectF;
begin
  FUpdating := True;
  try
    ARect := GetInternalRect;
    if (FPurpose = None) and (FTableView.RowIndicators.Visible) then
    begin
      FIndicator.FPlaceOffset := PointF(ARect.Left, 0);
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
      FImage.FPlaceOffset := PointF(ARect.Left, 0);
      ARect.Left := ARect.Left + FTableView.ItemImageSize + 4;
    end;
    if FAccessory.Accessory <> atNone then
    begin
      ARect.Right := ARect.Right-4;
      FAccessory.FPlaceOffset := Point(-4, 0);
    end;
    FTitle.FPlaceOffset := PointF(ARect.Left, 0);
    FTitle.Width := ARect.Width;// * (((Ord(FTitleWidth)+1)*10) / 100);

    FTitle.Height := GetTextHeight(FTitle.Text, FTitle.Font,  FTitle.WordWrap, FTitle.FTrimming, FTitle.Width);

    FSubTitle.FPlaceOffset := PointF(ARect.Left, 0);
    FSubTitle.Width := ARect.Width;// * (((Ord(FTitleWidth)+1)*10) / 100);
    FSubTitle.Height := GetTextHeight(FSubTitle.Text, FSubTitle.Font, FSubTitle.WordWrap, FSubTitle.FTrimming, FSubTitle.Width);
    if FSubTitle.Text <> '' then
    begin
      FTitle.FPlaceOffset := PointF(FTitle.FPlaceOffset.x, -9);
      FSubTitle.FPlaceOffset := PointF(FSubTitle.FPlaceOffset.x, 9);
    end;
    FDetail.FPlaceOffset := PointF(ARect.Right-(ARect.Width/2), 0);
    FDetail.Width := ARect.Width/2;
    FDetail.Height := GetTextHeight(FDetail.Text, FDetail.Font, FDetail.WordWrap, FDetail.FTrimming);
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
  if (FTableView.FullWidthSeparator = False) and (FPurpose = TksTableViewItemPurpose.None) then
    ASeperatorMargin := FTitle.FPlaceOffset.X;
  ACanvas.DrawLine(PointF(ASeperatorMargin, ARect.Top), PointF(ARect.Right, ARect.Top), 1);
  if (IsLastItem) or (FPurpose = TksTableViewItemPurpose.Header) then
    ACanvas.DrawLine(PointF(0, ARect.Bottom),
      PointF(ARect.Right, ARect.Bottom), 1);
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
    if Assigned(FTableView.OnItemCheckmarkChanged) then
      FTableView.OnItemCheckmarkChanged(Self, Self, FChecked);
  end;
end;

procedure TksTableViewItem.SetItemData(const AIndex: string; const Value: TValue);
begin
  if FData = nil then
    FData := TDictionary<string, TValue>.Create;
  FData.AddOrSetValue(AIndex, Value);
end;

procedure TksTableViewItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TksTableViewItem.SetHeight(const Value: single);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    FTableView.UpdateItemRects;
    CacheItem(True);
  end;
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

procedure TksTableViewItem.SetPickerItems(const Value: TStrings);
begin
  FPickerItems.Assign(Value);
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
       {
procedure TksTableViewItem.SetTitleWidth(const Value: TksTableViewTextWidth);
begin
  if FTitleWidth <> Value then
  begin
    FTitleWidth := Value;
    Changed;
  end;
end;      }

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

function TksTableViewItem.TextHeight(AText: string; AWordWrap, AIsHtml: Boolean; ATrimming: TTextTrimming;
  const AMaxWidth: single): single;
begin
  if AIsHtml then
    Result := GetTextSizeHtml(AText, FFont, AMaxWidth).y
  else
    Result := GetTextHeight(AText, FFont, AWordWrap, ATrimming, AMaxWidth);
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

  AHeight := GetTextHeight(AText, FFont, AWordWrap, Result.Trimming, AWidth);
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
  FItemBackground := TBrush.Create(TBrushKind.Solid, claWhite);
  FSeparatorColor := $FFF0F0F0;
  FSelectedColor := C_TABLEVIEW_DEFAULT_SELECTED_COLOR;
  FAlternatingItemBackground := claNull;
end;

destructor TksTableViewAppearence.Destroy;
begin
  FreeAndNil(FItemBackground);
  inherited;
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

procedure TksTableViewAppearence.SetItemBackground(const Value: TBrush);
begin
  FItemBackground.Assign(Value);
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
  if FUpdateCount > 0 then
    Exit;
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
  if FUpdateCount > 0 then
    Exit;
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

procedure TksTableView.ComboClosePopup(Sender: TObject);
begin
  (Sender as TStyledControl).Width := 0;
  RemoveObject(Sender as TFmxObject);
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

  {FSearchBox := TSearchBox.Create(nil);
  FSearchBox.Visible := False;
  FSearchBox.Align := TAlignLayout.Top;
  FSearchBox.OnTyping := DoFilterChanged;
  FSearchBox.OnChange := DoFilterChanged;}

  FSearchBox := TSearchBox.Create(Self);
  FSearchBox.Stored := False;
  FSearchBox.Locked := True;
  FSearchBox.Visible := False;
  FSearchBox.Align := TAlignLayout.Top;
  FSearchBox.OnTyping := DoFilterChanged;
  FSearchBox.OnChange := DoFilterChanged;
  FSearchBox.Parent := Self;

  FTextDefaults := TksTableViewTextDefaults.Create;
  FPullToRefresh := TksTableViewPullToRefresh.Create;
  Size.Width := C_TABLEVIEW_DEFAULT_WIDTH;
  Size.Height := C_TABLEVIEW_DEFAULT_HEIGHT;
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


  UpdateScrollingLimits;
  FSearchVisible := False;
  FShowAccessory := True;
  FItemIndex := -1;
  FItemHeight := C_TABLEVIEW_DEFAULT_ITEM_HEIGHT;
  FHeaderHeight := C_TABLEVIEW_DEFAULT_HEADER_HEIGHT;
  FItemImageSize := C_TABLEVIEW_DEFAULT_IMAGE_SIZE;
  FKeepSelection := False;

  FNeedsRefresh := False;
  FMouseDown := False;
  FCheckMarks := TksTableViewCheckMarks.cmNone;
  FShowSelection := True;
  FStickyHeaders := True;
  FFullWidthSeparator := True;

  FUpdateCount := 0;
  AddObject(FSearchBox);
end;

destructor TksTableView.Destroy;
begin
  if FSearchBox <> nil then
  begin
    FSearchBox.Parent := nil;
    FreeAndNil(FSearchBox);
  end;

  FreeAndNil(FRowIndicators);
  FreeAndNil(FBackgroundText);
  FreeAndNil(FFilteredItems);
  FreeAndNil(FItems);
  FreeAndNil(FAniCalc);
  FreeAndNil(FAppearence);
  FreeAndNil(FDeleteButton);
  FreeAndNil(FTextDefaults);
  FreeAndNil(FPullToRefresh);
  inherited;
end;

function TksTableView.CreateTimer(AInterval: integer; AProc: TTimerProc): TFmxHandle;
begin
  Result := 0;
  if FTimerService <> nil then
    Result := FTimerService.CreateTimer(AInterval, AProc);
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

procedure TksTableView.DoEmbeddedEditChange(AItem: TksTableViewItem; AEmbeddedEdit: TksTableViewItemEmbeddedBaseEdit);
begin
  if Assigned(FOnEmbeddedEditChange) then
    FOnEmbeddedEditChange(Self, AItem, AEmbeddedEdit, AEmbeddedEdit.Text);
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

procedure TksTableView.DoSelectDate(Sender: TObject);
var
  AAllow: Boolean;
  ARow: TksTableViewItem;
begin
  AAllow := True;
  ARow := TksTableViewItem(FDateSelector.TagObject);
  if AAllow then
  begin
    ARow.FSelectionValue := FDateSelector.Date;
    ARow.Detail.Text := FormatDateTime('ddd, dd mmmm, yyyy', FDateSelector.Date);
    ARow.Cached := False;
  end;
  if Assigned(FOnSelectDate) then
    FOnSelectDate(Self, ARow, FDateSelector.Date, AAllow);
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
      FMouseDownItem.ID, FMouseDownObject);
  HideAllActionButtons(False);
end;

procedure TksTableView.DoSelectPickerItem(Sender: TObject);
var
  AAllow: Boolean;
  ASelected: string;
  AItem: TksTableViewItem;
begin
  ASelected := '';
  AItem := TksTableViewItem(FCombo.TagObject);
  if FCombo.ItemIndex > -1 then
    ASelected := FCombo.Items[FCombo.ItemIndex];
  AAllow := True;
  if Assigned(FOnSelectPickerItem) then
    FOnSelectPickerItem(Self, AItem, ASelected, AAllow);
  if AAllow then
  begin
    AItem.FSelectionValue := ASelected;
    AItem.Detail.Text := ASelected;
    AItem.CacheItem(True);
  end;
end;

procedure TksTableView.DoSwitchClicked(AItem: TksTableViewItem; ASwitch: TksTableViewItemSwitch);
begin
  if Assigned(FOnSwitchClicked) then
    FOnSwitchClicked(Self, AItem, ASwitch, AItem.ID);
end;

procedure TksTableView.EndUpdate;
begin
  if FUpdateCount = 0 then
    Exit;
  FUpdateCount := FUpdateCount - 1;
  if FUpdateCount = 0 then
  begin
  UpdateItemRects;

    UpdateItemRects;
    UpdateScrollingLimits;
    CacheItems(True);
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

procedure TksTableView.HideFocusedControl;
var
  AParent: TFmxObject;
begin
  if FFocusedControl <> nil then
  begin
    FFocusedControl.HideControl;
    FFocusedControl := nil;
    AParent := Parent;
    while AParent <> nil do
    begin
      if (AParent is TForm) then
      begin
        (AParent as TForm).Focused := nil;
        Break;
      end;
      AParent := AParent.Parent;
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
  end;
end;

procedure TksTableView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
var
  AConsumesClick: Boolean;
begin
  if UpdateCount > 0 then Exit;
  inherited;
  if (FUpdateCount > 0) or (AIsSwiping) then
    Exit;

  FMouseDownObject := nil;
  FMouseDown := True;

  FSwipeDirection := ksSwipeUnknown;
  FMouseDownPoint := PointF(x, y);
  FMouseCurrentPos := FMouseDownPoint;

  FAniCalc.MouseDown(x, y);

  FMouseDownItem := GetItemFromYPos(y);
  if FMouseDownItem <> nil then
  begin
    FMouseDownObject := FMouseDownItem.ObjectAtPos(x, y);
    if (FMouseDownObject <> FFocusedControl) and (FFocusedControl <> nil) then
      HideFocusedControl;

    if FMouseDownObject <> nil then
    begin
      AConsumesClick := FMouseDownObject.ConsumesClick;
      FMouseDownItem.DoClick(FMouseDownPoint.x, (FMouseDownPoint.y - FMouseDownItem.ItemRect.Top) + ScrollViewPos);
      if AConsumesClick then
        Exit;
    end;

    if FMouseDownItem = nil then
      Exit;
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

  //if not (ssLeft in Shift) then
  //  FMouseDownItem := nil;

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

  if FMouseDownObject <> nil then
    FMouseDownObject.MouseUp(x, y);
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
      Canvas.Clear(GetColorOrDefault(FAppearence.Background, claWhite));
      if (csDesigning in ComponentState) then
      begin
        // design-time border
        Canvas.Stroke.Color := claBlack;
        Canvas.Stroke.Dash := TStrokeDash.Dash;
        Canvas.StrokeThickness := 1;
        Canvas.DrawRect(RectF(0, 0, Width, Height), 0, 0, AllCorners, 1);
      end;
      if (FPullToRefresh.Enabled) and (ScrollViewPos < 0) then
      begin
        Canvas.Stroke.Thickness := 1/GetScreenScale;
        Canvas.Stroke.Color := claDimgray;
        Canvas.DrawLine(PointF(0, 0-ScrollViewPos), PointF(Width, 0-ScrollViewPos), 1);
        // pull to refresh...
        if (FMouseDown) then
          FNeedsRefresh := (ScrollViewPos <= -50);

        Canvas.Fill.Color := FPullToRefresh.TextColor;
        Canvas.Font.Size := 16;

        if (FNeedsRefresh) and (ScrollViewPos <= -25) then
        begin
          Canvas.FillText(RectF(0, 0, Width, 50), FPullToRefresh.FReleaseText, False, 1, [], TTextAlign.Center);
          FNeedsRefresh := True;
        end
        else
          Canvas.FillText(RectF(0, 0, Width, 50), FPullToRefresh.FPullText, False, 1, [], TTextAlign.Center);
        Canvas.Fill.Color := GetColorOrDefault(FAppearence.Background, claWhite);
        Canvas.FillRect(RectF(0, 0-ScrollViewPos, Width, Height), 0, 0, AllCorners, 1);
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
  if FItems.Count = 0  then
    Exit;

  if FUpdateCount > 0 then
    Exit;
  UpdateItemRects;
  CacheItems(True);
end;

procedure TksTableView.SelectDate(ARow: TksTableViewItem; ASelected: TDateTime; AOnSelectDate: TNotifyEvent);
begin
  if FDateSelector = nil then
  begin
    FDateSelector := TDateEdit.Create(nil);
    FDateSelector.OnClosePicker := ComboClosePopup;
  end;
  FDateSelector.Position.X := ARow.ItemRect.Right - 100;
  FDateSelector.Position.Y := ARow.ItemRect.Top;
  FDateSelector.OnChange := nil;
  FDateSelector.TagObject := ARow;
  FDateSelector.Width := 0;
  {$IFDEF MSWINDOWS}
  FDateSelector.Width := 200;
  {$ENDIF}
  AddObject(FDateSelector);
  FDateSelector.Date := ASelected;
  FDateSelector.OnChange := AOnSelectDate;
  FDateSelector.OpenPicker;
end;

procedure TksTableView.SelectItem(ARow: TksTableViewItem; AItems: TStrings; ASelected: string; AOnSelectItem: TNotifyEvent);
begin
  if FCombo = nil then
  begin
    FCombo := TComboBox.Create(nil);
    FCombo.OnClosePopup := ComboClosePopup;
  end;
  FCombo.OnChange := nil;
  FCombo.Position.X := ARow.ItemRect.Right - 100;
  FCombo.Position.Y := ARow.ItemRect.Top;
  FCombo.TagObject := ARow;
  FCombo.Items.Assign(AItems);

  FCombo.ItemIndex := AItems.IndexOf(ASelected);
  FCombo.Width := 0;
  {$IFDEF MSWINDOWS}
  FCombo.Width := 200;
  {$ENDIF}
  FCombo.OnChange := AOnSelectItem;
  //FCombo.Align := TAlignLayout.Center;
  AddObject(FCombo);
  FCombo.DropDown;
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
  FFullWidthSeparator := Value;
  Invalidate;
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

procedure TksTableView.SetPullToRefresh(const Value: TksTableViewPullToRefresh);
begin
  if Value <> nil then
    FPullToRefresh.Assign(Value);
end;

procedure TksTableView.SetScrollViewPos(const Value: single);
begin

  if not SameValue(FScrollPos, Value, 1/GetScreenScale) then
  //if Round(FScrollPos) <> Round(Value) then

  begin
    HideFocusedControl;
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
      AShadowWidth := 1;

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
      ABmp.Canvas.StrokeThickness := 1;
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
    //ACanvas.DrawRect(ObjectRect, 0, 0, AllCorners, 1);
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

function TksTableViewItemSwitch.ConsumesClick: Boolean;
begin
  Result := True;
end;

constructor TksTableViewItemSwitch.Create(ATableItem: TksTableViewItem);
begin
  inherited;
  FChecked := False;
  FSelectedColor := C_TABLEIEW_DEFAULT_SWITCH_COLOR;
end;

procedure TksTableViewItemSwitch.MouseDown(x, y: single);
begin
  inherited;
  Checked := not Checked;
end;

procedure TksTableViewItemSwitch.MouseUp(x, y: single);
begin
  inherited;
  FTableItem.FTableView.DoSwitchClicked(FTableItem, Self);

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

{ TksTableViewShadow }

procedure TksTableViewShadow.Assign(ASource: TPersistent);
var
  ASrc: TksTableViewShadow;
begin
  ASrc := (ASource as TksTableViewShadow);
  Visible := ASrc.Visible;
  Color := ASrc.Color;
  Offset := ASrc.Offset;
end;

constructor TksTableViewShadow.Create;
begin
  FOffset := 2;
  FColor := claSilver;
  FVisible := True;
end;

procedure TksTableViewShadow.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;


{ TksListItemRowTableCell }

procedure TksListItemRowTableCell.Changed;
begin
  // need to implement.
end;

constructor TksListItemRowTableCell.Create(ATable: TksTableViewItemTable);
begin
  inherited Create;
  FTable := ATable;
  FTextSettings := TTextSettings.Create(nil);
  FFill := TBrush.Create(TBrushKind.Solid, claWhite);
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, claBlack);
  FPadding := TBounds.Create(RectF(0, 0, 0, 0));
  FTextSettings.FontColor := claBlack;
  FTextSettings.Font.Family := 'Arial';
  FTextSettings.HorzAlign := TTextAlign.Center;
  FTextSettings.VertAlign := TTextAlign.Center;
  FTextSettings.Font.Size := 12;
  FSides := AllSides;
  FVisible := True;
  {$IFDEF DEBUG}
  FText := 'CELL';
  {$ENDIF}
end;

destructor TksListItemRowTableCell.Destroy;
begin
  FreeAndNil(FTextSettings);
  FreeAndNil(FFill);
  FreeAndNil(FStroke);
  FreeAndNil(FPadding);
  inherited;
end;

procedure TksListItemRowTableCell.DrawToCanvas(x, y: single; ACanvas: TCanvas; ACol, ARow: integer; AShadow: TksTableViewShadow; AText: Boolean);
var
  s: single;
  ARect: TRectF;
  ATextRect: TRectF;
  AXShift: single;
  AYShift: single;
  AShadowRect: TRectF;
begin
  if not FVisible then
    Exit;
  s := GetScreenScale;
  if s < 2 then
    s := 2;
  //s := s * 1;

  with ACanvas do
  begin
    Stroke.Color := claBlack;
    Stroke.Thickness := (FStroke.Thickness * s)/2;
    AXShift := 0;
    AYShift := 0;
    if ACol = 0 then AXShift := 1*s;
    if ARow = 0 then AYShift := 1*s;
    ARect := RectF(x*s, y*s, (x+FWidth)*s, (y+FHeight)*s);

    if AText = False then
    begin
      if AShadow.Visible then
      begin
        // bottom shadow...
        AShadowRect := RectF(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom+(AShadow.Offset*s));
        ACanvas.Fill.Color := AShadow.Color;
        OffsetRect(AShadowRect, AShadow.Offset*s, 0);
        ACanvas.FillRect(AShadowRect, 0, 0, AllCorners, 1);
        // right shadow...
        AShadowRect := RectF(ARect.Right, ARect.Top, ARect.Right+(AShadow.Offset*s), ARect.Bottom);
        ACanvas.Fill.Color := AShadow.Color;
        OffsetRect(AShadowRect, 0, AShadow.Offset*s);
        ACanvas.FillRect(AShadowRect, 0, 0, AllCorners, 1);
      end;

      if IsFixedCell then
        ACanvas.Fill.Color := GetColorOrDefault(FFill.Color, FTable.FixCellColor)
      else
      begin
        ACanvas.Fill.Color := GetColorOrDefault(FFill.Color, claWhite);
        if FTable.Banding.Active then
        begin
          case FRow mod 2 of
            0: ACanvas.Fill.Color := GetColorOrDefault(FTable.Banding.Color1, claWhite);
            1: ACanvas.Fill.Color := GetColorOrDefault(FTable.Banding.Color2, claWhite);
          end;
        end;
      end;
      ACanvas.Fill.Kind := FFill.Kind;
      ACanvas.FillRect(RectF(ARect.Left+AXShift, ARect.Top+AYShift, ARect.Right, ARect.Bottom), 0, 0, AllCorners, 1);

      ACanvas.Stroke.Color :=  GetColorOrDefault(FStroke.Color, claDimgray);
      ACanvas.StrokeCap := TStrokeCap.Flat;
      ACanvas.StrokeJoin := TStrokeJoin.Miter;
      DrawRect(RectF(ARect.Left+AXShift, ARect.Top+AYShift, ARect.Right, ARect.Bottom), 0, 0, AllCorners, 1);

      ACanvas.Stroke.Color := ACanvas.Fill.Color;

      if (TSide.Left in FSides) = False then DrawRectSides(RectF(ARect.Left+AXShift, ARect.Top+AYShift, ARect.Right, ARect.Bottom), 0, 0, AllCorners, 1, [TSide.Left]);
      if (TSide.Top in FSides) = False then DrawRectSides(RectF(ARect.Left+AXShift, ARect.Top+AYShift, ARect.Right, ARect.Bottom), 0, 0, AllCorners, 1, [TSide.Top]);
      if (TSide.Right in FSides) = False then DrawRectSides(RectF(ARect.Left+AXShift, ARect.Top+AYShift, ARect.Right, ARect.Bottom), 0, 0, AllCorners, 1, [TSide.Right]);
      if (TSide.Bottom in FSides) = False then DrawRectSides(RectF(ARect.Left+AXShift, ARect.Top+AYShift, ARect.Right, ARect.Bottom), 0, 0, AllCorners, 1, [TSide.Bottom]);
    end
    else
    begin
      ARect := RectF(x, y, x+FWidth, y+FHeight);
      ATextRect := ARect;
      ATextRect.Left := ATextRect.Left + (FPadding.Left);
      ATextRect.Top := ATextRect.Top + (FPadding.Top * s);
      ATextRect.Right := ATextRect.Right - (FPadding.Right * s);
      ATextRect.Bottom := ATextRect.Bottom - (FPadding.Bottom * s);
      ACanvas.Font.Assign(FTextSettings.Font);
      ACanvas.Font.Size := FTextSettings.Font.Size;
      RenderText(ACanvas, ATextRect.Left, ATextRect.Top, ATextRect.Width, ATextRect.Height, FText,
                 ACanvas.Font, FTextSettings.FontColor, True, FTextSettings.HorzAlign,
                 FTextSettings.VertAlign, TTextTrimming.Character);
    end;
  end;
end;

function TksListItemRowTableCell.GetShadow: TksTableViewShadow;
begin
  Result := FTable.Shadow;
end;

function TksListItemRowTableCell.IsFixedCell: Boolean;
begin
  Result := (FRow <= (FTable.FixedRows-1)) or (FCol <= (FTable.FixedCols-1));
end;

procedure TksListItemRowTableCell.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TksListItemRowTableCell.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

procedure TksListItemRowTableCell.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Changed;
end;

{ TksListItemRowTableBanding }

procedure TksListItemRowTableBanding.Assign(Source: TPersistent);
begin
  inherited;
  FActive := (Source as TksListItemRowTableBanding).Active;
  FColor1 := (Source as TksListItemRowTableBanding).FColor1;
  FColor2 := (Source as TksListItemRowTableBanding).FColor2;
end;


constructor TksListItemRowTableBanding.Create;
begin
  inherited Create;
  FActive :=False;
  FColor1 := claNull;
  FColor2 := claNull;
end;

procedure TksListItemRowTableBanding.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

{ TksTableViewItemTable }

procedure TksTableViewItemTable.Clear;
var
  X, Y: integer;
  ARow: TksListItemRowTableRow;
begin
  for y := Low(FRows) to High(FRows) do
  begin
    ARow := FRows[y];
    for x := Low(ARow) to High(ARow) do
      FreeAndNil(ARow[x]);
  end;
end;

constructor TksTableViewItemTable.Create(ARow: TKsTableViewItem);
begin
  inherited;
  FShadow := TksTableViewShadow.Create;
  FBanding := TksListItemRowTableBanding.Create;
  SetLength(FRows, 5, 5);
  FBackground := claWhite;
  FBorderColor := claBlack;
  FColCount := 5;
  FRowCount := 5;
  FFixedCellColor := claGainsboro;
  FFixedRows := 1;
  FFixedCols := 1;
end;

destructor TksTableViewItemTable.Destroy;
begin
  Clear;
  FreeAndNil(FShadow);
  FreeAndNil(FBanding);
  inherited;
end;

function TksTableViewItemTable.GetCells(ACol, ARow: integer): TksListItemRowTableCell;
begin
  Result := FRows[ARow, ACol];
end;

function TksTableViewItemTable.GetColWidths(ACol: integer): single;
begin
  Result := FRows[0, ACol].Width;
end;

function TksTableViewItemTable.GetTableSize: TSizeF;
var
  ICount: integer;
begin
  Result.cx := 0;
  Result.cy := 0;

  if FRowCount > 0 then
  begin
    for ICount := Low(FRows) to High(FRows) do
    begin
      Result.cy := Result.cy + Frows[ICount, 0].Height;
    end;
  end;
  if (FColCount > 0) and (FColCount > 0) then
  begin
    for ICount := Low(FRows[0]) to High(FRows[0]) do
    begin
      Result.cx := Result.cx + Frows[0, ICount].Width;
    end;
  end;
end;

procedure TksTableViewItemTable.MergeRowCells(x, y, AMergeCount: integer);
var
  ICount: integer;
  ACell: TksListItemRowTableCell;
begin
  ACell := Cells[x, y];
  for ICount := x to x+(AMergeCount-1) do
  begin
    if ICount > x then
    begin
      Cells[ICount, y].Visible := False;
      ACell.Width := ACell.Width + Cells[ICount, y].Width;
    end;
  end;
end;

procedure TksTableViewItemTable.Render(ACanvas: TCanvas);
begin
  RenderTableContents(ACanvas, False); // render the grid.
  RenderTableContents(ACanvas, True);  // render the cell text
end;

procedure TksTableViewItemTable.RenderTableContents(ACanvas: TCanvas; AText: Boolean);
var
  IRowCount, ICellCount: integer;
  AXPos, AYPos: single;
  ARow: TksListItemRowTableRow;
  ACell: TksListItemRowTableCell;
  ASides: TSides;
  ABmp: TBitmap;
  ASize: TSizeF;
  AScale: single;
begin
  AScale := GetScreenScale;
  if AScale < 2 then
    AScale := 2;

  if AText then
  begin
    AXPos := ObjectRect.Left;
    AYPos := ObjectRect.Top;

    ACell := nil;
    for IRowCount := Low(FRows) to High(FRows) do
    begin
      ARow := FRows[IRowCount];
      for ICellCount := Low(ARow) to High(ARow) do
      begin
        ACell := ARow[ICellCount];
        begin
          ASides := [TSide.Right, TSide.Bottom];
          if ICellCount = 0 then ASides := ASides + [TSide.Left];
          if IRowCount = 0 then ASides := ASides + [TSide.Top];
          ACell.DrawToCanvas(AXpos, AYPos, ACanvas, ICellCount, IRowCount, FShadow, True);
        end;
        AXPos := AXPos + (ColWidths[ICellCount]);
      end;
      AYpos := AYpos + (ACell.Height);
      AXpos := ObjectRect.Left;
    end;
    Exit;
  end;


  ABmp := TBitmap.Create;
  try
    ASize := GetTableSize;
    FWidth := ASize.cx;
    FHeight := ASize.cy;
    ABmp.SetSize(Round((ASize.cx+FShadow.Offset+2) * (AScale)), Round((ASize.cy+FShadow.Offset+2) * (AScale)));
    ABmp.Clear(claNull);
    ABmp.Canvas.BeginScene;
    with ABmp.Canvas.Fill do
    begin
      Kind := TBrushKind.Solid;
      Color := FBackground;
    end;
    with ABmp.Canvas.Stroke do
    begin
      Kind := TBrushKind.Solid;
      Color := FBorderColor;
    end;

    AXPos := 0;
    AYPos := 0;

    ACell := nil;
    for IRowCount := Low(FRows) to High(FRows) do
    begin
      ARow := FRows[IRowCount];
      for ICellCount := Low(ARow) to High(ARow) do
      begin
        ACell := ARow[ICellCount];
        begin
          ASides := [TSide.Right, TSide.Bottom];
          if ICellCount = 0 then ASides := ASides + [TSide.Left];
          if IRowCount = 0 then ASides := ASides + [TSide.Top];
          ACell.DrawToCanvas(AXpos, AYPos, ABmp.Canvas, ICellCount, IRowCount, FShadow, False);
        end;
        AXPos := AXPos + (ColWidths[ICellCount]);
      end;
      AYpos := AYpos + (ACell.Height);
      AXpos := 0;
    end;
    ABmp.Canvas.EndScene;
    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height),
                       RectF(ObjectRect.Left, ObjectRect.Top, ObjectRect.Left+FWidth+FShadow.Offset+1, ObjectRect.Top+FHeight+FShadow.Offset+1), 1, True);
  finally
    FreeAndNil(ABmp);
  end;
end;

procedure TksTableViewItemTable.ResizeTable;
var
  AShadowWidth: integer;
  x,y: integer;
  ARow: TksListItemRowTableRow;
  ACell: TksListItemRowTableCell;
begin
  SetLength(FRows, FRowCount, FColCount);
  for y := Low(FRows) to High(FRows) do
  begin
    ARow := FRows[y];
    for x := Low(ARow) to High(ARow) do
    begin
      ACell := ARow[x];
      if ACell = nil then
      begin
        ACell := TksListItemRowTableCell.Create(Self);
        ACell.Width := FDefaultColWidth;
        ACell.Height := FDefaultRowHeight;
        ACell.FRow := y;
        ACell.FCol := x;
        FRows[y, x] := ACell;
      end;
    end;
  end;

  AShadowWidth := 0;
  if FShadow.Visible then
    AShadowWidth := FShadow.Offset;
  FWidth := GetTableSize.cx + AShadowWidth + (4*GetScreenScale);
  FHeight := GetTableSize.cy + AShadowWidth + (4*GetScreenScale);
end;

procedure TksTableViewItemTable.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackground := Value;
end;

procedure TksTableViewItemTable.SetBanding(const Value: TksListItemRowTableBanding);
begin
  FBanding.Assign(Value);
end;

procedure TksTableViewItemTable.SetBorderColor(const Value: TAlphaColor);
begin
  FBorderColor := Value;
end;

procedure TksTableViewItemTable.SetColColor(ACol: integer; AColor: TAlphaColor);
var
  ICount: integer;
begin
  for ICount := Low(FRows) to High(FRows) do
    FRows[ICount, ACol].Fill.Color := AColor;
end;

procedure TksTableViewItemTable.SetColCount(const Value: integer);
begin
  FColCount := Value;
  ResizeTable;
end;

procedure TksTableViewItemTable.SetColFont(ACol: integer; AFontName: TFontName; AColor: TAlphaColor; ASize: integer; AStyle: TFontStyles);
var
  ICount: integer;
  ACell: TksListItemRowTableCell;
begin
  for ICount := Low(FRows) to High(FRows) do
  begin
    ACell := FRows[ICount, ACol];
    with ACell.TextSettings do
    begin
      if AFontName <> '' then Font.Family := AFontName;
      Font.Size := ASize;
      FontColor := AColor;
      Font.Style := AStyle;
    end;
  end;
end;

procedure TksTableViewItemTable.SetColWidths(ACol: integer; const Value: single);
var
  ICount: integer;
begin
  for ICount := Low(FRows) to High(FRows) do
    FRows[ICount, ACol].Width := Value;
  ResizeTable;
end;

procedure TksTableViewItemTable.SetDefaultColWidth(const Value: single);
begin
  FDefaultColWidth := Value;
end;

procedure TksTableViewItemTable.SetDefaultRowHeight(const Value: single);
begin
  FDefaultRowHeight := Value;
end;

procedure TksTableViewItemTable.SetFixedCellColor(const Value: TAlphaColor);
begin
  FFixedCellColor := Value;
end;

procedure TksTableViewItemTable.SetRowColor(ARow: integer; AColor: TAlphaColor);
var
  ICount: integer;
begin
  for ICount := Low(FRows[ARow]) to High(FRows[ARow]) do
    FRows[ARow, ICount].Fill.Color := AColor;
end;

procedure TksTableViewItemTable.SetRowCount(const Value: integer);
begin
  FRowCount := Value;
  ResizeTable;
end;

procedure TksTableViewItemTable.SetRowFont(ARow: integer; AFontName: TFontName; AColor: TAlphaColor; ASize: integer; AStyle: TFontStyles);
var
  ICount: integer;
  ACell: TksListItemRowTableCell;
begin
  for ICount := Low(FRows[ARow]) to High(FRows[ARow]) do
  begin
    ACell := FRows[ARow, ICount];
    with ACell.TextSettings do
    begin
      if AFontName <> '' then Font.Family := AFontName;
      Font.Size := ASize;
      FontColor := AColor;
      Font.Style := AStyle;
    end;
  end;
end;

{ TksTableViewPullToRefresh }

procedure TksTableViewPullToRefresh.Assign(Source: TPersistent);
var
  ASrc: TksTableViewPullToRefresh;
begin
  if Source = nil then
    Exit;
  ASrc := (Source as TksTableViewPullToRefresh);
  FEnabled := ASrc.Enabled;
  FPullText := ASrc.PullText;
  FReleaseText := ASrc.ReleaseText;
  FFont.Assign(ASrc.Font);
  FTextColor := ASrc.TextColor;
end;

constructor TksTableViewPullToRefresh.Create;
begin
  FFont := TFont.Create;
  FEnabled := True;
  FPullText := 'pull to refresh';
  FReleaseText := 'release to refresh';
  FFont.Size := 16;
  FTextColor := claSilver;
end;

destructor TksTableViewPullToRefresh.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TksTableViewPullToRefresh.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TksTableViewPullToRefresh.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{ TksTableViewItemEmbeddedControl }

function TksTableViewItemEmbeddedControl.ConsumesClick: Boolean;
begin
  Result := True;
end;

constructor TksTableViewItemEmbeddedControl.Create(ATableItem: TksTableViewItem);
begin
  inherited;
  FControl := CreateControl;
  FWidth := FControl.Width;
  FHeight := FControl.Height;
  FFocused := False;
  FControl.OnExit := DoExitControl;
end;

destructor TksTableViewItemEmbeddedControl.Destroy;
begin
  FreeAndNil(FControl);
  inherited;
end;

procedure TksTableViewItemEmbeddedControl.DoExitControl(Sender: TObject);
begin
  HideControl;
end;

procedure TksTableViewItemEmbeddedControl.HideControl;
begin
  FTableItem.FTableView.RemoveObject(FControl);
  FFocused := False;
  FTableItem.FTableView.Invalidate;
  FTableItem.CacheItem(True);
end;

procedure TksTableViewItemEmbeddedControl.InitializeControl;
begin
  //
end;

procedure TksTableViewItemEmbeddedControl.SimulateClick(x, y: single);
var
  AParent   : TFmxObject;
  AForm     : TCommonCustomForm;
  AFormPoint: TPointF;
begin
  AParent := FControl.Parent;
  if AParent = nil then
    Exit;
  while not (AParent is TCommonCustomForm) do
    AParent := AParent.Parent;

  if (AParent is TCommonCustomForm) then
  begin
    AForm      := TCommonCustomForm(AParent);
    AFormPoint := FControl.LocalToAbsolute(PointF(X,Y));

    AForm.MouseDown(TMouseButton.mbLeft, [], AFormPoint.X, AFormPoint.Y);
    AForm.MouseUp(TMouseButton.mbLeft, [], AFormPoint.X, AFormPoint.Y);
  end;
end;

procedure TksTableViewItemEmbeddedControl.MouseDown(x, y: single);
begin
  ShowControl;
  SimulateClick(x, y);
end;

procedure TksTableViewItemEmbeddedControl.ShowControl;
var
  r: TRectF;
begin
  inherited;
  FTableItem.FTableView.HideFocusedControl;
  r := GetObjectRect;
  {$IFDEF MSWINDOWS}
  InflateRect(r, -4, -2 );
  {$ENDIF}
  FControl.SetBounds(r.Left, (FTableItem.ItemRect.Top - FTableItem.FTableView.ScrollViewPos) + r.Top, r.width, r.height);
  InitializeControl;
  FTableItem.FTableView.AddObject(FControl);
  FFocused := True;
  FTableItem.FTableView.FFocusedControl := Self;
  FTableItem.CacheItem(True);
  FTableItem.FTableView.Invalidate;

end;

procedure TksTableViewItemEmbeddedBaseEdit.DoEditChange(Sender: TObject);
begin
  Text := (Sender as TCustomEdit).Text;
end;

function TksTableViewItemEmbeddedBaseEdit.GetCustomEdit: TCustomEdit;
begin
  Result := (FControl as TCustomEdit);
end;

procedure TksTableViewItemEmbeddedBaseEdit.InitializeControl;
begin
  inherited;
  CustomEdit.Text := FText;
  CustomEdit.StyleLookup := 'transparentedit';
  CustomEdit.OnChange := DoEditChange;
  CustomEdit.OnTyping := DoEditChange;
end;

procedure TksTableViewItemEmbeddedBaseEdit.Render(ACanvas: TCanvas);
var
  ABmp: TBitmap;
  ARect: TRectF;
  ATextRect: TRectF;
begin
  ABmp := TBitmap.Create;
  try
    ABmp.SetSize(Round(Width * GetScreenScale), Round(Height * GetScreenScale));
    ABmp.BitmapScale := GetScreenScale;
    ABmp.Clear(claNull);
    ABmp.Canvas.BeginScene;
    try
      ARect := RectF(0, 0, (ABmp.Width / GetScreenScale), (ABmp.Height / GetScreenScale));

      ABmp.Canvas.Fill.Color := claWhite;
      ABmp.Canvas.Stroke.Color := claSilver;
      ABmp.Canvas.StrokeThickness := 1;

      ABmp.Canvas.FillRect(ARect, 0, 0, AllCorners, 1);
      ABmp.Canvas.DrawRect(ARect, 0, 0, AllCorners, 1);
    finally
      ABmp.Canvas.EndScene;
    end;
    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ObjectRect, 1, True);
    ACanvas.Stroke.Color := clablack;
  finally
    ABmp.Free;
  end;
  ATextRect := GetObjectRect;
  InflateRect(ATextRect, -6, 0);
  if not FFocused then
    RenderText(ACanvas, ATextRect, FText, nil, claBlack, False, TTextAlign.Leading, TTextAlign.Center, TTextTrimming.None);
end;

{ TksTableViewItemEmbeddedEdit }

function TksTableViewItemEmbeddedEdit.CreateControl: TStyledControl;
begin
  Result := TEdit.Create(FTableItem.FTableView);
  (Result as TEdit).StyleLookup := 'transparentedit';
end;

function TksTableViewItemEmbeddedEdit.GetEditControl: TEdit;
begin
  Result := (FControl as TEdit);
end;

procedure TksTableViewItemEmbeddedBaseEdit.SetText(const Value: string);
begin
  FText := Value;
  Changed;
  TableItem.FTableView.DoEmbeddedEditChange(TableItem, Self);
end;

initialization

  AccessoryImages := TksTableViewAccessoryImageList.Create;
  ATextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  AIsSwiping := False;

finalization

  FreeAndNil(AccessoryImages);
  FreeAndNil(ATextLayout);

end.
