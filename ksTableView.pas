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
*  See the License forthe specific language governing permissions and          *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksTableView;

interface

uses Classes, FMX.Controls, FMX.Layouts, FMX.Types, System.Types, Generics.Collections,
  FMX.Graphics, FMX.Objects, FMX.InertialMovement, System.UITypes,
  System.UIConsts, System.Rtti, FMX.DateTimeCtrls,
  FMX.Styles, FMX.Styles.Objects, FMX.Edit, FMX.SearchBox, FMX.ListBox, FMX.Effects;

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


  {$IFDEF MSWINDOWS}
  C_SCROLL_BAR_WIDTH = 8;
  {$ELSE}
  C_SCROLL_BAR_WIDTH = 8;
  {$ENDIF}

  {$IFDEF ANDROID}
  C_TABLEIEW_DEFAULT_SWITCH_COLOR = claDodgerBlue;
  C_TABLEVIEW_PAGE_SIZE = 50;
  {$ELSE}
  C_TABLEIEW_DEFAULT_SWITCH_COLOR = claLimeGreen;
  C_TABLEVIEW_PAGE_SIZE = 200;
  {$ENDIF}

type
  TksTableViewItem = class;
  TksTableViewItemObject = class;
  TksTableView = class;
  TksTableViewActionButtons = class;
  TksTableViewActionButton = class;
  TksTableViewItemSwitch = class;
  TksTableViewItemTable = class;
  TksTableViewItemEmbeddedBaseEdit = class;
  TksTableViewItemEmbeddedEdit = class;
  TksTableViewItemButton = class;
  TksTableViewSelectionOptions = class;

  TksTableItemAlign = (Leading, Center, Trailing, Fit); // SF - Added support for Fitting width/height
  TksSwipeDirection = (ksSwipeUnknown, ksSwipeLeftToRight, ksSwipeRightToLeft, ksSwipeTopToBottom, ksSwipeBottomToTop);
  TksTableViewShape = (ksRectangle, ksRoundRect, ksEllipse);
  TksTableViewItemPurpose = (None, Header);
  TksTableViewCheckMarks = (cmNone, cmSingleSelect, cmMultiSelect);
  TksTableViewActionButtonAlignment = (abLeftActionButtons, abRightActionButtons);
  TksImageDrawMode = (ksDrawModeStretch, ksDrawModeFit);
  TksTableViewButtonStyle = (ksButtonDefault, ksButtonSegmentLeft, ksButtonSegmentMiddle, ksButtonSegmentRight);
  TksTableViewButtonState = (ksPressed, ksUnpressed);
  TksTableViewItemAppearance = ( iaNormal, iaTile_Image, iaTile_TitleImage, iaTile_ImageTitle, iaTile_TitleImageSubTitle, iaTile_SubTitleImageTitle ); // SF - Tile
  TksTableItemSelector = (NoSelector, DateSelector, ItemPicker);
  TksTableViewOverlaySelectorPosition = (ksSelectorLeft, ksSelectorRight ); // SF - TC
  TksTableViewOverlaySelectorStyle = (ksBlankSpace, ksArrow, ksSemiCircle);

  TksTableViewRowCacheEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARow: TksTableViewItem; ARect: TRectF) of object;
  TksTableViewDeletingItemEvent = procedure(Sender: TObject; AItem: TksTableViewItem; var ACanDelete: Boolean) of object;
  TksTableViewDeleteItemEvent = procedure(Sender: TObject; AItem: TksTableViewItem) of object;
  TksTableViewItemClickEvent = procedure(Sender: TObject; x, y: single; AItem: TksTableViewItem; AId: string; ARowObj: TksTableViewItemObject) of object;
  TksItemSwipeEvent = procedure(Sender: TObject; ARow: TksTableViewItem; ASwipeDirection: TksSwipeDirection; AButtons: TksTableViewActionButtons) of object;
  TksItemActionButtonClickEvent = procedure(Sender: TObject; ARow: TksTableViewItem; AButton: TksTableViewActionButton) of object;
  TksTableViewItemSwitchEvent = procedure(Sender: TObject; AItem: TksTableViewItem; ASwitch: TksTableViewItemSwitch; ARowID: string) of object;
  TksTableViewItemButtonEvent = procedure(Sender: TObject; AItem: TksTableViewItem; AButton: TksTableViewItemButton; ARowID: string) of object;
  TksItemChecMarkChangedEvent = procedure(Sender: TObject; ARow: TksTableViewItem; AChecked: Boolean) of object;
  TksTableViewSelectDateEvent = procedure(Sender: TObject; AItem: TksTableViewItem; ASelectedDate: TDateTime; var AAllow: Boolean) of object;
  TksTableViewSelectPickerItem = procedure(Sender: TObject; AItem: TksTableViewItem; ASelected: string; var AAllow: Boolean) of object;
  TksTableViewEmbeddedEditChange = procedure(Sender: TObject; ARow: TksTableViewItem; AEdit: TksTableViewItemEmbeddedBaseEdit; AText: string) of object;
  TksTableViewScrollChangeEvent = procedure(Sender: TObject; AScrollPos, AMaxScrollLimit: single) of object;
  TksTableViewCanDragItemEvent = procedure(Sender: TObject; ADragRow: TksTableViewItem; var AllowDrag: Boolean) of object;             // SF - DD
  TksTableViewCanDropItemEvent = procedure(Sender: TObject; ADragRow, ADropRow: TksTableViewItem; var AllowDrop: Boolean)  of object;   // SF - DD
  TksTableViewDropItemEvent    = procedure(Sender: TObject; ADragRow, ADropRow: TksTableViewItem; var AllowMove: Boolean) of object;             // SF - DD
  TksTableViewSearchFilterChange = procedure(Sender: TObject; ASearchText: string) of object;



  TksAccessoryType = (atNone, atMore, atCheckmark, atDetail, atBack, atRefresh,
    atAction, atPlay, atRewind, atForward, atPause, atStop, atAdd, atPrior,
    atNext, atArrowUp, atArrowDown, atArrowLeft, atArrowRight, atReply,
    atSearch, atBookmarks, atTrash, atOrganize, atCamera, atCompose, atInfo,
    atPagecurl, atDetails, atRadioButton, atRadioButtonChecked, atCheckBox,
    atCheckBoxChecked, atUserDefined1, atUserDefined2, atUserDefined3);

  TksTableViewActionButton = class
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

  TksTableViewActionButtons = class(TObjectList<TksTableViewActionButton>)
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
    function ButtonFromXY(x, y: single): TksTableViewActionButton;
  public
    constructor Create(AOwner: TksTableViewItem);
    function AddButton(AText: string; AColor, ATextColor: TAlphaColor; AWidth: integer): TksTableViewActionButton;
  end;

  TksTableViewItemObject = class
  private
    [weak]FTableItem: TksTableViewItem;
    FAlign: TksTableItemAlign;
    FVertAlign: TksTableItemAlign;
    //FHitText: Boolean;
    FID: string;

    FWidth: single;
    FHeight: single;
    FPlaceOffset: TPointF;
    FHitTest: Boolean;
    FOffsetX: single;
    FOffsetY: single;
    FShowSelection: Boolean;
    FMouseDown: Boolean;
    FMargins: TBounds; // SF
    FHeightPercentange : Single;                 // SF - Pos
    FWidthPercentange : Single;                  // SF - Pos
    [weak]FPositionRelativeTo : TksTableViewItemObject; // SF - Pos
    procedure SetHeight(const Value: single);
    procedure SetWidth(const Value: single);
    procedure SetHeightPercentange(const Value: single);                  // SF - Pos
    procedure SetWidthPercentange(const Value: single);                   // SF - Pos
    procedure SetPositionRelativeTo(const Value: TksTableViewItemObject);  // SF - Pos
    procedure SetHitTest(const Value: Boolean);
    procedure SetOffsetX(const Value: single);
    procedure SetOffsetY(const Value: single);
    procedure SetShowSelection(const Value: Boolean);
  protected
    function ConsumesClick: Boolean; virtual;
    function GetAlign: TksTableItemAlign;
    function GetID: string;
    function GetVertAlign: TksTableItemAlign;
    function GetItemRect: TRectF;
    function GetObjectRect: TRectF; virtual;
    procedure Changed;
    procedure Render(ACanvas: TCanvas); virtual;
    procedure SetAlign(Value: TksTableItemAlign);
    procedure SetVertAlign(Value: TksTableItemAlign);
    procedure SetID(Value: string);
    property ObjectRect: TRectF read GetObjectRect;
    procedure Deselect;
  public
    constructor Create(ATableItem: TksTableViewItem); virtual;
    destructor Destroy; override;
    procedure MouseDown(x, y: single); virtual;
    procedure MouseUp(x, y: single); virtual;
    property Align: TksTableItemAlign read GetAlign write SetAlign;
    property Height: single read FHeight write SetHeight;
    property HitTest: Boolean read FHitTest write SetHitTest default True;
    property ID: string read GetID write SetID;
    property Margins: TBounds read FMargins write FMargins;
    //property PlaceOffset: TPointF read FPlaceOffset write SetPlaceOffset;
    property OffsetX: single read FOffsetX write SetOffsetX;
    property OffsetY: single read FOffsetY write SetOffsetY;
    property TableItem: TksTableViewItem read FTableItem;
    property VertAlign: TksTableItemAlign read GetVertAlign write SetVertAlign;
    property Width: single read FWidth write SetWidth;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default False;
    property HeightPercentange: single read FHeightPercentange write SetHeightPercentange;                  // SF - Pos
    property WidthPercentange: single read FWidthPercentange write SetWidthPercentange;                     // SF - Pos
    property PositionRelativeTo: TksTableViewItemObject read FPositionRelativeTo write SetPositionRelativeTo;  // SF - Pos

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
    FDrawMode: TksImageDrawMode;
    FShadow: TksTableViewShadow;
    [weak]FExternalBitmap: TBitmap;
  private
    FOwnsBitmap: Boolean;
    procedure SetBitmap(const Value: TBitmap);
    function GetBitmap: TBitmap;
    procedure SetOwnsBitmap(const Value: Boolean);
    procedure SetShadow(const Value: TksTableViewShadow);
    procedure SetDrawMode(const Value: TksImageDrawMode);
  protected
    procedure Render(ACanvas: TCanvas); override;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Shadow: TksTableViewShadow read FShadow write SetShadow;
    property OwnsBitmap: Boolean read FOwnsBitmap write SetOwnsBitmap default False;
    property DrawMode: TksImageDrawMode read FDrawMode write SetDrawMode;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    destructor Destroy; override;
  end;

  TksTableViewItemImage = class(TksTableViewItemBaseImage)
  public
    property Bitmap;
    property Shadow;
    property DrawMode;
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

  TksTableViewItemTileBackground = class(TksTableViewItemShape)                 // SF - Tile
  private                                                                       // SF - Tile
    FPadding: TBounds;                                                          // SF - Tile
  public                                                                        // SF - Tile
    constructor Create(ATableItem: TksTableViewItem); override;                 // SF - Tile
    destructor Destroy; override;                                               // SF - Tile
                                                                                // SF - Tile
    property Padding: TBounds read FPadding write FPadding;                     // SF - Tile
  end;                                                                          // SF - Tile


  TksTableViewItemButton = class(TksTableViewItemObject)
  private
    FText: string;
    FState: TksTableViewButtonState;
    FTintColor: TAlphaColor;
    procedure SetText(const Value: string);
    procedure SetTintColor(const Value: TAlphaColor);
  protected
    function ConsumesClick: Boolean; override;
    procedure Render(ACanvas: TCanvas); override;
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    procedure MouseDown(x, y: single); override;
    procedure MouseUp(x, y: single); override;
    property Text: string read FText write SetText;
    property TintColor: TAlphaColor read FTintColor write SetTintColor;
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
  public
    constructor Create(ATableItem: TksTableViewItem); override;
    property Accessory: TksAccessoryType read GetAccessory write SetAccessory;
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
    FTileBackground: TksTableViewItemTileBackground; // SF - Tile
    FImage: TksTableViewItemImage;
    FTitle: TksTableViewItemText;
    FSubTitle: TksTableViewItemText;
    FDetail: TksTableViewItemText;
    FAccessory: TksTableViewItemAccessory;
    FHeight: single;
    FHeightPercentage: single;  // SF - Addded support for a Row to be a percentage of the height
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
    FColCount: integer;  // SF - Addded support to override col count per row (used with headers)
    FIsFirstCol : Boolean;                    // SF - Tile
    FIsLastCol : Boolean;                     // SF - Tile
    FIsFirstRow : Boolean;                    // SF - Tile
    FIsLastRow : Boolean;                     // SF - Tile
    FAppearance : TksTableViewItemAppearance; // SF - Tile

    FDragging: Boolean;
    FFill : TBrush;                           // SF - BK
    function MatchesSearch(AFilter: string): Boolean;
    function IsVisible(AViewport: TRectF): Boolean;
    function GetHeight: single;
    function GetHeightPercentage: single;  // SF
    function GetItemRect: TRectF;
    function GetInternalRect: TRectF;

    function GetIndex: integer;
    function GetAbsoluteIndex: integer;
    function GetSearchIndex: string;
    function GetCached: Boolean;
    procedure SetSearchIndex(const Value: string);
    procedure SetItemRect(const Value: TRectF);
    procedure SetIndex(const Value: integer);
    procedure SetAppearance(const Value: TksTableViewItemAppearance); // SF - Tile

    procedure Changed;
    procedure RealignStandardObjects;
    procedure SetHeight(const Value: single);
    procedure SetHeightPercentage(const Value: single); // SF
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
    procedure DeselectObjects;
    procedure SetFill(const Value: TBrush);
  protected
    procedure Render(ACanvas: TCanvas; AScrollPos: single);
    procedure CacheItem(const AForceCache: Boolean = False);

  public
    constructor Create(ATableView: TksTableView); reintroduce;
    destructor Destroy; override;
    function ObjectAtPos(x, y: single): TksTableViewItemObject;
    function IsLastItem: Boolean;
    procedure RecreateCache;
    // image functions...
    function DrawBitmap(ABmp: TBitmap; ARect: TRectF): TksTableViewItemImage; overload;
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
    function DrawRect(x, y, AWidth, AHeight: single; AStroke, AFill: TAlphaColor): TksTableViewItemShape; overload;
    function DrawRect(ARect: TRectF; AStroke, AFill: TAlphaColor): TksTableViewItemShape; overload;


    function AddButton(AWidth: integer; AText: string;
                       const ATintColor: TAlphaColor = claNull;
                       const AVertAlign: TksTableItemAlign = TksTableItemAlign.Center;
                       const AYPos: integer = 0): TksTableViewItemButton; overload;
    function AddButton(AStyle: TksTableViewButtonStyle; const ATintColor: TAlphaColor = claNull): TksTableViewItemButton; overload;

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
    property HeightPercentage: single read GetHeightPercentage write SetHeightPercentage; // SF
    property ItemRect: TRectF read GetItemRect write SetItemRect;
    property IndicatorColor: TAlphaColor read GetIndicatorColor write SetIndicatorColor;
    // property InternalRect: TRectF read GetInternalRect;
    property ID: string read FID write FID;
    property TileBackground : TksTableViewItemTileBackground read FTileBackground;                      // SF - Tile
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
    property ColCount: integer read FColCount write FColCount default 0;  // SF
    property IsFirstCol: Boolean read FIsFirstCol;                                                          // SF - Tile
    property IsLastCol: Boolean read FIsLastCol;                                                            // SF - Tile
    property IsFirstRow: Boolean read FIsFirstRow;                                                          // SF - Tile
    property IsLastRow: Boolean read FIsLastRow;                                                            // SF - Tile
    property Appearance: TksTableViewItemAppearance read FAppearance write SetAppearance default iaNormal;  // SF - Tile
    property Fill: TBrush read FFill write SetFill;                                                         // SF - BK

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
    FBackground: TBrush;
    FItemBackground: TBrush;
    FAlternatingItemBackground: TAlphaColor;
    FSeparatorColor: TAlphaColor;
    FHeaderColor: TAlphaColor;
    FSelectedColor: TAlphaColor;
    procedure SetBackground(const Value: TBrush);
    procedure SetItemBackground(const Value: TBrush);
    procedure SetAlternatingItemBackground(const Value: TAlphaColor);
    procedure SetSeparatorBackground(const Value: TAlphaColor);
    procedure SetHeaderColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
  public
    constructor Create(AListView: TksTableView);
    destructor Destroy; override;
  published
    property Background: TBrush read FBackground write SetBackground;
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

  TksDragImage = class(TRectangle)
  private
    FBorder: TRectangle;
    FShadow: TShadowEffect;
    FMouseDownOffset: TPointF;
    procedure SetAllowDropColor(const Value: TStrokeBrush);
    function GetAllowDropColor: TStrokeBrush;
    property MouseDownOffset: TPointF read FMouseDownOffset write FMouseDownOffset;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Shadow: TShadowEffect read FShadow;
    property AllowDropStroke: TStrokeBrush read GetAllowDropColor write SetAllowDropColor;
  end;

  TksDragHighlightOptions = class(TPersistent)
  private
    FAllowDropStroke: TStrokeBrush;
    FDisallowDropStroke: TStrokeBrush;
    FEnabled: Boolean;
    procedure SetAllowDropStroke(const Value: TStrokeBrush);
    procedure SetDisallowDropStroke(const Value: TStrokeBrush);
  public
    constructor Create; virtual;
    destructor Destroy; override;

  published
    property AllowDropStroke: TStrokeBrush read FAllowDropStroke write SetAllowDropStroke;
    property DisallowDropStroke: TStrokeBrush read FDisallowDropStroke write SetDisallowDropStroke;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;

  TksDragDropOptions = class(TPersistent)
  private
    FEnabled: Boolean;
    FShadow: Boolean;
    FOpacity: single;
    FDragSpaceColor: TAlphaColor;
    FDragHighlightOptions: TksDragHighlightOptions;
    FLiveMoving : Boolean; // SF - LiveDD

    procedure SetOpacity(const Value: single);
    procedure SetShadow(const Value: Boolean);
    procedure SetDragHighlightOptions(const Value: TksDragHighlightOptions);
    procedure SetLiveMoving(const Value: Boolean);    // SF - LiveDD
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property DragHighlight: TksDragHighlightOptions read FDragHighlightOptions write SetDragHighlightOptions;
    property DragSpaceColor: TAlphaColor read FDragSpaceColor write FDragSpaceColor default $FFECECEC;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Shadow: Boolean read FShadow write SetShadow default True;
    property Opacity: single read FOpacity write SetOpacity;
    property LiveMoving: Boolean read FLiveMoving write SetLiveMoving;  // SF - LiveDD
  end;


  TksTableViewSelectionOverlayOptions = class(TPersistent)
  private
    [weak]FParent: TksTableViewSelectionOptions;
    FPosition: TksTableViewOverlaySelectorPosition;
    FStyle: TksTableViewOverlaySelectorStyle;
    FEnabled: Boolean;
    FStroke: TStrokeBrush;
    FBackgroundColor: TAlphaColor;
    FBitmap: TBitmap;
    FSize: integer;
    procedure SetPosition(const Value: TksTableViewOverlaySelectorPosition);
    procedure SetEnabled(const Value: Boolean);
    procedure SetStrokeBrush(const Value: TStrokeBrush);
    procedure RecreateIndicator(AHeight: single);
    procedure SetStyle(const Value: TksTableViewOverlaySelectorStyle);
    procedure SetSize(const Value: integer);
    procedure DoStrokeChanged(Sender: TObject);
  public
    constructor Create(AParent: TksTableViewSelectionOptions);
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property BackgroundColor: TAlphaColor read FBackgroundColor write FBackgroundColor default claWhite;
    property Position: TksTableViewOverlaySelectorPosition read FPosition write SetPosition default ksSelectorRight;
    property Stroke: TStrokeBrush read FStroke write SetStrokeBrush;
    property Style: TksTableViewOverlaySelectorStyle read FStyle write SetStyle default ksArrow;
    property Size: integer read FSize write SetSize default 1;
  end;


  TksTableViewSelectionOptions = class(TPersistent)
  private
    [weak]FTableView: TksTableView;
    FSelectionOverlay: TksTableViewSelectionOverlayOptions;
    FShowSelection: Boolean;
    FKeepSelection: Boolean;
    procedure SetKeepSelection(const Value: Boolean);
    procedure SetShowSelection(const Value: Boolean);
    procedure SetSelectionOverlay(const Value: TksTableViewSelectionOverlayOptions);
  public
    constructor Create(ATableView: TKsTableView);
    destructor Destroy; override;
  published
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
    property KeepSelection: Boolean read FKeepSelection write SetKeepSelection default False;
    property SelectionOverlay: TksTableViewSelectionOverlayOptions read FSelectionOverlay write SetSelectionOverlay;
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
    //FShowSelection: Boolean;
    FItemIndex: integer;
    FMouseDownPoint: TPointF;
    FMouseCurrentPos: TPointF;
    FUpdateCount: integer;
    FShowAccessory: Boolean;
    FSelectTimer: TFmxHandle;
    FDeselectTimer: TFmxHandle;
    //FKeepSelection: Boolean;
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
    FColCount: integer;  // SF - Addded support multiple columns
    FMouseEventsEnabled: Boolean;
    FMaxScrollPos: single;
    FDragDropImage : TksDragImage;                                                // SF - DD
    FDragDropScrollTimer: TFmxHandle;                                           // SF - DD
    FDragging: Boolean;
    FOnBeforePaint : TPaintEvent;                                               // SF - BK
    FOnAfterPaint : TPaintEvent;                                                // SF - BK
    FSelectionOptions: TksTableViewSelectionOptions;
    //FSelectorType : TksTableViewSelectorType;                                   // SF - TC (To be replaced)

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
    FOnButtonClicked: TksTableViewItemButtonEvent;
    FOnScrollViewChange: TksTableViewScrollChangeEvent;
    FOnCanDragItem : TksTableViewCanDragItemEvent;                              // SF - DD
    FOnCanDropItem : TksTableViewCanDropItemEvent;                              // SF - DD
    FOnDropItem : TksTableViewDropItemEvent;
    FDragDropOptions: TksDragDropOptions;                                    // SF - DD
    FOnSearchFilterChanged: TksTableViewSearchFilterChange;

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
    procedure SetColCount(const Value: integer);  // SF
    procedure SetKsItemHeight(const Value: integer);
    procedure SetSearchVisible(const Value: Boolean);
    //procedure SetShowSelection(const Value: Boolean);
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
    //procedure SetKeepSelection(const Value: Boolean);
    function GetItemFromPos(AXPos,AYPos: single): TksTableViewItem;  // SF
    //function GetItemFromYPos(AYPos: single): TksTableViewItem;
    procedure DoPullToRefresh;
    procedure UpdateFilteredItems;
    procedure DoSelectTimer;                                                    // SF - DD
    procedure UpdateDropImage(x, y: single);                              // SF - DD
    procedure DoDropScroll;                                                     // SF - DD
    procedure DoSelectItem;


    procedure SetCheckMarks(const Value: TksTableViewCheckMarks);
    procedure HideAllActionButtons(ASync: Boolean);
    procedure SetTextDefaults(const Value: TksTableViewTextDefaults);
    function CreateTimer(AInterval: integer; AProc: TTimerProc): TFmxHandle;
    procedure KillTimer(var ATimer: TFmxHandle);
    procedure KillAllTimers;
    procedure SetFullWidthSeparator(const Value: Boolean);
    procedure ComboClosePopup(Sender: TObject);
    procedure DoSwitchClicked(AItem: TksTableViewItem; ASwitch: TksTableViewItemSwitch);
    procedure DoButtonClicked(AItem: TksTableViewItem; AButton: TksTableViewItemButton);
    procedure SetPullToRefresh(const Value: TksTableViewPullToRefresh);
    procedure HideFocusedControl;
    procedure DoEmbeddedEditChange(AItem: TksTableViewItem; AEmbeddedEdit: TksTableViewItemEmbeddedBaseEdit);
    procedure EnableMouseEvents;
    procedure DisableMouseEvents;
    //procedure SetKeepSelection(const Value: Boolean);
    procedure SetSelectionOptions(const Value: TksTableViewSelectionOptions);
    //procedure SetShowSelection(const Value: Boolean);
  protected
    function GetTotalItemHeight: single;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; x, y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure DoMouseLeave; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
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
    procedure RedrawAllVisibleItems;
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
    property DragDropOptions: TksDragDropOptions read FDragDropOptions write FDragDropOptions;
    property FullWidthSeparator: Boolean read FFullWidthSeparator write SetFullWidthSeparator default True;

    property HeaderHeight: integer read FHeaderHeight write SetHeaderHeight default C_TABLEVIEW_DEFAULT_HEADER_HEIGHT;
    property ColCount: integer read FColCount write SetColCount default 0;  // SF
   // property SelectorType: TksTableViewSelectorType read FSelectorType write FSelectorType;  // SF - TC (To be replaced)

    property ItemHeight: integer read FItemHeight write SetKsItemHeight default C_TABLEVIEW_DEFAULT_ITEM_HEIGHT;
    property ItemImageSize: integer read FItemImageSize write SetItemImageSize default C_TABLEVIEW_DEFAULT_IMAGE_SIZE;
    //property KeepSelection: Boolean read FKeepSelection write SetKeepSelection default False;
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
    property SelectionOptions: TksTableViewSelectionOptions read FSelectionOptions write SetSelectionOptions;
    property ShowAccessory: Boolean read GetShowAccessory write SetShowAccessory default True;
    //property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
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
    property OnButtonClick: TksTableViewItemButtonEvent read FOnButtonClicked write FOnButtonClicked;

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
    property OnScrollViewChange: TksTableViewScrollChangeEvent read FOnScrollViewChange write FOnScrollViewChange;
    property OnSelectDate: TksTableViewSelectDateEvent read FOnSelectDate write FOnSelectDate;
    property OnSelectPickerItem: TksTableViewSelectPickerItem read FOnSelectPickerItem write FOnSelectPickerItem;
    property OnSearchFilterChanged: TksTableViewSearchFilterChange read FOnSearchFilterChanged write FOnSearchFilterChanged;
    property OnSwitchClick: TksTableViewItemSwitchEvent read FOnSwitchClicked write FOnSwitchClicked;
    property OnCanDragItem: TksTableViewCanDragItemEvent read FOnCanDragItem write FOnCanDragItem;       // SF - DD
    property OnCanDropItem: TksTableViewCanDropItemEvent read FOnCanDropItem write FOnCanDropItem;       // SF - DD
    property OnDropItem: TksTableViewDropItemEvent read FOnDropItem write FOnDropItem;                   // SF - DD
    property OnBeforePaint: TPaintEvent read FOnBeforePaint write FOnBeforePaint;                        // SF - BK
    property OnAfterPaint: TPaintEvent read FOnAfterPaint write FOnAfterPaint;                           // SF - BK
  end;

procedure Register;

//procedure DrawSwitch(ACanvas: TCanvas; ARect: TRectF; AChecked: Boolean; ASelectedColor: TAlphaColor);


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

    ABmp.Canvas.Stroke.Color := claSilver;
    ABmp.Canvas.FillEllipse(ASwitchRect, 1);
    ABmp.Canvas.DrawEllipse(ASwitchRect, 1);

    ABmp.Canvas.EndScene;
    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ARect, 1, False);
  finally
    FreeAndNil(ABmp);
  end;
end;

procedure DrawButton(ACanvas: TCanvas; ARect: TRectF; AText: string; ASelected: Boolean; AColor: TAlphaColor; AStyle: TksTableViewButtonStyle);
var
  ABmp: TBitmap;
  r: TRectF;
  ARadius: single;
  AFill, AOutline, AFontColor: TAlphaColor;
  AScale: single;
begin
  AScale := GetScreenScale;

  ARadius := 5*AScale;

  ABmp := TBitmap.Create(Round(ARect.Width * AScale), Round(ARect.Height * AScale));
  try
    if AColor = claNull then
      AColor := claDodgerblue;

    ABmp.Clear(claNull);
    ABmp.BitmapScale := AScale;
    r := RectF(0, 0, ABmp.Width, ABmp.Height);
    ABmp.Canvas.BeginScene;
    ABmp.Canvas.StrokeThickness := AScale;
    ABmp.Canvas.Stroke.Color := claSilver;
    ABmp.Canvas.Font.Size := (13 * AScale);

    if ASelected then
    begin
      AFill := AColor;
      AOutline := AColor;
      AFontColor := claWhite;
    end
    else
    begin
      AFill := claWhite;
      AOutline := AColor;
      AFontColor := AColor;
    end;
    ABmp.Canvas.Blending := True;
    ABmp.Canvas.Fill.Color := AFill;
    ABmp.Canvas.Stroke.Color := AOutline;
    if AStyle = ksButtonSegmentLeft then
    begin
      ABmp.Canvas.FillRect(r, ARadius, ARadius, [TCorner.TopLeft, TCorner.BottomLeft], 1, ABmp.Canvas.Fill);
      ABmp.Canvas.DrawRect(r, ARadius, ARadius, [TCorner.TopLeft, TCorner.BottomLeft], 1, ABmp.Canvas.Stroke);
    end
    else
    if AStyle = ksButtonSegmentRight then
    begin
      ABmp.Canvas.FillRect(r, ARadius, ARadius, [TCorner.TopRight, TCorner.BottomRight], 1, ABmp.Canvas.Fill);
      ABmp.Canvas.DrawRect(r, ARadius, ARadius, [TCorner.TopRight, TCorner.BottomRight], 1, ABmp.Canvas.Stroke);
    end
    else
    begin
      ABmp.Canvas.FillRect(r, 0, 0, AllCorners, 1, ABmp.Canvas.Fill);
      ABmp.Canvas.DrawRect(r, 0, 0, AllCorners, 1, ABmp.Canvas.Stroke);
    end;

    ABmp.Canvas.Fill.Color := AFontColor;
    ABmp.Canvas.FillText(r, AText, False, 1, [], TTextAlign.Center);

    ABmp.Canvas.EndScene;

    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ARect, 1, True);
  finally
    FreeAndNil(ABmp);
  end;
end;

// ------------------------------------------------------------------------------

procedure TksTableViewItemObject.Changed;
begin
  FTableItem.Cached := False;
  FTableItem.CacheItem(False);
  FTableItem.FTableView.Invalidate;
end;

constructor TksTableViewItemObject.Create(ATableItem: TksTableViewItem);
begin
  inherited Create;
  FMargins := TBounds.Create(TRectF.Empty); // SF
  FTableItem := ATableItem;
  FHitTest := True;
  FOffsetX := 0;
  FOffsetY := 0;
  FShowSelection := False;
  FMouseDown := False;
  FPositionRelativeTo := nil; // SF - Pos
end;

destructor TksTableViewItemObject.Destroy;
begin
  FreeAndNil(FMargins);
  inherited;
end;

procedure TksTableViewItemObject.Deselect;
begin
  if FMouseDown then
  begin
    FMouseDown := False;
    Changed;
  end;
end;

procedure TksTableViewItemObject.MouseDown(x, y: single);
begin
  if (FMouseDown = False) and (FHitTest) then
  begin
    FMouseDown := True;
    Changed;
  end;
end;

procedure TksTableViewItemObject.MouseUp(x, y: single);
begin
  //
end;

procedure TksTableViewItemObject.Render(ACanvas: TCanvas);
var
  ARect: TRectF;
begin
  if (FMouseDown) and (FShowSelection) then
  begin
    ARect := GetObjectRect;
    ACanvas.Fill.Color := FTableItem.FTableView.Appearence.SelectedColor;
    ACanvas.Stroke.Color := claSilver;
    ACanvas.FillRect(ARect, 0, 0, AllCorners, 1);
    ACanvas.DrawRect(ARect, 0, 0, AllCorners, 1);
  end;
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

{function TksTableViewItemObject.GetItemRect: TRectF;
begin
  Result := FTableItem.ItemRect;
end;  }

function TksTableViewItemObject.GetItemRect: TRectF;
begin
  if (FPositionRelativeTo<>Nil) then                                            // SF - PosFIX
    Result := FPositionRelativeTo.GetObjectRect                                 // SF - PosFIX
  else                                                                          // SF - PosFIX
    Result := FTableItem.ItemRect;
end;

{
function TksTableViewItemObject.GetObjectRect: TRectF;
var
  ARowRect: TRectF;
begin
  ARowRect := GetItemRect;
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
end;  }

function TksTableViewItemObject.GetObjectRect: TRectF;
var
  ARowRect: TRectF;
  RelativeOffset : TPointF;                                                     // SF - Pos
begin
  if (FTableItem.Appearance<>iaNormal) then                                     // SF - Tile
  begin                                                                         // SF - Tile
    Result := RectF(FPlaceOffset.X,FPlaceOffset.Y,                              // SF - Tile
                    FPlaceOffset.X+FWidth,FPlaceOffset.Y+FHeight);              // SF - Tile
    exit;                                                                       // SF - Tile
  end;                                                                          // SF - Tile



  ARowRect := GetItemRect;

  RelativeOffset := PointF(0, 0);

  if (FPositionRelativeTo <> nil) then                                          // SF - Pos
  begin                                                                         // SF - Pos
    RelativeOffset.X := FPositionRelativeTo.GetObjectRect.Left;                 // SF - Pos
    RelativeOffset.Y := FPositionRelativeTo.GetObjectRect.Top;                  // SF - Pos
  end;

  if (WidthPercentange > 0) then                                                // SF - Pos
    FWidth := ARowRect.Width * WidthPercentange / 100;                          // SF - Pos

  if (HeightPercentange > 0) then                                               // SF - Pos
    FHeight := ARowRect.Height * HeightPercentange / 100;                       // SF - Pos

  Result := RectF(ARowRect.Left + FMargins.Left, FMargins.Top, FWidth, FHeight); // SF

  Result := RectF(RelativeOffset.X + FMargins.Left, RelativeOffset.Y + FMargins.Top,
                  RelativeOffset.X + FMargins.Left + FWidth, RelativeOffset.Y + FMargins.Top + FHeight); // SF - Pos



  case FAlign of
    TksTableItemAlign.Center: OffsetRect(Result, ((ARowRect.Width - Result.Width) / 2), 0);
    TksTableItemAlign.Trailing: OffsetRect(Result, ((ARowRect.Width - Result.Width) - C_SCROLL_BAR_WIDTH) - FMargins.Right, 0); // SF
    TksTableItemAlign.Fit: Result.Width := ARowRect.Width - FMargins.Left - FMargins.Right;                                     // SF  - Pos                                                    // SF
    {TksTableItemAlign.Fit:                                                      // SF
      begin                                                                     // SF
        Result.Left  := FMargins.Left;                                          // SF
        Result.Width := ARowRect.Width - FMargins.Left - FMargins.Right;        // SF
      end;  }                                                                    // SF
  end;

  case FVertAlign of
    TksTableItemAlign.Center: OffsetRect(Result, 0, (ARowRect.Height - Result.Height) / 2);
    TksTableItemAlign.Trailing: OffsetRect(Result, 0, (ARowRect.Height - Result.Height) - FMargins.Bottom); // SF
    TksTableItemAlign.Fit: Result.Height := ARowRect.Height - FMargins.Top - FMargins.Bottom;               // SF  - Pos                                                    // SF
{    TksTableItemAlign.Fit:                                                      // SF
      begin                                                                     // SF
        Result.Top   := FMargins.Top;                                           // SF
        Result.Height := ARowRect.Height - FMargins.Top - FMargins.Bottom;      // SF
      end; }                                                                     // SF
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

procedure TksTableViewItemObject.SetHeightPercentange(const Value: single);     // SF - Pos
begin                                                                           // SF - Pos
  if FHeightPercentange <> Value then                                           // SF - Pos
  begin                                                                         // SF - Pos
    FHeightPercentange := Value;                                                // SF - Pos
    Changed;                                                                    // SF - Pos
  end;                                                                          // SF - Pos
end;                                                                            // SF - Pos

procedure TksTableViewItemObject.SetWidthPercentange(const Value: single);      // SF - Pos
begin                                                                           // SF - Pos
  if FWidthPercentange <> Value then                                            // SF - Pos
  begin                                                                         // SF - Pos
    FWidthPercentange := Value;                                                 // SF - Pos
    Changed;                                                                    // SF - Pos
  end;                                                                          // SF - Pos
end;                                                                            // SF - Pos

procedure TksTableViewItemObject.SetPositionRelativeTo(const Value: TksTableViewItemObject);  // SF - Pos
begin                                                                                         // SF - Pos
  if FPositionRelativeTo <> Value then                                                        // SF - Pos
  begin                                                                                       // SF - Pos
    FPositionRelativeTo := Value;                                                             // SF - Pos
    Changed;                                                                                // SF - Pos
  end;                                                                                      // SF - Pos
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

procedure TksTableViewItemObject.SetShowSelection(const Value: Boolean);
begin
  FShowSelection := Value;
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
  inherited;
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
  ARect: TRectF;
  AScaleX, AScaleY: single;
  AOriginalRect: TRectF;
begin
  inherited;
  if Bitmap <> nil then
  begin
    ARect := GetObjectRect;
    AOriginalRect := ARect;

    if FDrawMode = ksDrawModeFit then
    begin
      ARect := RectF(ARect.Left, ARect.Top, ARect.Left+Bitmap.Width, ARect.Top+Bitmap.Height);
      AScaleX := GetObjectRect.Width / ARect.Width;
      AScaleY := GetObjectRect.Height / ARect.Height;
      ARect.Height := ARect.Height * Min(AScaleX, AScaleY);
      ARect.Width := ARect.Width * Min(AScaleX, AScaleY);
      OffsetRect(ARect, (AOriginalRect.Width - ARect.Width)/2, (AOriginalRect.Height - ARect.Height)/2) ;

    end;

    if FShadow.Visible then
    begin
      AShadowBmp := TBitmap.Create;
      try
        AShadowRect := ARect;
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
    ACanvas.DrawBitmap(Bitmap, RectF(0, 0, Bitmap.Width, Bitmap.Height), ARect, 1, True);
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

procedure TksTableViewItemBaseImage.SetDrawMode(const Value: TksImageDrawMode);
begin
  if FDrawMode <> Value then
  begin
    FDrawMode := Value;
    Changed;
  end;
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

procedure TksDragDropOptions.SetLiveMoving(const Value: Boolean);               // SF - LiveDD
begin                                                                           // SF - LiveDD
  FLiveMoving := Value;                                                         // SF - LiveDD
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
    //if (Items[ICount].ItemRect.Left = 0) then     // SF - only add up items in first column
    if (Items[ICount].FIsFirstCol) then     // SF - Tile
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

function TksTableViewItem.AddButton(AWidth: integer; AText: string;
                                    const ATintColor: TAlphaColor = claNull;
                                    const AVertAlign: TksTableItemAlign = TksTableItemAlign.Center;
                                    const AYPos: integer = 0): TksTableViewItemButton;
begin
  Result := TksTableViewItemButton.Create(Self);
  Result.Align := TksTableItemAlign.Trailing;
  Result.VertAlign := AVertAlign;
  Result.OffsetY := AYPos;
  Result.Width := AWidth;
  Result.Height := 32;
  if ATintColor <> claNull then
  begin
    Result.TintColor := ATintColor;
  end;
  Result.Text := AText;
  FObjects.Add(Result);
  Changed;
end;

function TksTableViewItem.AddButton(AStyle: TksTableViewButtonStyle; const ATintColor: TAlphaColor): TksTableViewItemButton;
begin
  Result := AddButton(44, '', ATintColor);
  Result.Width := 44;
  Result.Height := 44;
end;

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
  ColumnOffset : Single;  // SF
  w,h: integer;
  DrawnSelection : Boolean;  // SF - BK
begin
  if (FUpdating) or (FTableView.UpdateCount > 0) then
    Exit;
  if (FItemRect.Height = 0) or (FItemRect.Width = 0) then
    Exit;

  if AForceCache then
  begin
    FBitmap.DisposeOf;
    FBitmap := nil;
    FBitmap := TBitmap.Create;
    FBitmap.BitmapScale := GetScreenScale;
    FCached := False;
  end;

  if (FCached = True) or (FCaching) then
    Exit;

  FCaching := True;

  ColumnOffset := ItemRect.Left;          // SF

  OffsetRect(FItemRect,-ColumnOffset,0);  // SF - Offset so columns 2+ draw properly


  RealignStandardObjects;

  w := Round(FItemRect.Width * GetScreenScale);
  h := Round(FItemRect.Height * GetScreenScale);

  FBitmap.SetSize(w, h);
  //FreeAndNil(FBitmap);

   //FBitmap := TBitmap.Create(w, h);
  //if w <> 720 then
  //  beep;
  //if h <> 120 then
  //  beep;

  //if FBitmap = nil then
  //  FBitmap := TBitmap.Create(w, h)
  //else
  //begin
  //  FreeAndNil(FBitmap);
    //FBitmap.SetSize(Round(FItemRect.Width * GetScreenScale), Round(FItemRect.Height * GetScreenScale));

  //end;

 {                                                                               // SF - BK
  FBitmap.Clear(FTableView.Appearence.ItemBackground.Color);                    // SF - BK
                                                                                // SF - BK
  //if FTableView.Appearence.ItemBackground. then                               // SF - BK
  //FBitmap.                                                                    // SF - BK
                                                                                // SF - BK
  if FTableView.Appearence.AlternatingItemBackground <> claNull then            // SF - BK
  begin                                                                         // SF - BK
    if FIndex mod 2 = 0 then                                                    // SF - BK
      FBitmap.Clear(FTableView.Appearence.AlternatingItemBackground);           // SF - BK
  end;                                                                          // SF - BK
}                                                                               // SF - BK

  DrawnSelection := false;                                                      // SF - BK
  if (FTableView.FSelectionOptions.ShowSelection) and (FCanSelect) then
  begin
    if (FIndex = FTableView.ItemIndex) or (Checked)  then
    begin                                                                       // SF - BK
      FBitmap.Clear(GetColorOrDefault(FTableView.Appearence.SelectedColor,      // SF - BK
                                      C_TABLEVIEW_DEFAULT_SELECTED_COLOR));     // SF - BK
      DrawnSelection := true;                                                   // SF - BK
    end;                                                                        // SF - BK
  end;                                                                          // SF - BK
{                                                                               // SF - BK
  if (FPurpose <> None) then                                                    // SF - BK
    FBitmap.Clear(GetColorOrDefault(FTableView.Appearence.HeaderColor,          // SF - BK
      C_TABLEVIEW_DEFAULT_HEADER_COLOR));                                       // SF - BK
}                                                                               // SF - BK
  ARect := RectF(0, 0, FBitmap.Width, FBitmap.Height);

  FBitmap.Canvas.BeginScene;
  try
    if not (DrawnSelection) then                                                                    // SF - BK
    begin                                                                                           // SF - BK
      if (FFill.Kind<>TBrushKind.None) then                                                         // SF - BK
        FBitmap.Canvas.Fill.Assign(FFill)                                                           // SF - BK
      else if (FPurpose<>None) then                                                                 // SF - BK
      begin                                                                                         // SF - BK
        FBitmap.Canvas.Fill.Kind  := TBrushKind.Solid;                                              // SF - BK
        FBitmap.Canvas.Fill.Color := GetColorOrDefault(FTableView.Appearence.HeaderColor,           // SF - BK
                                     C_TABLEVIEW_DEFAULT_HEADER_COLOR)                              // SF - BK
      end                                                                                           // SF - BK
      else                                                                                          // SF - BK
      begin                                                                                         // SF - BK
        if (FTableView.Appearence.AlternatingItemBackground <> claNull) and (FIndex mod 2 = 0) then // SF - BK
        begin                                                                                       // SF - BK
          FBitmap.Canvas.Fill.Kind  := TBrushKind.Solid;                                            // SF - BK
          FBitmap.Canvas.Fill.Color := FTableView.Appearence.AlternatingItemBackground;             // SF - BK
        end                                                                                         // SF - BK
        else                                                                                        // SF - BK
          FBitmap.Canvas.Fill.Assign(FTableView.Appearence.ItemBackground);                         // SF - BK
      end;                                                                                          // SF - BK
      FBitmap.Canvas.FillRect(ARect, 0, 0, AllCorners, 1);                                          // SF - BK
    end;

    if Assigned(FTableView.BeforeRowCache) then
      FTableView.BeforeRowCache(FTableView, FBitmap.Canvas, Self, ARect);

    case FTableView.RowIndicators.Outlined of
      False: FIndicator.Stroke.Kind := TBrushKind.None;
      True: FIndicator.Stroke.Kind := TBrushKind.Solid;
    end;
    FIndicator.Render(FBitmap.Canvas);
    FTileBackground.Render(FBitmap.Canvas);                                     // SF - Tile
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
  OffsetRect(FItemRect,ColumnOffset,0);          // SF
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
  FBitmap := TBitmap.Create;
  FBitmap.BitmapScale := GetScreenScale;
  FObjects := TksTableViewItemObjects.Create(FTableView);
  FFont := TFont.Create;
  FFont.Size := C_TABLEVIEW_DEFAULT_FONT_SIZE;
  FFill := TBrush.Create(TBrushKind.None, claNull); // SF - BK
  FIndicator := TksTableViewItemShape.Create(Self);
  FIndicator.VertAlign := TksTableItemAlign.Center;

  FTileBackground := TksTableViewItemTileBackground.Create(Self);               // SF - Tile

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
  FHeightPercentage := 0; // SF
  FCaching := False;
  FUpdating := False;
  FCanSelect := True;
  //FTitleWidth := ksWidth60Percent;
  FTagString := '';
  FTagInteger := 0;
  FColCount := 0;          // SF
  FAppearance := iaNormal; // SF - Tile
  FDragging := False;
end;

procedure TksTableViewItem.DeselectObjects;
var
  ICount: integer;
begin
  for ICount := 0 to FObjects.Count-1 do
  begin
    FObjects[ICount].Deselect;
  end;
end;

destructor TksTableViewItem.Destroy;
begin
  FreeAndNil(FIndicator);
  FreeAndNil(FTileBackground);                                                  // SF - Tile
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
  FreeAndNil(FFill);
  inherited;
end;

procedure TksTableViewItem.DoClick(x, y: single);
var
  ABtn: TksTableViewActionButton;
  AObj: TksTableViewItemObject;
begin
  DeselectObjects;
  AObj := ObjectAtPos(x, y + ItemRect.Top);
  if AObj <> nil then
  begin

    if AObj.HitTest then
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
      // custom button...
      if Assigned(FTableView.OnItemActionButtonClick) then
        FTableView.OnItemActionButtonClick(FTableView, Self, ABtn);
      FActionButtons.HideButtons(True);
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
    if FActionButtons.Count = 0 then
      Exit;

    if ADirecton = ksSwipeRightToLeft then FActionButtons.FAlignment := TksTableViewActionButtonAlignment.abRightActionButtons;
    if ADirecton = ksSwipeLeftToRight then FActionButtons.FAlignment := TksTableViewActionButtonAlignment.abLeftActionButtons;
      fActionButtons.ShowButtons;
  finally
    AIsSwiping := False;
  end;

end;

function TksTableViewItem.DrawBitmap(ABmp: TBitmap; ARect: TRectF): TksTableViewItemImage;
begin
  Result := DrawBitmap(ABmp, ARect.Left, ARect.Top, ARect.Width, ARect.Height);
end;

function TksTableViewItem.DrawBitmap(ABmp: TBitmap; x, AWidth, AHeight: single): TksTableViewItemImage;
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

function TksTableViewItem.DrawRect(ARect: TRectF; AStroke, AFill: TAlphaColor): TksTableViewItemShape;
begin
  Result := DrawRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height, AStroke, AFill);
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

function TksTableViewItem.GetHeightPercentage: single;  // SF
begin
  Result := FHeightPercentage;
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
  Result.Right := Result.Right - C_SCROLL_BAR_WIDTH;
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
  for ICount := FObjects.Count-1 downto 0 do
  begin
    AObj := FObjects[ICount];
    if PtInRect(AObj.ObjectRect, PointF(x, (y-ItemRect.Top)-FTableView.GetSearchHeight)) then
    begin
      Result := AObj;
      Exit;
    end;
  end;
end;

{procedure TksTableViewItem.RealignStandardObjects;
var
  ARect: TRectF;
  TileImageOffsetT : Single;                                                    // SF - Tile
  TileImageOffsetB : Single;                                                    // SF - Tile
  Margins          : TRectF;                                                    // SF - Tile
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
end;      }

procedure TksTableViewItem.RealignStandardObjects;
var
  ARect: TRectF;
  TileImageOffsetT : Single;                                                    // SF - Tile
  TileImageOffsetB : Single;                                                    // SF - Tile
  Margins          : TRectF;                                                    // SF - Tile
begin
  FUpdating := True;
  try
    if (FAppearance=TksTableViewItemAppearance.iaNormal) then                   // SF - Tile
    begin                                                                       // SF - Tile
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
      FTileBackground.Width:=0;                                                 // SF - Tile
      FTileBackground.Height:=0;                                                // SF - Tile
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
    end
    else if (FAppearance>=iaTile_Image) and (FAppearance<=iaTile_SubTitleImageTitle) then                                                   // SF - Tile
    begin                                                                                                                                   // SF - Tile
      FIndicator.Width := 0;                                                                                                                // SF - Tile
      FTitle.Width     := 0;                                                                                                                // SF - Tile
      FSubTitle.Width  := 0;                                                                                                                // SF - Tile
      FDetail.Width    := 0;                                                                                                                // SF - Tile
      FAccessory.Width := 0;                                                                                                                // SF - Tile
      TileImageOffsetT := 0;                                                                                                                // SF - Tile
      TileImageOffsetB := 0;                                                                                                                // SF - Tile
                                                                                                                                            // SF - Tile
      Margins.Left   := FTileBackground.Margins.Left;                                                                                       // SF - Tile
      Margins.Top    := FTileBackground.Margins.Top;                                                                                        // SF - Tile
      Margins.Right  := FTileBackground.Margins.Right;                                                                                      // SF - Tile
      Margins.Bottom := FTileBackground.Margins.Bottom;                                                                                     // SF - Tile
                                                                                                                                            // SF - Tile
      if (not FIsFirstCol) then Margins.Left   := Round(Margins.Left   / 2);                                                                // SF - Tile
      if (not FIsFirstRow) then Margins.Top    := Round(Margins.Top    / 2);                                                                // SF - Tile
      if (not FIsLastCol ) then Margins.Right  := Round(Margins.Right  / 2);                                                                // SF - Tile
      if (not FIsLastRow ) then Margins.Bottom := Round(Margins.Bottom / 2);                                                                // SF - Tile
                                                                                                                                            // SF - Tile
      ARect.Left   := FItemRect.Left   + Margins.Left;                                                                                      // SF - Tile
      ARect.Top    := Margins.Top;                                                                                                          // SF - Tile
      ARect.Width  := FItemRect.Width  - Margins.Left - Margins.Right;                                                                      // SF - Tile
      ARect.Height := FItemRect.Height - Margins.Top  - Margins.Bottom;                                                                     // SF - Tile
                                                                                                                                            // SF - Tile
      FTileBackground.FPlaceOffset := PointF(ARect.Left, ARect.Top);                                                                        // SF - Tile
      FTileBackground.Width        := ARect.Width;                                                                                          // SF - Tile
      FTileBackground.Height       := ARect.Height;                                                                                         // SF - Tile
                                                                                                                                            // SF - Tile
      ARect.Left   := ARect.Left   + FTileBackground.Padding.Left;                                                                          // SF - Tile
      ARect.Top    := ARect.Top    + FTileBackground.Padding.Top;                                                                           // SF - Tile
      ARect.Right  := ARect.Right  - FTileBackground.Padding.Right;                                                                         // SF - Tile
      ARect.Bottom := ARect.Bottom - FTileBackground.Padding.Bottom;                                                                        // SF - Tile
                                                                                                                                            // SF - Tile
      if (FAppearance=iaTile_TitleImage) or (FAppearance=iaTile_TitleImageSubTitle) then                                                    // SF - Tile
      begin                                                                                                                                 // SF - Tile
        FTitle.Width        := GetTextWidth(FTitle.Text, FTitle.Font);                                                                      // SF - Tile
        FTitle.Height       := GetTextHeight(FTitle.Text, FTitle.Font,  FTitle.WordWrap, FTitle.FTrimming, FTitle.Width);                   // SF - Tile
        FTitle.FPlaceOffset := PointF(ARect.Left + ((ARect.Width - FTitle.Width) / 2), ARect.Top);                                          // SF - Tile
                                                                                                                                            // SF - Tile
        TileImageOffsetT := FTitle.Height;                                                                                                  // SF - Tile
      end                                                                                                                                   // SF - Tile
      else if (FAppearance=iaTile_SubTitleImageTitle) then                                                                                  // SF - Tile
      begin                                                                                                                                 // SF - Tile
        FSubTitle.Width        := GetTextWidth(FSubTitle.Text, FSubTitle.Font);                                                             // SF - Tile
        FSubTitle.Height       := GetTextHeight(FSubTitle.Text, FSubTitle.Font, FSubTitle.WordWrap, FSubTitle.FTrimming, FSubTitle.Width);  // SF - Tile
        FSubTitle.FPlaceOffset := PointF(ARect.Left + ((ARect.Width - FSubTitle.Width) / 2), ARect.Top);                                    // SF - Tile
                                                                                                                                            // SF - Tile
        TileImageOffsetT := FSubTitle.Height;                                                                                               // SF - Tile
      end;                                                                                                                                  // SF - Tile
                                                                                                                                            // SF - Tile
      if (FAppearance=iaTile_ImageTitle) or (FAppearance=iaTile_SubTitleImageTitle) then                                                    // SF - Tile
      begin                                                                                                                                 // SF - Tile
        FTitle.Width        := GetTextWidth(FTitle.Text, FTitle.Font);                                                                      // SF - Tile
        FTitle.Height       := GetTextHeight(FTitle.Text, FTitle.Font,  FTitle.WordWrap, FTitle.FTrimming, FTitle.Width);                   // SF - Tile
        FTitle.FPlaceOffset := PointF(ARect.Left + ((ARect.Width - FTitle.Width) / 2), ARect.Bottom - FTitle.Height);                       // SF - Tile
                                                                                                                                            // SF - Tile
        TileImageOffsetB := FTitle.Height;                                                                                                  // SF - Tile
      end                                                                                                                                   // SF - Tile
      else if (FAppearance=iaTile_TitleImageSubTitle) then                                                                                  // SF - Tile
      begin                                                                                                                                 // SF - Tile
        FSubTitle.Width        := GetTextWidth(FSubTitle.Text, FSubTitle.Font);                                                             // SF - Tile
        FSubTitle.Height       := GetTextHeight(FSubTitle.Text, FSubTitle.Font, FSubTitle.WordWrap, FSubTitle.FTrimming, FSubTitle.Width);  // SF - Tile
        FSubTitle.FPlaceOffset := PointF(ARect.Left + ((ARect.Width - FSubTitle.Width) / 2), ARect.Bottom - FSubTitle.Height);              // SF - Tile
                                                                                                                                            // SF - Tile
        TileImageOffsetB := FSubTitle.Height;                                                                                               // SF - Tile
      end;                                                                                                                                  // SF - Tile
                                                                                                                                            // SF - Tile
      if FImage.Bitmap <> nil then                                                                                                          // SF - Tile
      begin                                                                                                                                 // SF - Tile
        FImage.Width := ARect.Width;                                                                                                        // SF - Tile
        FImage.Height := ARect.Height - TileImageOffsetT - TileImageOffsetB;                                                                // SF - Tile
        FImage.FPlaceOffset := PointF(ARect.Left, ARect.Top + TileImageOffsetT);                                                            // SF - Tile
        ARect.Left := ARect.Left + FTableView.ItemImageSize + 4;                                                                            // SF - Tile
      end;                                                                                                                                  // SF - Tile
    end;                                                                                                                                    // SF - Tile

  finally
    FUpdating := False;
  end;
end;


procedure TksTableViewItem.RecreateCache;
begin
  CacheItem(True);
end;

procedure TksTableViewItem.Render(ACanvas: TCanvas; AScrollPos: single);
var
  ARect: TRectF;
  AButtonRect: TRectF;
  AWidth: single;
  ASeperatorMargin: single;
  ASrcRect: TRectF;  // SF
begin
  ARect := FItemRect;
  OffsetRect(ARect, 0, 0 - AScrollPos);

  if (FDragging) and (FTableView.DragDropOptions.DragSpaceColor <> claNull) then
  begin
    ACanvas.Fill.Color := FTableView.DragDropOptions.DragSpaceColor;
    ACanvas.FillRect(ARect, 0, 0, AllCorners, 1);
    Exit;
  end;

  CacheItem;
  if FBitmap = nil then
    Exit;
  if FActionButtons.Visible = False then
    ACanvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), ARect, 1, True)
  else
  begin
    AWidth   := (FActionButtons.TotalWidth / 100) * FActionButtons.PercentWidth;
    ASrcRect := RectF(0, 0, FBitmap.Width, FBitmap.Height);     // SF

    case FActionButtons.FAlignment of
      abLeftActionButtons:
      begin
        ARect.Left     := ARect.Left + AWidth;                                       // SF
        ASrcRect.Right := ASrcRect.Right - AWidth;                                   // SF
        AButtonRect := RectF(FItemRect.Left, ARect.Top, ARect.Left, ARect.Bottom);   // SF
        FActionButtons.Render(ACanvas, AButtonRect);
      end;
      abRightActionButtons:
      begin
        ARect.Right   := ARect.Right - AWidth;                                       // SF
        ASrcRect.Left := ASrcRect.Left + AWidth;                                     // SF
        AButtonRect := RectF(ARect.Right, ARect.Top, FItemRect.Right, ARect.Bottom);
        FActionButtons.Render(ACanvas, AButtonRect);
      end;
    end;

    ACanvas.DrawBitmap(FBitmap, ASrcRect, ARect, 1, True);  // SF
  end;

  if (FPurpose = TksTableViewItemPurpose.Header) or (FAppearance=iaNormal) then                                   // SF - Tile
  begin                                                                                                           // SF - Tile
    // seperator...
    ACanvas.Stroke.Color := FTableView.Appearence.SeparatorColor;
    ACanvas.StrokeThickness := 1;
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Dash := TStrokeDash.Solid;
    if FPurpose = Header then
    begin
      ACanvas.Stroke.Color := $FFD2D2D2;
      ACanvas.StrokeThickness := 0.5;
    end;
    ASeperatorMargin := 0;
    if (FTableView.FullWidthSeparator = False) and (FPurpose = TksTableViewItemPurpose.None) then
      ASeperatorMargin := FTitle.FPlaceOffset.X;


    if (ARect.Left=0) then                                                                      // SF - TC
    begin                                                                                       // SF - TC
      ACanvas.DrawLine(PointF(ARect.Left + ASeperatorMargin, Round(ARect.Top) - 0.5),           // SF - TC
                       PointF(FTableView.Width             , Round(ARect.Top) - 0.5), 1);       // SF - TC
                                                                                                // SF - TC
      if (IsLastItem) or (FPurpose = TksTableViewItemPurpose.Header) then                       // SF - TC
        ACanvas.DrawLine(PointF(0               , Round(ARect.Bottom) - 0.5),                   // SF - TC
                         PointF(FTableView.Width, Round(ARect.Bottom) - 0.5), 1);               // SF - TC
    end;                                                                                        // SF - TC
                                                                                                // SF - TC
    if (FPurpose <> Header) and not (FIsLastCol) then                                           // SF - TC
      ACanvas.DrawLine(PointF(Round(ARect.Right) - 0.5, ARect.Top),                             // SF - TC
                       PointF(Round(ARect.Right) - 0.5, ARect.Bottom), 1);                      // SF - TC
                                                                                                // SF - TC


   (* AOverlay := FTableView.FSelectionOptions.SelectionOverlay;
    if AOverlay.Enabled then
    begin
      ACanvas.Stroke.Assign(AOverlay.Stroke);
      if (Self = FTableView.SelectedItem) then
      begin
        ACanvas.Stroke.Kind := TBrushKind.None;

      end;
      if (AOverlay.FPosition = ksSelectorRight) then
      begin
        {ACanvas.DrawLine(PointF(Round(ARect.Right), ARect.Top),
                         PointF(Round(ARect.Right) , ARect.Bottom), 1);}
        ACanvas.Stroke.Assign(AOverlay.Stroke);
        ACanvas.Stroke.Thickness := ACanvas.Stroke.Thickness -0.25;
        if Self = FTableView.SelectedItem then
        begin
          ACanvas.DrawLine(PointF(Round(ARect.Right) , ARect.Top),
                           PointF(Round(ARect.Right - (ARect.Height / 2)), ARect.Top + (ARect.Height / 2)), 1);
          ACanvas.DrawLine(PointF(Round(ARect.Right - (ARect.Height / 2)), ARect.Top + (ARect.Height / 2)),
                           PointF(Round(ARect.Right) , ARect.Bottom), 1);
        end;
      end
      else if (AOverlay.FPosition = ksSelectorLeft) then
      begin                                                                                                                                                                           // SF - TC
        ACanvas.DrawLine(PointF(Round(ARect.Left) + 0.5, ARect.Top),
                         PointF(Round(ARect.Left) + 0.5, ARect.Bottom), 1);
      end;
    end;    *)
  end;
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

procedure TksTableViewItem.SetFill(const Value: TBrush);
begin
  if Value <> nil then
    FFill.Assign(Value)
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

procedure TksTableViewItem.SetHeightPercentage(const Value: single);  // SF
begin
  if Value <> FHeight then
  begin
    FHeightPercentage := Value;
    FTableView.UpdateItemRects;
    CacheItem(True);
  end;
end;

procedure TksTableViewItem.SetIndex(const Value: integer);
begin
  FIndex := Value;
end;

procedure TksTableViewItem.SetAppearance(const Value: TksTableViewItemAppearance); // SF - Tile
begin                                                                              // SF - Tile
  if (FAppearance<>Value) then                                                     // SF - Tile
  begin                                                                            // SF - Tile
    FAppearance := Value;                                                          // SF - Tile
    FCached := False;                                                              // SF - Tile
    CacheItem;                                                                     // SF - Tile
  end;                                                                             // SF - Tile
end;                                                                               // SF - Tile

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
  FItemBackground := TBrush.Create(TBrushKind.Solid, claWhite);
  FBackground := TBrush.Create(TBrushKind.Solid, claWhite);
  FSeparatorColor := $FFF0F0F0;
  FSelectedColor := C_TABLEVIEW_DEFAULT_SELECTED_COLOR;
  FAlternatingItemBackground := claNull;
end;

destructor TksTableViewAppearence.Destroy;
begin
  FreeAndNil(FItemBackground);
  FreeAndNil(FBackground);
  inherited;
end;

procedure TksTableViewAppearence.SetAlternatingItemBackground
  (const Value: TAlphaColor);
begin
  FAlternatingItemBackground := Value;
end;

procedure TksTableViewAppearence.SetBackground(const Value: TBrush);
begin
  if Value <> nil then
    FBackground.Assign(Value);
end;

procedure TksTableViewAppearence.SetHeaderColor(const Value: TAlphaColor);
begin
  FHeaderColor := Value;
end;

procedure TksTableViewAppearence.SetItemBackground(const Value: TBrush);
begin
  if Value <> nil then
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

{procedure TksTableView.UpdateItemRects;
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

end; }

procedure TksTableView.UpdateItemRects;
var
  ICount: integer;
  AYPos: single;
  AXPos: single;            // SF
  ANoCols: integer;         // SF
  ACol: Integer;            // SF
  ARow: Integer;            // SF - Tile
  AHeight: single;          // SF
  AClientHeight: single;    // SF
  AWidth: single;
  AItem: TksTableViewItem;
  AIsLastRow: Boolean;      // SF - Tile
begin
  if FUpdateCount > 0 then
    Exit;
  UpdateFilteredItems;

  AHeight       := 0;
  ANoCols       := Max(1,ColCount);                                                // SF
  AWidth        := Width / ANoCols;                                                // SF
  AXPos         := 0;                                                              // SF
  AYPos         := GetSearchHeight;                                                 // SF
  ACol          := 0;                                                              // SF
  ARow          := 0;                                                              // SF - Tile
  AClientHeight := Height - GetSearchHeight();                                     // SF
                                                                                   // SF
  for ICount := 0 to FFilteredItems.Count - 1 do                                   // SF
  begin                                                                            // SF
    AItem := FFilteredItems[ICount];                                               // SF
                                                                                   // SF
    if (AItem.HeightPercentage>0) then                                             // SF
       AItem.FHeight := AClientHeight * AItem.HeightPercentage / 100.0;            // SF
                                                                                   // SF
    if (AItem.Purpose=TksTableViewItemPurpose.Header) then                         // SF
    begin                                                                          // SF
      ARow := 0;                                                                   // SF - Tile
      if (ACol>0) then                                                             // SF
      begin                                                                        // SF
        ACol  := 0;                                                                // SF
        AYPos := AYPos + AHeight;                                                  // SF
      end;                                                                         // SF
                                                                                   // SF
      AItem.ItemRect := RectF(0, AYPos, Width, AYPos + AItem.Height);              // SF
                                                                                   // SF
      if (AItem.ColCount>0) then                                                   // SF
        ANoCols := AItem.ColCount                                                  // SF
      else                                                                         // SF
        ANoCols := Max(1,AItem.ColCount);                                          // SF
                                                                                   // SF
      AWidth := Width / ANoCols;                                                   // SF
    end                                                                            // SF
    else                                                                           // SF
      AItem.ItemRect := RectF(AXPos, AYPos, AXPos + AWidth, AYPos + AItem.Height); // SF
                                                                                   // SF
    AItem.FIsFirstCol := (ACol=0);                                                 // SF - Tile
    AItem.FIsLastCol  := (ACol=ANoCols-1);                                         // SF - Tile
    AItem.FIsFirstRow := (ARow=0);                                                 // SF - Tile
    AItem.FIsLastRow  := false;                                                    // SF - Tile
    AItem.Index := ICount;                                                         // SF
                                                                                   // SF
    // First column item sets row height                                           // SF
    if (ACol=0) then                                                               // SF
      AHeight := AItem.Height;                                                     // SF
                                                                                   // SF
    if (AItem.Purpose<>TksTableViewItemPurpose.Header) and (ACol<ANoCols-1) then   // SF
    begin                                                                          // SF
      Inc(ACol);                                                                   // SF
      AXPos := AXPos + AWidth;                                                     // SF
    end                                                                            // SF
    else                                                                           // SF
    begin                                                                          // SF
      AYPos := AYPos + AHeight;                                                    // SF
      ACol  := 0;                                                                  // SF
      AXPos := 0;                                                                  // SF
      if (AItem.Purpose<>TksTableViewItemPurpose.Header) then                      // SF - Tile
        Inc(ARow);                                                                 // SF - Tile
    end;                                                                           // SF
  end;
  AIsLastRow := true;                                                              // SF - Tile
  for ICount := FFilteredItems.Count - 1 downto 0 do                               // SF - Tile
  begin                                                                            // SF - Tile
    AItem := FFilteredItems[ICount];                                               // SF - Tile
                                                                                   // SF - Tile
    if (FFilteredItems[ICount].Purpose=TksTableViewItemPurpose.Header) then        // SF - Tile
      AIsLastRow := true                                                           // SF - Tile
    else                                                                           // SF - Tile
    begin                                                                          // SF - Tile
      AItem.FIsLastRow  := AIsLastRow;                                             // SF - Tile
      if (AItem.FIsFirstCol) then                                                  // SF - Tile
        AIsLastRow := false;                                                       // SF - Tile
    end;                                                                           // SF - Tile
  end;                                                                             // SF - Tile                                                                            // SF
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


    FMaxScrollPos := Max((GetTotalItemHeight - Height) + GetSearchHeight, 0);
    Targets[1].Point := TPointD.Create(0,
                                       FMaxScrollPos);
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
  if Items.Count < C_TABLEVIEW_PAGE_SIZE then
    AStartPos := 0;
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
  FDragDropOptions := TksDragDropOptions.Create;
  FSelectionOptions := TksTableViewSelectionOptions.Create(Self);

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
  //FKeepSelection := False;

  FNeedsRefresh := False;
  FMouseDown := False;
  FCheckMarks := TksTableViewCheckMarks.cmNone;
  //FShowSelection := True;
  FStickyHeaders := True;
  FFullWidthSeparator := True;

  FMouseEventsEnabled := True;
  FUpdateCount := 0;
  FDragging := False;
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
  FreeAndNil(FDragDropOptions);
  FreeAndNil(FDeleteButton);
  FreeAndNil(FTextDefaults);
  FreeAndNil(FPullToRefresh);
  FreeAndNil(FSelectionOptions);
  inherited;
end;

procedure TksTableView.DisableMouseEvents;
begin
  FMouseEventsEnabled := False;
end;

function TksTableView.CreateTimer(AInterval: integer; AProc: TTimerProc): TFmxHandle;
begin
  Result := 0;
  if FTimerService <> nil then
    Result := FTimerService.CreateTimer(AInterval, AProc);
end;

procedure TksTableView.DoButtonClicked(AItem: TksTableViewItem; AButton: TksTableViewItemButton);
begin
  if Assigned(FOnSwitchClicked) then
    FOnButtonClicked(Self, AItem, AButton, AItem.ID);
end;

procedure TksTableView.DoDeselectItem;
begin
  //if FMouseDownObject <> nil then
  //  FMouseDownObject.DeselectObjects;

  if FTimerService = nil then
    Exit;
  KillTimer(FDeselectTimer);
  if ItemIndex <> -1 then
  begin
    FItems[ItemIndex].DeselectObjects;
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
  UpdateItemRects;            // SF
  UpdateScrollingLimits;      // SF
  Repaint;
  if Assigned(FOnSearchFilterChanged) then
    FOnSearchFilterChanged(Self, FSearchBox.Text);
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
  //MouseUp(TMouseButton.mbLeft, [], FMouseCurrentPos.x, FMouseCurrentPos.y);
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

procedure TksTableView.EnableMouseEvents;
begin
  FMouseEventsEnabled := True;
end;

procedure TksTableView.EndUpdate;
begin
  if FUpdateCount = 0 then
    Exit;
  FUpdateCount := FUpdateCount - 1;
  if FUpdateCount = 0 then
  begin
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
  //BeginUpdate;
  for ICount := 0 to FItems.Count - 1 do
  begin
    Items[ICount].Checked := False;
  end;
  //EndUpdate;
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

procedure TksTableView.DoSelectTimer;                                           // SF - DD
var
  Form : TCustomForm;
  //ScreenMousePos : TPointF;
  //FormMousePos : TPointF;
  AAllowDrag: Boolean;
begin
  if FMouseDownItem = nil then
    Exit;

  AAllowDrag := False;
  if (Assigned(FOnCanDragItem)) and (FDragDropOptions.Enabled = True) then
    FOnCanDragItem(Self, FMouseDownItem, AAllowDrag);

  if AAllowDrag then
  begin
    KillTimer(FSelectTimer);
    FMouseDownItem.FActionButtons.HideButtons(True);
    if (Root.GetObject() is TCustomForm) then
    begin
      Form := TCustomForm(Root.GetObject());

      FDragDropImage                    := TksDragImage.Create(Form);
      FDragDropImage.FShadow.Enabled := FDragDropOptions.Shadow;

      FDragDropImage.Parent             := Form;
      FDragDropImage.HitTest := False;
      FDragDropImage.Width              := FMouseDownItem.FBitmap.Width / GetScreenScale;
      FDragDropImage.Height             := FMouseDownItem.FBitmap.Height  / GetScreenScale;
      FDragDropImage.Fill.Bitmap.Bitmap := FMouseDownItem.FBitmap;

      FDragDropImage.Fill.Kind          := TBrushKind.Bitmap;
      FDragDropImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
      //FDragDropImage.Stroke.Thickness   := GetScreenScale() / 2;
      FDragDropImage.Opacity            := FDragDropOptions.Opacity;

      //FDragging := True;
      FDragDropScrollTimer := CreateTimer(100,DoDropScroll);

      Capture();
      FDragDropImage.MouseDownOffset := PointF(FMouseDownPoint.X - FMouseDownItem.ItemRect.Left,
                                               FMouseDownPoint.Y - FMouseDownItem.ItemRect.Top + GetScrollViewPos); // SF - DDFIX
      if FDragDropImage.MouseDownOffset.Y < 8 then
        FDragDropImage.MouseDownOffset := PointF(FDragDropImage.MouseDownOffset.X, FDragDropImage.MouseDownOffset.y + 8);
      UpdateDropImage(FMouseDownPoint.X+8, FMouseDownPoint.Y+8);
      FDragging := True;
      FMouseDownItem.FDragging := True;
    end;
  end
  else
    DoSelectItem;
end;

procedure TksTableView.UpdateDropImage(x, y: single);
var
  ScreenMousePos : TPointF;
  FormMousePos   : TPointF;
  AAllowDrop: Boolean;
  ADragOverItem: TksTableViewItem;
begin

  if FDragDropImage = nil then
    Exit;

  AAllowDrop := True;  // SF - LiveDD

  ADragOverItem := GetItemFromPos(x, y);
  if (Assigned(FOnCanDropItem)) and (ADragOverItem <> nil) then
  begin
    FOnCanDropItem(Self, FMouseDownItem, ADragOverItem, AAllowDrop);
    if FDragDropOptions.DragHighlight.Enabled then
    begin
      case AAllowDrop of
        True: FDragDropImage.AllowDropStroke := FDragDropOptions.DragHighlight.AllowDropStroke; //GetColorOrDefault(FDragDropOptions.DragHighlight.AllowDropColor, claDimgray);
        False: FDragDropImage.AllowDropStroke := FDragDropOptions.DragHighlight.DisallowDropStroke; //GetColorOrDefault(FDragDropOptions.DragHighlight.DisallowDropColor, claDimgray);
      end;
      // add 1 to the thickness so it shows inside the existing dark gray border...
      FDragDropImage.Stroke.Thickness := FDragDropImage.Stroke.Thickness + 1;
    end
    else
      FDragDropImage.Stroke.Color := claNull;
  end;

  FDragDropImage.Fill.Bitmap.Bitmap := FMouseDownItem.FBitmap;                                                     // SF - LiveDD
  FDragDropImage.Width              := FMouseDownItem.FBitmap.Width / GetScreenScale;                              // SF - LiveDD
  FDragDropImage.Height             := FMouseDownItem.FBitmap.Height  / GetScreenScale;                            // SF - LiveDD
                                                                                                                   // SF - LiveDD
  if (FDragDropImage.MouseDownOffset.X>FDragDropImage.Width) then                                                  // SF - LiveDD
    FDragDropImage.MouseDownOffset := PointF((FDragDropImage.Width / 2)+8,FDragDropImage.MouseDownOffset.Y);       // SF - LiveDD
                                                                                                                   // SF - LiveDD
  if (FDragDropImage.MouseDownOffset.Y>FDragDropImage.Height) then                                                 // SF - LiveDD
    FDragDropImage.MouseDownOffset := PointF(FDragDropImage.MouseDownOffset.X,(FDragDropImage.Height / 2)+8);      // SF - LiveDD


  ScreenMousePos := LocalToScreen(PointF(x, y));
  FormMousePos   := TForm(FDragDropImage.Parent).ScreenToClient(ScreenMousePos);
  FDragDropImage.SetBounds(FormMousePos.X - FDragDropImage.MouseDownOffset.X,
                           FormMousePos.Y - FDragDropImage.MouseDownOffset.Y,
                           FDragDropImage.Width, FDragDropImage.Height);
  Invalidate;
end;

procedure TksTableView.DoDropScroll;                                            // SF - DD
var
  MinHeight : Single;
  MaxHeight : Single;
begin
  if (FMouseCurrentPos.Y<0) then
  begin
    FScrolling   := false;
    FScrollPos   := FScrollPos - (GetScreenScale() * (0-FMouseCurrentPos.Y));
    MinHeight    := GetSearchHeight;

    if (FScrollPos<MinHeight) then
      FScrollPos := MinHeight;

    Repaint();
  end
  else if (FMouseCurrentPos.Y>Height) then
  begin
    FScrolling := false;
    FScrollPos := FScrollPos + (GetScreenScale() * (FMouseCurrentPos.Y-Height));
    MaxHeight  := Max((GetTotalItemHeight - Height) + GetSearchHeight, 0);

    if (FScrollPos>MaxHeight) then
      FScrollPos := MaxHeight;

    Repaint();
  end;
end;

{
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
end;  }


function TksTableView.GetItemFromPos(AXPos,AYPos: single): TksTableViewItem;          // SF
var                                                                                   // SF
  ICount: integer;                                                                    // SF
  AFiltered: TksTableViewItems;                                                       // SF
begin                                                                                 // SF
  Result := nil;                                                                      // SF
  AFiltered := FFilteredItems;                                                        // SF
  for ICount := 0 to AFiltered.Count - 1 do                                           // SF
  begin                                                                               // SF
    if PtInRect(AFiltered[ICount].ItemRect, PointF(AXPos, (AYPos + GetScrollViewPos) - GetSearchHeight))  // SF
    then                                                                              // SF
    begin                                                                             // SF
      Result := AFiltered[ICount];                                                    // SF
      Exit;                                                                           // SF
    end;                                                                              // SF
  end;                                                                                // SF
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

procedure TksTableView.KillTimer(var ATimer: TFmxHandle);
begin
  if FTimerService <> nil then
  begin
    if (ATimer<>0) then
    begin
      FTimerService.DestroyTimer(ATimer);
      ATimer := 0;
    end;
  end;
end;

procedure TksTableView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
var
  AConsumesClick: Boolean;
begin
  if (UpdateCount > 0) or (FMouseEventsEnabled = False) then
    Exit;
  inherited;
  if (FUpdateCount > 0) or (AIsSwiping) then
    Exit;

  Capture;

  FMouseDownObject := nil;
  FMouseDown := True;

  FSwipeDirection := ksSwipeUnknown;
  FMouseDownPoint := PointF(x, y);
  FMouseCurrentPos := FMouseDownPoint;


  FAniCalc.MouseDown(x, y);

  FMouseDownItem := GetItemFromPos(x,y);  // SF
  if FMouseDownItem <> nil then
  begin
    {if FMouseDownItem.FActionButtons.Visible then
    begin
      AActionBtn := FMouseDownItem.FActionButtons.ButtonFromXY(x, y);
      if AActionBtn <> nil then
      begin
        if Assigned(FOnItemActionButtonClick) then
          FOnItemActionButtonClick(Self, FMouseDownItem, AActionBtn);
      end;
    end; }

    FMouseDownObject := FMouseDownItem.ObjectAtPos(x, y + ScrollViewPos);

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
      FSelectTimer := CreateTimer(400, DoSelectTimer);                          // SF - DD
    end;

  end;
end;

procedure TksTableView.MouseMove(Shift: TShiftState; x, y: single);
var
  AMouseDownRect: TRectF;
  ADragOverItem : TksTableViewItem;                                                     // SF - LiveDD
  AAllowDrop    : Boolean;                                                              // SF - LiveDD
  I             : Integer;                                                              // SF - LiveDD
begin


  if (UpdateCount > 0) or (FMouseEventsEnabled = False) then
    Exit;
  FMouseCurrentPos := PointF(x, y);
  inherited;

  //if (Assigned(FDragDropImage)) then                                                  // SF - DD
  if FDragging then
  begin
    ADragOverItem := GetItemFromPos(x, y);                                                            // SF - DD
    if (FDragDropOptions.FLiveMoving) and (ADragOverItem<>Nil) and (ADragOverItem<>FMouseDownItem) then                // SF - LiveDD
    begin                                                                                             // SF - LiveDD
                                                               // SF - LiveDD
      for I:=FItems.IndexOf(ADragOverItem) downto 1 do                                                // SF - LiveDD
      begin                                                                                           // SF - LiveDD
        if (Items[I].Purpose<>TksTableViewItemPurpose.Header) then                                    // SF - LiveDD
          break;                                                                                      // SF - LiveDD
        ADragOverItem := Items[I-1];                                                                  // SF - LiveDD
      end;                                                                                            // SF - LiveDD
                                                                                                      // SF - LiveDD
      AAllowDrop := (ADragOverItem=Nil) or (ADragOverItem.Purpose<>TksTableViewItemPurpose.Header);   // SF - LiveDD
      if (Assigned(FOnCanDropItem)) then                                                              // SF - LiveDD
        FOnCanDropItem(Self, FMouseDownItem, ADragOverItem, AAllowDrop);                              // SF - LiveDD
                                                                                                      // SF - LiveDD
      if (AAllowDrop) and (ADragOverItem<>Nil) then                                                   // SF - LiveDD
      begin                                                                                           // SF - LiveDD
        FMouseDownItem.Appearance := ADragOverItem.Appearance;                                        // SF - LiveDD
        FMouseDownItem.Height     := ADragOverItem.Height;                                            // SF - LiveDD
                                                                                                      // SF - LiveDD
        FItems.Move(FItems.IndexOf(FMouseDownItem), FItems.IndexOf(ADragOverItem));                   // SF - LiveDD
                                                                                                      // SF - LiveDD
        UpdateItemRects;                                                                              // SF - LiveDD
                                                                                                      // SF - LiveDD
        RedrawAllVisibleItems();                                                                      // SF - LiveDD
      end;                                                                                            // SF - LiveDD
    end;                                                                                              // SF - LiveDD

    UpdateDropImage(FMouseCurrentPos.X+8, FMouseCurrentPos.Y+8);                        // SF - DD
    exit;                                                                               // SF - DD
  end;


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
var                                                                             // SF - DD
  //MouseDropItem :  TksTableViewItem;
  ACanDrop: Boolean;
  AAllowMove: Boolean;
  ADragOverItem: TksTableViewItem;
  Form: TCustomForm;
begin
  AAllowMove := False;
  //if FMouseDownObject <> nil then
  // FMouseDownObject.MouseUp(0, 0);

  if (UpdateCount > 0) or (FMouseEventsEnabled = False) then
    Exit;
  inherited;

  if (FDragging) then                                            // SF - DD
  begin                                                                         // SF - DD
    Form := TCustomForm(Root.GetObject());
    Form.RemoveObject(FDragDropImage);
    FreeAndNil(FDragDropImage);                                                 // SF - DD
    FDragDropImage := nil;
    KillTimer(FDragDropScrollTimer);                                            // SF - DD
    ReleaseCapture();                                                           // SF - DD
    ADragOverItem := GetItemFromPos(x, y);
                                                                            // SF - DD
    if (Assigned(FOnDropItem)) and (ADragOverItem <> nil) then
    begin
      ACanDrop := True;
      if Assigned(FOnCanDropItem) then
         FOnCanDropItem(Self, FMouseDownItem, ADragOverItem, ACanDrop);

      if ACanDrop then
      begin
        FOnDropItem(Self,FMouseDownItem, ADragOverItem, AAllowMove);         // SF - DD                                                                                // SF - DD
        if AAllowMove then
        begin
          // move the drag row to the new position...
          FItems.Move(FItems.IndexOf(FMouseDownItem), FItems.IndexOf(ADragOverItem));
          UpdateItemRects;
        end;
      end;
                                                                        // SF - DD

    end;
   FDragging := False;
    if FMouseDownItem <> nil then
      FMouseDownItem.FDragging := False;
    Invalidate;
    Exit;                                                                     // SF - DD
  end;

  if PtInRect(GetMouseDownBox, PointF(x,y)) then
  begin
    if FSelectTimer <> 0 then
    begin
      //KillTimer(FSelectTimer);
      //FSelectTimer := 0;
      if (FMouseDownObject = nil) or (FMouseDownObject.ConsumesClick = False) then
        DoSelectItem;
    end;
  end;

  //if FScrolling then
    FAniCalc.MouseUp(x, y);

  FMouseDown := False;
  if (FItemIndex > -1) and (FSelectionOptions.FKeepSelection = False) then
    DeselectItem(200);

  if (FMouseDownObject <> nil) and (FScrolling = False) then
    FMouseDownObject.MouseUp(x, y);
end;

procedure TksTableView.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
  Offset: Single;
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  if (not Handled) then
  begin
    if ssHorizontal in Shift then
    begin
      // Ignore horizontal
    end
    else
    begin
      Offset := Height / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      SetScrollViewPos(ScrollViewPos + Offset);
      Handled := True;
    end
  end;
end;

procedure TksTableView.Paint;
var
  ICount: integer;
  AItems: TksTableViewItems;
  AViewport: TRectF;
  AItem: TksTableViewItem;
  AItemsDrawn: Boolean;
  ARect: TRectF;
  ASelectedRect: TRectF;
  //LastY : Single; // SF - TC
begin
  if not FPainting then
  begin
    //LastY := 0;
    FPainting := True;
    try
      if (Assigned(OnBeforePaint)) then                                         // SF - BK
        OnBeforePaint(Self,Canvas);                                             // SF - BK

      if FAppearence.Background <> nil then
        Canvas.Fill.Assign(FAppearence.Background);
      Canvas.FillRect(RectF(0, 0, Width, Height), 0, 0, AllCorners, 1);

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
        //Canvas.Fill.Color := GetColorOrDefault(FAppearence.Background, claWhite);
        if FAppearence.Background <> nil then
          Canvas.Fill.Assign(FAppearence.Background);
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

          //LastY := AItems[ICount].ItemRect.Bottom - AViewport.Top;  // SF - TC
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

      if FSelectionOptions.SelectionOverlay.Enabled then
      begin
        with FSelectionOptions.SelectionOverlay do
        begin

          if Position = ksSelectorLeft then
          begin
            Canvas.Stroke.Assign(FSelectionOptions.SelectionOverlay.Stroke);
            //Canvas.DrawLine(PointF(0, 0), PointF(0, Height), 1);
            if SelectedItem <> nil then
            begin
              ASelectedRect := SelectedItem.GetItemRect;
              FSelectionOptions.SelectionOverlay.DrawToCanvas(Canvas, ASelectedRect);
            end;
          end
          else
          begin
            Canvas.Stroke.Assign(FSelectionOptions.SelectionOverlay.Stroke);
            Canvas.DrawLine(PointF(Width, 0), PointF(Width, Height), 1);
            if SelectedItem <> nil then
            begin
              ASelectedRect := SelectedItem.GetItemRect;
              OffsetRect(ASelectedRect, 0, 0-ScrollViewPos);
              FSelectionOptions.SelectionOverlay.DrawToCanvas(Canvas, ASelectedRect);
            end;
          end
        end;

      end;





      {if (FSelectionOptions.FSelectionOverlay.Enabled) and (LastY<Height) then  // SF - TC
      begin                                                                        // SF - TC
        Canvas.Stroke.Color := Appearence.SeparatorColor;                          // SF - TC
        Canvas.StrokeThickness := 1;                                               // SF - TC
        Canvas.Stroke.Kind := TBrushKind.Solid;                                    // SF - TC
        Canvas.Stroke.Dash := TStrokeDash.Solid;                                   // SF - TC
                                                                                   // SF - TC
        if (FSelectionOptions.FSelectionOverlay.Position = ksSelectorLeft) then                 // SF - TC
        begin                                                                      // SF - TC
          if (AViewport.Top<0) then                                                // SF - TC
            Canvas.DrawLine(PointF(Round(Width) - 0.5, 0),                         // SF - TC
                            PointF(Round(Width) - 0.5, -AViewport.Top), 1);        // SF - TC
                                                                                   // SF - TC
          Canvas.DrawLine(PointF(Round(Width) - 0.5, LastY),                       // SF - TC
                          PointF(Round(Width) - 0.5, Height), 1);                  // SF - TC
        end                                                                        // SF - TC
        else if (FSelectionOptions.FSelectionOverlay.Position = ksSelectorRight) then           // SF - TC
        begin                                                                      // SF - TC
          if (AViewport.Top<0) then                                                // SF - TC
            Canvas.DrawLine(PointF(0.5, 0),                                        // SF - TC
                            PointF(0.5, -AViewport.Top), 1);                       // SF - TC
                                                                                   // SF - TC
          Canvas.DrawLine(PointF(0.5, LastY),                                      // SF - TC
                          PointF(0.5, Height), 1);                                 // SF - TC
        end;                                                                       // SF - TC
      end;   }                                                                      // SF - TC


      if (Assigned(OnAfterPaint)) then                                          // SF - BK
        OnAfterPaint(Self,Canvas);                                              // SF - BK
    finally
      FPainting := False;
    end;
  end;
end;


procedure TksTableView.RedrawAllVisibleItems;
var
  AList: TList<TksTableViewItem>;
  ICount: integer;
begin
  AList := GetVisibleItems;
  try
    for ICount := 0 to AList.Count-1 do
      AList[ICount].RecreateCache;
    Invalidate;
  finally
    AList.Free;
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
    if FSelectionOptions.ShowSelection then
    begin
      if ASelected <> nil then
      begin
        ASelected.CacheItem(True);
      end;
      if ANewSelected <> nil then
        ANewSelected.CacheItem(True);
    end;
    Invalidate;
    if FMouseDown = False then
    begin
      if (FSelectionOptions.KeepSelection = False) and  (FItemIndex > -1) then
        DeselectItem(250);
    end;
  end;
end;

procedure TksTableView.SetColCount(const Value: integer);       // SF
begin                                                           // SF
  FColCount := Value;                                           // SF
  UpdateItemRects;                                              // SF
  Invalidate;                                                   // SF
end;                                                            // SF


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
  begin
    HideFocusedControl;
    FScrollPos := Value {- GetSearchHeight};
    HideAllActionButtons(True);
    Repaint; //Invalidate;
    if (Round(FScrollPos) = 0) and (FNeedsRefresh) then
    begin
      FNeedsRefresh := False;
      DoPullToRefresh;
    end;
    if Assigned(FOnScrollViewChange) then
      FOnScrollViewChange(Self, FScrollPos, FMaxScrollPos);
  end;
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

procedure TksTableView.SetSelectionOptions(const Value: TksTableViewSelectionOptions);
begin
  FSelectionOptions := Value;
end;

procedure TksTableView.SetShowAccessory(const Value: Boolean);
begin

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
  inherited;
  if (Width = 0) or (Height = 0) then
    Exit;

  ABmp := TBitmap.Create;
  try
    AShadowWidth := 0;

    if (FTableItem.FTableView.RowIndicators.Shadow) and (Self = FTableItem.FIndicator) then
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
      FFill.Color := FFill.Color;
      if FShape in [ksRectangle, ksRoundRect] then
      begin
        if FFill.Color <> claNull  then
          ABmp.Canvas.FillRect(ARect, FCornerRadius, FCornerRadius, AllCorners, 1);

        ABmp.Canvas.DrawRect(ARect, FCornerRadius, FCornerRadius, AllCorners, 1);
      end;
      if FShape = ksEllipse then
      begin
        if FFill.Color <> claNull  then
          ABmp.Canvas.FillEllipse(ARect, 1);
        ABmp.Canvas.DrawEllipse(ARect, 1);
      end;
    finally
      ABmp.Canvas.EndScene;
    end;
    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ObjectRect, 1, False);
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

{ TksTableViewItemTileBackground }                                                       // SF - Tile

constructor TksTableViewItemTileBackground.Create(ATableItem: TksTableViewItem);// SF - Tile
begin                                                                           // SF - Tile
  inherited;                                                                    // SF - Tile
  FPadding := TBounds.Create(RectF(5,5,5,5));                                   // SF - Tile
  CornerRadius := 5;                                                            // SF - Tile
  Width := 0;                                                                   // SF - Tile
  Height := 0;                                                                  // SF - Tile
  Margins.Left := 5;                                                            // SF - Tile
  Margins.Top := 5;                                                             // SF - Tile
  Margins.Right := 5;                                                           // SF - Tile
  Margins.Bottom := 5;                                                          // SF - Tile
end;
                                                                                // SF - Tile
destructor TksTableViewItemTileBackground.Destroy;                              // SF - Tile
begin                                                                           // SF - Tile
  FreeAndNil(FPadding);                                                         // SF - Tile
  inherited;                                                                    // SF - Tile
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

function TksTableViewActionButtons.AddButton(AText: string; AColor, ATextColor: TAlphaColor; AWidth: integer): TksTableViewActionButton;
begin
  Result := TksTableViewActionButton.Create(False);
  Result.Text := AText;
  Result.Color := AColor;
  Result.Width := AWidth;
  Result.TextColor := ATextColor;
  Add(Result);
end;

function TksTableViewActionButtons.ButtonFromXY(x, y: single): TksTableViewActionButton;
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
  if (Visible = False) or (Count = 0) then
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
        begin
          FAnimating := False;
          FTableItem.FTableView.EnableMouseEvents;
        end;
      end;
    end);
    ATask.Start;
    Exit;
  end;

  FAnimating := True;
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
  if (FAnimating) or (Count = 0) then
    Exit;
  FTableItem.FTableView.DisableMouseEvents;
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
    FTableItem.FTableView.EnableMouseEvents;
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

{ TksTableViewActionButton }

constructor TksTableViewActionButton.Create(AIsDelete: Boolean);
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
  inherited;
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
  inherited;
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
  inherited;
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

{ TksTableViewItemButton }

function TksTableViewItemButton.ConsumesClick: Boolean;
begin
  Result := True;
end;

constructor TksTableViewItemButton.Create(ATableItem: TksTableViewItem);
begin
  inherited;
  FTintColor := claNull;
  FText := '';
  FState := ksUnpressed;

end;

procedure TksTableViewItemButton.MouseDown(x, y: single);
begin
  if FHitTest = False then
    Exit;
  inherited;
  if FState <> ksPressed then
  begin
    FState := ksPressed;
    Changed;
  end;
end;

procedure TksTableViewItemButton.MouseUp(x, y: single);
begin
  inherited;
  if FState <> ksUnpressed then
  begin
    FState := ksUnpressed;
    Changed;
    FTableItem.FTableView.DoButtonClicked(FTableItem, Self);
  end;
end;

procedure TksTableViewItemButton.Render(ACanvas: TCanvas);
begin
  inherited Render(ACanvas);
  DrawButton(ACanvas, GetObjectRect, FText, FState = ksPressed, FTintColor, ksButtonDefault);
end;

procedure TksTableViewItemButton.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TksTableViewItemButton.SetTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
end;

{ TksDragDropOptions }

constructor TksDragDropOptions.Create;
begin
  inherited Create;
  FDragHighlightOptions := TksDragHighlightOptions.Create;
  FShadow := True;
  FOpacity := 1;
  FEnabled := False;
  FDragSpaceColor := $FFECECEC;
end;

destructor TksDragDropOptions.Destroy;
begin
  FreeAndNil(FDragHighlightOptions);
  inherited;
end;

procedure TksDragDropOptions.SetDragHighlightOptions(const Value: TksDragHighlightOptions);
begin
  FDragHighlightOptions := Value;
end;

procedure TksDragDropOptions.SetOpacity(const Value: single);
begin
  FOpacity := Value;
  if FOpacity < 0 then
    FShadow := False;
end;

procedure TksDragDropOptions.SetShadow(const Value: Boolean);
begin
  FShadow := Value;
  if FShadow = False then
    FOpacity := 1;
end;

{ TksDragImage }

constructor TksDragImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorder := TRectangle.Create(Self);
  FBorder.Stroke.Color := claDimgray;
  FBorder.Fill.Color := claNull;
  FBorder.Align := TAlignLayout.Client;
  Stroke.Thickness := 3;
  FShadow := TShadowEffect.Create(Self);
  FShadow.Direction := 45;
  FShadow.Distance := 5;
  FShadow.Softness := 0.2;
  Stroke.Color := claBlack;
  AddObject(FShadow);
  AddObject(FBorder);
end;

destructor TksDragImage.Destroy;
begin
  FreeAndNil(FShadow);
  inherited;
end;

function TksDragImage.GetAllowDropColor: TStrokeBrush;
begin
  Result := Stroke;
end;

procedure TksDragImage.SetAllowDropColor(const Value: TStrokeBrush);
begin
  Stroke.Assign(Value);
end;

{ TksDragHighlightOptions }

constructor TksDragHighlightOptions.Create;
begin
  FAllowDropStroke := TStrokeBrush.Create(TBrushKind.Solid, claLimegreen);
  FDisallowDropStroke := TStrokeBrush.Create(TBrushKind.Solid, claRed);
  FEnabled := True;
end;

destructor TksDragHighlightOptions.Destroy;
begin
  FreeAndNil(FAllowDropStroke);
  FreeAndNil(FDisallowDropStroke);
  inherited;
end;

procedure TksDragHighlightOptions.SetAllowDropStroke(const Value: TStrokeBrush);
begin
  FAllowDropStroke.Assign(Value);
end;

procedure TksDragHighlightOptions.SetDisallowDropStroke(const Value: TStrokeBrush);
begin
  FDisallowDropStroke.Assign(Value);
end;

{ TksTableViewSelectionOptions }

constructor TksTableViewSelectionOptions.Create(ATableView: TKsTableView);
begin
  inherited Create;
  FTableView := ATableView;
  FSelectionOverlay := TksTableViewSelectionOverlayOptions.Create(Self);
  FShowSelection := True;
  FKeepSelection := False;
end;

destructor TksTableViewSelectionOptions.Destroy;
begin
  FreeAndNil(FSelectionOverlay);
  inherited;
end;


procedure TksTableViewSelectionOptions.SetKeepSelection(const Value: Boolean);
begin
  if FKeepSelection <> Value then
  begin
    FKeepSelection := Value;
    FTableView.Invalidate;
  end;
end;

procedure TksTableViewSelectionOptions.SetSelectionOverlay(const Value: TksTableViewSelectionOverlayOptions);
begin
  FSelectionOverlay := Value;
end;


procedure TksTableViewSelectionOptions.SetShowSelection(const Value: Boolean);
begin
  if FShowSelection <> Value then
  begin
    FShowSelection := Value;

    FTableView.Invalidate;
  end;
end;

{ TksTableViewSelectionOverlayOptions }

constructor TksTableViewSelectionOverlayOptions.Create(AParent: TksTableViewSelectionOptions);
begin
  inherited Create;
  FParent := AParent;
  FBitmap := TBitmap.Create;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, claBlack);
  FPosition := ksSelectorRight;
  FBackgroundColor := claWhite;
  FStroke.OnChanged := DoStrokeChanged;
end;

destructor TksTableViewSelectionOverlayOptions.Destroy;
begin
  FreeAndNil(FStroke);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TksTableViewSelectionOverlayOptions.DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
begin
  if FBitmap.Height <> ARect.Height then
    RecreateIndicator(ARect.Height-1);
  case FPosition of
    ksSelectorLeft: ACanvas.DrawBitmap(FBitmap,
                                       RectF(0, 0, FBitmap.Width, FBitmap.Height),
                                       RectF(ARect.Left, ARect.Top, ARect.Left+FBitmap.Width, ARect.Bottom-1),
                                       1,
                                       True);
    ksSelectorRight: ACanvas.DrawBitmap(FBitmap,
                                        RectF(0, 0, FBitmap.Width, FBitmap.Height),
                                        RectF(ARect.Right - (FBitmap.Width/2), ARect.Top, ARect.Right + (FBitmap.Width/2), ARect.Bottom-1),
                                        1,
                                        True);
  end;
end;

procedure TksTableViewSelectionOverlayOptions.RecreateIndicator(AHeight: single);
var
  APath: TPathData;
  ASize: single;
  AOffset: single;
  AIndicatorRect: TRectF;
begin
  FBitmap.SetSize(Round(AHeight), Round(AHeight));
  FBitmap.Clear(claNull);
  FBitmap.Canvas.BeginScene;
  try
    FBitmap.Canvas.Stroke.Assign(FStroke);
    FBitmap.Canvas.Fill.Color := FBackgroundColor;

    ASize := 20 + (3*FSize);
    AOffset := (AHeight - ASize) / 2;

    AIndicatorRect := RectF(AOffset, AOffset, FBitmap.Width-AOffset, FBitmap.Height-AOffset);

    if FStyle = ksBlankSpace then
    begin
      FBitmap.Canvas.Stroke.Thickness := 2;
      FBitmap.Canvas.Stroke.Color := FBackgroundColor;
      FBitmap.Canvas.DrawLine(PointF(FBitmap.Width/2, 0), PointF(PointF(FBitmap.Width/2, FBitmap.Height-1)), 1);
    end;

    if FStyle = ksSemiCircle then
    begin
      FBitmap.Canvas.Stroke.Thickness := FBitmap.Canvas.Stroke.Thickness /2;
      FBitmap.Canvas.FillEllipse(AIndicatorRect, 1);
      FBitmap.Canvas.DrawEllipse(AIndicatorRect, 1);
    end;

    if FStyle = ksArrow then
    begin
      FBitmap.Canvas.Stroke.Thickness := FBitmap.Canvas.Stroke.Thickness /2;
      APath := TPathData.Create;
      try
        APath.MoveTo(PointF(AIndicatorRect.Left, AIndicatorRect.CenterPoint.Y));
        APath.LineTo(PointF(AIndicatorRect.CenterPoint.X, AIndicatorRect.Top));
        APath.LineTo(PointF(AIndicatorRect.Right, AIndicatorRect.CenterPoint.Y));
        APath.LineTo(PointF(AIndicatorRect.CenterPoint.X, AIndicatorRect.Bottom));
        APath.LineTo(PointF(AIndicatorRect.Left, AIndicatorRect.CenterPoint.Y));
        APath.ClosePath;
        FBitmap.Canvas.FillPath(APath, 1);
        FBitmap.Canvas.DrawPath(APath, 1);

      finally
        FreeAndNil(APath);
      end;
    end;

  finally
    FBitmap.Canvas.EndScene;
  end;
end;

procedure TksTableViewSelectionOverlayOptions.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    FParent.FTableView.Invalidate;
  end;
end;

procedure TksTableViewSelectionOverlayOptions.SetPosition(const Value: TksTableViewOverlaySelectorPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    FParent.FTableView.Invalidate;
  end;
end;

procedure TksTableViewSelectionOverlayOptions.SetSize(const Value: integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    FParent.FTableView.Invalidate;
  end;
end;

procedure TksTableViewSelectionOverlayOptions.SetStrokeBrush(const Value: TStrokeBrush);
begin
  if Value <> nil then
  begin
    FStroke.Assign(Value);
    FParent.FTableView.Invalidate;
  end;
end;

procedure TksTableViewSelectionOverlayOptions.SetStyle(const Value: TksTableViewOverlaySelectorStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    FBitmap.SetSize(0, 0);
    FParent.FTableView.Invalidate;
  end;
end;

procedure TksTableViewSelectionOverlayOptions.DoStrokeChanged(Sender: TObject);
begin
  FParent.FTableView.Invalidate;
end;

initialization

  AccessoryImages := TksTableViewAccessoryImageList.Create;
  ATextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  AIsSwiping := False;

finalization

  FreeAndNil(AccessoryImages);
  FreeAndNil(ATextLayout);

end.
