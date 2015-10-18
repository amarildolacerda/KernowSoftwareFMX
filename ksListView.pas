{*******************************************************************************
*                                                                              *
*  TksListView - High-Performance Mobile ListView Component                    *
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

{ *** HTML SUPPORT - PLEASE READ ***

In order to use the HTML formatting within ksListView, you will need to have the
TMS Pack for FireMonkey installed.

You can get this from the following link...
http://www.tmssoftware.com/site/tmsfmxpack.asp

Once installed, simply uncomment the conditional define in ksDrawFunctions.pas

}

unit ksListView;

interface

{$IFDEF VER290}
  {$DEFINE XE8_OR_NEWER}
{$ENDIF}

{$IFDEF VER300}
  {$DEFINE XE8_OR_NEWER}
  {$DEFINE XE10_OR_NEWER}
{$ENDIF}

uses
  Classes, FMX.Types, FMX.Controls, FMX.ListView, Types, FMX.TextLayout,
  FMX.ListView.Types, FMX.Graphics, Generics.Collections, System.UITypes,
  {$IFDEF XE8_OR_NEWER} FMX.ImgList, {$ENDIF} System.Rtti,
  System.UIConsts, FMX.StdCtrls, FMX.Styles.Objects, System.Generics.Collections,
  FMX.ListBox, FMX.DateTimeCtrls, FMX.Menus, FMX.Objects, FMX.SearchBox,
  FMX.Edit, FMX.SpinBox, FMX.Memo
  {$IFDEF XE10_OR_NEWER}, FMX.ListView.Appearances {$ENDIF}
  ;

const
  C_LONG_TAP_DURATION     = 5;  // 500 ms
  C_BUTTON_HEIGHT = 29;
  {$IFDEF ANDROID}
  C_DEFAULT_ACTIVE_SWITCH_COLOR = claDodgerBlue;
  C_SEGMENT_BUTTON_HEIGHT = 30;
  {$ELSE}
  C_DEFAULT_ACTIVE_SWITCH_COLOR = claLimegreen;
  C_SEGMENT_BUTTON_HEIGHT = 30;
  {$ENDIF}
  C_SWIPE_DISTANCE = 60;

  C_DEFAULT_TEXT_COLOR = claBlack;
  C_DEFAULT_HEADER_TEXT_COLOR = claBlack;
  C_DEFAULT_SEGMENT_BUTTON_COLOR = claNull;

  C_TITLE    = 'TITLE';
  C_SUBTITLE = 'SUBTITLE';
  C_DETAIL   = 'DETAIL';

  C_DEFAULT_DELETE_BUTTON_TEXT = 'Delete';
  C_DEFAULT_ACTION_BUTTON_WIDTH = 64;
  C_DEFAULT_PAGE_SIZE = 100;
  C_DEFAULT_ACTION_BUTTON_TEXT_COLOR = claWhite;

  C_DEFAULT_SELECTED_COLOR = claGainsboro;

  C_LEFT_MARGIN = 10;
  C_DEFAULT_SEPARATOR_COLOR = $FFE0E0E0;
  C_PLATFORM_ACCESSORY_COLOR = claSilver;

  C_ACTION_BTN_ANIMATION_SPEED = 0.2;

type
  TksListView = class;
  TKsListItemRow = class;
  TKsListItemRows = class;
  TksListItemRowObj = class;
  TksListItemRowSwitch = class;
  TksListItemRowButton = class;
  TksListItemRowSegmentButtons = class;
  TksListItemRowActionButton = class;
  TksListItemRowActionButtons = class;
  TksListItemRowTable = class;

  TksListViewCheckMarks = (ksCmNone, ksCmSingleSelect, ksCmMultiSelect);
  TksListViewCheckStyle = (ksCmsDefault, ksCmsRed, ksCmsGreen, ksCmsBlue);
  TksListViewShape = (ksRectangle, ksRoundRect, ksEllipse);
  TksItemImageShape = (ksRectangleImage, ksRoundRectImage, ksCircleImage);


  TksAccessoryType = (atNone, atMore, atCheckmark, atDetail, atBack, atRefresh,
                      atAction, atPlay, atRewind, atForward, atPause, atStop,
                      atAdd, atPrior, atNext, atArrowUp, atArrowDown, atArrowLeft,
                      atArrowRight, atReply, atSearch, atBookmarks, atTrash, atOrganize,
                      atCamera, atCompose, atInfo, atPagecurl, atDetails,
                      atRadioButton, atRadioButtonChecked,
                      atCheckBox, atCheckBoxChecked,
                      atUserDefined1, atUserDefined2, atUserDefined3);

  TksImageButtonStyle = TksAccessoryType;
  {(Action, Add, Camara, Compose, Information, ArrowLeft,
    ArrowDown, ArrowRight, ArrowUp, Delete, Details, Organise, PageCurl, Pause,
    Play, Refresh, Reply, Search, Stop, Trash);  }

  TksButtonState = (Pressed, Unpressed);
  TksListItemRowSelector = (NoSelector, DateSelector, ItemPicker);
  TksScrollDirection = (sdUp, sdDown);
  TksItemSwipeDirection = (sdLeftToRight, sdRightToLeft);
  TksActionButtonState = (ksActionBtnVisible, ksActionBtnHidden, ksActionBtnAnimIn, ksActopmBtnAnimOut);

  {TksButtonStyle = (ksButtonDefault, ksButtonSegmentLeft, ksButtonSegmentMiddle, ksButtonSegmentRight);

  TksToolAccessory = ( taBack, taRefresh, taAction, taPlay, taRewind, taForward, taPause, taStop, taAdd,
                       taPrior, taNext, taArrowUp, taArrowDown, taArrowLeft, taArrowRight, taReply, taSearch,
                       taBookmarks, taTrash, taOrganize, taCamera, taCompose, taInfo, taPagecurl, taDetail );
                     }
  TksMouseEventType = (ksMouseItemClick, ksMouseItemRightClick, ksMouseDown, ksMouseMove, ksMouseUp, ksMouseLongPress);



  {$IFDEF XE10_OR_NEWER}
  TksListViewItems = TAppearanceListViewItems;
  {$ELSE}
  TksListViewItems = TListViewItems;
  {$ENDIF}
  TksListViewRowClickEvent = procedure(Sender: TObject; x, y: single; AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj) of object;
  TksListViewClickSwitchEvent = procedure(Sender: TObject; AItem: TKsListItemRow; ASwitch: TksListItemRowSwitch; ARowID: string) of object;
  TksListViewClickButtonEvent = procedure(Sender: TObject; AItem: TKsListItemRow; AButton: TksListItemRowButton; ARowID: string) of object;
  TksListViewClickSegmentButtonEvent = procedure(Sender: TObject; AItem: TKsListItemRow; AButtons: TksListItemRowSegmentButtons; ARowID: string) of object;
  TksListViewFinishScrollingEvent = procedure(Sender: TObject; ATopIndex, AVisibleItems: integer) of object;
  TksListViewSelectDateEvent = procedure(Sender: TObject; AItem: TksListItemRow; ASelectedDate: TDateTime; var AAllow: Boolean) of object;
  TksListViewSelectPickerItem = procedure(Sender: TObject; AItem: TksListItemRow; ASelected: string; var AAllow: Boolean) of object;
  TksDeleteItemEvent = procedure(Sender: TObject; AIndex: Integer) of object;
  TksItemSwipeEvent = procedure(Sender: TObject; ARow: TksListItemRow; ASwipeDirection: TksItemSwipeDirection; AButtons: TksListItemRowActionButtons) of object;
  TksItemActionButtonClickEvent = procedure(Sender: TObject; ARow: TksListItemRow; AButton: TksListItemRowActionButton) of object;
  TksSearchFilterChange = procedure(Sender: TObject; ASearchText: string) of object;
  TksRowCacheEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARow: TksListItemRow; ARect: TRectF) of object;
  TksOptionSelectionEvent = procedure(Sender: TObject; ARow: TksListItemRow; AItemIndex: integer; ASelected: string) of object;
  TksEmbeddedEditChange = procedure(Sender: TObject; ARow: TksListItemRow; AText: string) of object;
  TksEmbeddedListBoxChange = procedure(Sender: TObject; ARow: TksListItemRow; AItemIndex: integer; ASelected: string) of object;
  TksEmbeddedSpinBoxChange = procedure(Sender: TObject; ARow: TksListItemRow; AValue: integer) of object;
  TksEmbeddedTrackBarChange = procedure(Sender: TObject; ARow: TksListItemRow; AValue: integer) of object;
  //TksEmbeddedButtonClick = procedure(Sender: TObject; ARow: TksListItemRow; AValue: integer) of object;

  // ------------------------------------------------------------------------------

  TksVisibleItems = record
    Count: integer;
    IndexStart: integer;
    IndexEnd: integer;
  end;

  TksMouseEvent = record
    EventType: TksMouseEventType;
    X, Y: single;
    ID: string;
    Row: TksListItemRow;
    RowObj: TksListItemRowObj;
  end;

  TksMouseEventList = TList<TksMouseEvent>;

  TksAccessoryImage = class(TBitmap)
  public
    procedure SetBitmap(ASource: TBitmap);
    procedure DrawToCanvas(ACanvas: TCanvas; ADestRect: TRectF);
  end;

  TksAccessoryImageList = class(TObjectList<TksAccessoryImage>)
  private
    FImageMap: TBitmap;
    function GetAccessoryFromResource(AStyleName: string; const AState: string = ''): TksAccessoryImage;
    procedure Initialize;
    function GetAccessoryImage(AAccessory: TksAccessoryType): TksAccessoryImage;
  public
    constructor Create;
    destructor Destroy; override;
    property Images[AAccessory: TksAccessoryType]: TksAccessoryImage read GetAccessoryImage; default;
    property ImageMap: TBitmap read FImageMap;
  end;




  TksListItemRowObj = class(TPersistent)
  strict private
    FRect: TRectF;
  private
    FId: string;
    FPlaceOffset: TPointF;
    [weak]FRow: TKsListItemRow;
    FAlign: TListItemAlign;
    FVertAlignment: TListItemAlign;
    FTagBoolean: Boolean;
    FGuid: string;
    FWidth: single;
    FHeight: single;
    FConumesRowClick: Boolean;
    FHitTest: Boolean;
    procedure SetRect(const Value: TRectF);
    procedure SetID(const Value: string);
    procedure Changed;
    procedure SetAlign(const Value: TListItemAlign);
    procedure SetVertAlign(const Value: TListItemAlign);
    function GetOffsetX: single;
    function GetOffsetY: single;
    procedure SetOffsetX(const Value: single);
    procedure SetOffsetY(const Value: single);
    procedure SetHeight(const Value: single);
    procedure SetWidth(const Value: single);
    procedure ProcessClick(x, y: single); //virtual;
  protected
    function GetConsumesRowClick: Boolean; virtual;
    function GetRowRect: TRectF;
    procedure CalculateRect(ARowBmp: TBitmap); virtual;
    procedure DoChanged(Sender: TObject);
    procedure ChangeSize; virtual;
    procedure DoClick(x, y: single); virtual;
  public
    constructor Create(ARow: TKsListItemRow); virtual;
    procedure Assign(ASource: TPersistent); override;
    function Render(ACanvas: TCanvas): Boolean; virtual;
    procedure MouseDown; virtual;
    procedure MouseUp; virtual;
    property Rect: TRectF read FRect write SetRect;
    property ID: string read FId write SetID;
    property Align: TListItemAlign read FAlign write SetAlign default TListItemAlign.Leading;
    property VertAlign: TListItemAlign read FVertAlignment write SetVertAlign default TListItemAlign.Center;
    property PlaceOffset: TPointF read FPlaceOffset write FPlaceOffset;
    property TagBoolean: Boolean read FTagBoolean write FTagBoolean;
    property Width: single read FWidth write SetWidth;
    property Height: single read FHeight write SetHeight;
    property OffsetY: single read GetOffsetY write SetOffsetY;
    property OffsetX: single read GetOffsetX write SetOffsetX;
    property ConsumesRowClick: Boolean read FConumesRowClick default False;
    property HitTest: Boolean read FHitTest write FHitTest;
  end;




  TksListItemRowText = class(TksListItemRowObj)
  private
    FFont: TFont;
    FAlignment: TTextAlign;
    FTextColor: TAlphaColor;
    FBackground: TAlphaColor;
    FText: string;
    FWordWrap: Boolean;
    FFullWidth: Boolean;
    FTextLayout: TTextAlign;
    FIsHtmlText: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetAlignment(const Value: TTextAlign);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetText(const Value: string);
    procedure SetWordWrap(const Value: Boolean);
    function CalculateTextHeight: single;
    procedure SetTextLayout(const Value: TTextAlign);
    procedure SetBackground(const Value: TAlphaColor);
    procedure SetIsHtmlText(const Value: Boolean);
  protected
    //procedure RecalculateSize(AWidth, AHeight: single);
    procedure CalculateRect(ARowBmp: TBitmap); override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property Font: TFont read FFont write SetFont;
    property TextAlignment: TTextAlign read FAlignment write SetAlignment;
    property TextLayout: TTextAlign read FTextLayout write SetTextLayout;
    property Background: TAlphaColor read FBackground write SetBackground default claNull;
    property TextColor: TAlphaColor read FTextColor write SetTextColor;
    property Text: string read FText write SetText;
    property IsHtmlText: Boolean read FIsHtmlText write SetIsHtmlText;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  end;

  // ------------------------------------------------------------------------------

  TksListItemBrush = class(TPersistent)
  private
    FColor: TAlphaColor;
    FKind: TBrushKind;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetKind(const Value: TBrushKind);
  public
    constructor Create; virtual;
    procedure Assign(ASource: TPersistent); override;
    property Color: TAlphaColor read FColor write SetColor;
    property Kind: TBrushKind read FKind write SetKind;
  end;

  TksListItemStroke = class(TPersistent)
  private
    FColor: TAlphaColor;
    FKind: TBrushKind;
    FThickness: single;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetKind(const Value: TBrushKind);
    procedure SetThickness(const Value: single);
  public
    constructor Create; virtual;
    procedure Assign(ASource: TPersistent); override;
    property Color: TAlphaColor read FColor write SetColor;
    property Kind: TBrushKind read FKind write SetKind;
    property Thickness: single read FThickness write SetThickness;
  end;

  TksListItemOptionSelector = class(TksListItemRowObj)
  private
    FItems: TStrings;
    FItemHeight: single;
    FItemIndex: integer;
    procedure SetItemHeight(const Value: single);
    procedure SetItemIndex(const Value: integer);
  protected
    function GetConsumesRowClick: Boolean; override;
    procedure DoClick(x, y: single); override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
  end;

  TksListItemEmbeddedControl = class(TksListItemRowObj)
  private
    procedure SimulateClick(x, y: single);
  protected
    FControl: TStyledControl;
    FFocused: Boolean;
    FCached: TBitmap;
    function GetControlBitmap: Boolean;
    procedure ChangeSize; override;
    function CreateControl: TStyledControl; virtual; abstract;
    function GetConsumesRowClick: Boolean; override;
    procedure InitializeControl; virtual;
    procedure ShowControl;
    procedure HideControl;
    procedure DoClick(x, y: single); override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    function Render(ACanvas: TCanvas): Boolean; override;

  end;

  TksListItemEmbeddedEdit = class(TksListItemEmbeddedControl)
  private
    function GetEdit: TEdit;
    procedure DoTyping(Sender: TObject);
  protected
    function CreateControl: TStyledControl; override;
  end;

  TksListItemEmbeddedMemo = class(TksListItemEmbeddedControl)
  private
    function GetMemo: TMemo;
    procedure DoTyping(Sender: TObject);
  protected
    function CreateControl: TStyledControl; override;
  end;

  TksListItemEmbeddedListBox = class(TksListItemEmbeddedControl)
  private
    function GetListBox: TListView;
    procedure DoSelect(const Sender: TObject; const AItem: TListViewItem);
  protected
    function CreateControl: TStyledControl; override;
  end;

  {TksListItemEmbeddedButton = class(TksListItemEmbeddedControl)
  private
    function GetButton: TButton;
    procedure DoClick(
  protected
    function CreateControl: TStyledControl; override;
  public
    property Button: TButton read GetButton;
  end;      }

  TksListItemEmbeddedSpinBox = class(TksListItemEmbeddedControl)
  private
    function GetSpinBox: TSpinBox;
    procedure DoChange(Sender: TObject);
  protected
    function CreateControl: TStyledControl; override;
  public
    property SpinBox: TSpinBox read GetSpinBox;
  end;

  TksListItemEmbeddedTrackBar = class(TksListItemEmbeddedControl)
  private
    function GetTrackBar: TTrackBar;
    procedure DoChange(Sender: TObject);
  protected
    function CreateControl: TStyledControl; override;
  public
    property TrackBar: TTrackBar read GetTrackBar;
  end;

  TksListItemTableShadow = class(TPersistent)
  private
    FColor: TAlphaColor;
    FOffset: integer ;
    FVisible: Boolean;
  public
    constructor Create; virtual;
    procedure SetVisible(const Value: Boolean);
    property Color: TAlphaColor read FColor write FColor default claSilver;
    property Offset: integer read FOffset write FOffset default 3;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TksListItemRowTableCell = class(TPersistent)
  private
    [weak]FTable: TksListItemRowTable;
    FRow, FCol: integer;
    FTextSettings: TTextSettings;
    FFill: TksListItemBrush;
    FStroke: TksListItemStroke;
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
    function GetShadow: TksListItemTableShadow;
  public
    constructor Create(ATable: TksListItemRowTable); virtual;
    destructor Destroy; override;
    function IsFixedCell: Boolean;
    procedure DrawToCanvas(x, y: single; ACanvas: TCanvas; ACol,
      ARow: integer; AShadow: TksListItemTableShadow; AText: Boolean);
    property Fill: TksListItemBrush read FFill;
    property Stroke: TksListItemStroke read FStroke;
    property TextSettings: TTextSettings read FTextSettings write SetTextSettings;
    property Text: string read FText write SetText;
    property Width: single read FWidth write FWidth;
    property Height: single read FHeight write FHeight;
    property Padding: TBounds read FPadding write FPadding;
    property Shadow: TksListItemTableShadow read GetShadow;
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

  TksListItemRowTable = class(TksListItemRowObj)
  private
    FBackground: TAlphaColor;
    FBorderColor: TAlphaColor;
    FRows: array of TksListItemRowTableRow;
    FRowCount: integer;
    FColCount: integer;
    FDefaultRowHeight: single;
    FDefaultColWidth: single;
    FShadow: TksListItemTableShadow;
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
    procedure CalculateRect(ARowBmp: TBitmap); override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    procedure Clear;
    function Render(ACanvas: TCanvas): Boolean; override;
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
    property Shadow: TksListItemTableShadow read FShadow;
    property TableSize: TSizeF read GetTableSize;
  end;

  TksListItemRowImage = class(TksListItemRowObj)
  private
    FBitmap: TBitmap;
    FImageShape: TksItemImageShape;
    FBorder: TksListItemStroke;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetImageShape(const Value: TksItemImageShape);
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Border: TksListItemStroke read FBorder;
    property ImageShape: TksItemImageShape read FImageShape write SetImageShape default ksRectangleImage;
  end;



  TksListItemRowShape = class(TksListItemRowObj)
  private
    FStroke: TksListItemStroke;
    FFill: TksListItemBrush;
    FShape: TksListViewShape;
    FCornerRadius: single;
    procedure SetCornerRadius(const Value: single);
    procedure SetShape(const Value: TksListViewShape);
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property Stroke: TksListItemStroke read FStroke;
    property Fill: TksListItemBrush read FFill;
    property CornerRadius: single read FCornerRadius write SetCornerRadius;
    property Shape: TksListViewShape read FShape write SetShape;
  end;

  TKsListItemRowAccessory = class(TksListItemRowObj)
  private
    FAccessoryType: TksAccessoryType;
    procedure SetAccessoryType(const Value: TksAccessoryType);
  protected
    procedure CalculateRect(ARowBmp: TBitmap); override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property AccessoryType: TksAccessoryType read FAccessoryType write SetAccessoryType;
  end;

  TksListItemRowSwitch = class(TksListItemRowObj)
  private
    FIsChecked: Boolean;
    FActiveColor: TAlphaColor;
    procedure SetIsChecked(const Value: Boolean);
    procedure SetActiveColor(const Value: TAlphaColor);
  protected
    function GetConsumesRowClick: Boolean; override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    procedure DoClick(x, y: single); override;
    property IsChecked: Boolean read FIsChecked write SetIsChecked;
    property ActiveColor: TAlphaColor read FActiveColor write SetActiveColor default C_DEFAULT_ACTIVE_SWITCH_COLOR;

  end;

  // ------------------------------------------------------------------------------

  TksListItemRowButton = class(TksListItemRowObj)
  private
    FTintColor: TAlphaColor;
    FText: string;
    FStyleLookup: string;
    FState: TksButtonState;
    procedure SetTintColor(const Value: TAlphaColor);
    procedure SetText(const Value: string);
    procedure SetStyleLookup(const Value: string);
  protected
    function GetConsumesRowClick: Boolean; override;
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
  protected
    function GetConsumesRowClick: Boolean; override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    procedure DoClick(x, y: single); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Captions: TStrings read FCaptions;
    property TintColor: TAlphaColor read FTintColor write SetTintColor;
  end;

  // ------------------------------------------------------------------------------

  TksListItemRowProgressBar = class(TksListItemRowObj)
  private
    FBarColor: TAlphaColor;
    FBackgroundColor: TAlphaColor;
    FBorderColor: TAlphaColor;
    FCornerRadius: single;
    FProgressPercent: integer;
    procedure SetBackgroundColorColor(const Value: TAlphaColor);
    procedure SetBarColor(const Value: TAlphaColor);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetProgressPercent(const Value: integer);
    procedure SetCornerRadius(const Value: single);
  public
    constructor Create(ARow: TKsListItemRow); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property BarColor: TAlphaColor read FBarColor write SetBarColor;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColorColor;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor;
    property CornerRadius: single read FCornerRadius write SetCornerRadius;
    property ProgressPercent: integer read FProgressPercent write SetProgressPercent;
  end;

  // ------------------------------------------------------------------------------

  TKsSegmentButtonPosition = (ksSegmentLeft, ksSegmentMiddle, ksSegmentRight);

  TksListItemObjects = class(TObjectList<TksListItemRowObj>)
  public
    function ObjectByID(AId: string): TksListItemRowObj;
  end;

  TksActionButtonType = (btCustom, btDelete);
  TKsListItemRowActionButton = class
  private
    FId: string;
    [weak]FOwner: TksListItemRowActionButtons;
    FBackground: TRectangle;
    FLabel: TLabel;
    FRow: TksListItemRow;
    FButtonType: TksActionButtonType;
    FVisible: Boolean;
    FWidth: integer;
    FOffscreenXPos: single;
    FOnScreenXPos: single;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure DoClick(Sender: TObject);
    procedure SetVisible(const Value: Boolean);
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
  public
    constructor Create(AOwner: TksListItemRowActionButtons; AID: string);
    destructor Destroy; override;
    procedure AddToObject(AObject: TFmxObject);
    property Color: TAlphaColor read GetColor write SetColor;
    property Text: string read GetText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property ID: string read FID;
    property ButtonType: TksActionButtonType read FButtonType;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: integer read FWidth write FWidth default C_DEFAULT_ACTION_BUTTON_WIDTH;
  end;

  TksListItemRowActionButtons = class(TObjectList<TksListItemRowActionButton>)
  private
    [weak]FTimerService: IFMXTimerService;
    [weak]FListView: TksListView;
    [weak]FRow: TksListItemRow;
    FSwipeDirection: TksItemSwipeDirection;
    FState: TksActionButtonState;
    FShowTimer: TFmxHandle;
    FHideTimer: TFmxHandle;
    function GetVisibleXPos(ABtn: TksListItemRowActionButton; ARowRect: TRectF; ASwipeDirection: TksItemSwipeDirection): single;
    function InsertButton(AIndex: integer; AText: string; AColor, ATextColor: TAlphaColor; const AButtonID: string = ''): TksListItemRowActionButton;
    procedure AddDeleteButton;

    function GetIsAnimating: Boolean;
    procedure DoAfterShow;
    procedure DoAfterHide;
  public
    constructor Create(AOwner: TksListView);
    function AddButton(AText: string; AColor, ATextColor: TAlphaColor; const AButtonID: string = ''): TksListItemRowActionButton;
    procedure InitializeActionButtons(ARow: TKsListItemRow; ASwipeDirection: TksItemSwipeDirection);
    procedure Show;
    procedure Hide(AAnimate: Boolean);
    property State: TksActionButtonState read FState;
    property IsAnimating: Boolean read GetIsAnimating;
  end;


  TksListItemRow = class(TListItemImage)
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
    FList: TksListItemObjects;
    FId: string;
    FShowAccessory: Boolean;
    FAutoCheck: Boolean;
    FImageIndex: integer;
    FCanSelect: Boolean;
    FChecked: Boolean;
    FIndex: integer;
    FSelector: TksListItemRowSelector;
    FSelectionValue: Variant;
    FPickerItems: TStrings;
    FRowHeight: integer;
    FLastHeight: single;
    FBackgroundColor: TAlphaColor;
    FUpdating: Boolean;
    //FHasCustomAccessory: Boolean;
    //FCustomAccessory: TksToolAccessory;
    function RowHeight(const AScale: Boolean = True): single;
    function RowWidth(const AScale: Boolean = True): single;
    function GetListView: TksListView;
    function GetRowObject(AIndex: integer): TksListItemRowObj;
    function GetRowObjectCount: integer;
    procedure SetAccessory(const Value: TksAccessoryType);
    procedure SetShowAccessory(const Value: Boolean);
    function GetAccessory: TksAccessoryType;
    procedure SetAutoCheck(const Value: Boolean);
    procedure SetImageIndex(const Value: integer);
    function GetSearchIndex: string;
    procedure SetSearchIndex(const Value: string);
    procedure SetIndicatorColor(const Value: TAlphaColor);
    procedure SetCanSelect(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    function GetPurpose: TListItemPurpose;
    procedure SetPurpose(const Value: TListItemPurpose);
    function GetSearchText: string;
    procedure SetSearchText(const Value: string);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetPickerItems(const Value: TStrings);
    function GetData(const AIndex: string): TValue;
    function GetHasData(const AIndex: string): Boolean;
    procedure SetData(const AIndex: string; const Value: TValue);
    function GetRowObjectByID(AId: string): TksListItemRowObj;
    property ListView: TksListView read GetListView;
    procedure DoOnListChanged(Sender: TObject; const Item: TksListItemRowObj; Action: TCollectionNotification);
    function ScreenWidth: single;
    procedure ProcessClick;
    procedure Changed;
    procedure ReleaseAllDownButtons;
    procedure PickerItemsChanged(Sender: TObject);
  protected
    procedure DoResize; override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;

    procedure Assign(Source: TPersistent); override;
    procedure CacheRow;
    procedure ReleaseRow;
    procedure SetRowFontStyle(AFontStyle: TFontStyles);
    procedure SetRowTextColor(AColor: TAlphaColor);


    {$IFDEF VER290}
    function LocalRect: TRectF;
    {$ENDIF}

    // bitmap functions...

    function DrawAccessory(AAccessory: TksAccessoryType; x, y: single): TksListItemRowImage;
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

    // progress bar
    function DrawProgressBar(x, y, AWidth, AHeight: single; APercent: integer;
                             ABarColor: TAlphaColor;
                             ACornerRadius: single;
                             const AAlign: TListItemAlign = TListItemAlign.Trailing;
                             const ABackgroundColor: TAlphaColor = claWhite;
                             const ABorderColor: TAlphaColor = claBlack ): TksListItemRowProgressBar;


    // switch
    function AddSwitch(x: single; AIsChecked: Boolean; const AAlign: TListItemAlign = TListItemAlign.
    Trailing): TksListItemRowSwitch;
    function AddSwitchRight(AMargin: integer; AIsChecked: Boolean): TksListItemRowSwitch;
    // buttons...
    function AddButton(AWidth: integer; AText: string;
                       const ATintColor: TAlphaColor = claNull;
                       const AVertAlign: TListItemAlign = TListItemAlign.Center;
                       const AYPos: integer = 0): TksListItemRowButton; overload;
    function AddButton(AStyle: TksImageButtonStyle; const ATintColor: TAlphaColor = claNull): TksListItemRowButton; overload;
    function AddSegmentButtons(AWidth: integer;
                               ACaptions: array of string;
                               const AItemIndex: integer = 0): TksListItemRowSegmentButtons; overload;
    function AddSegmentButtons(AXPos, AWidth: integer;
                               ACaptions: array of string;
                               AAlign: TListItemAlign;
                               const AItemIndex: integer = 0): TksListItemRowSegmentButtons; overload;

    function AddTable(AX, AY, AColWidth, ARowHeight: single; AColCount, ARowCount: integer): TksListItemRowTable;

    function AddEdit(AX, AY, AWidth: single; AText: string): TksListItemEmbeddedEdit;
    function AddMemo(AX, AY, AWidth, AHeight: single; AText: string): TksListItemEmbeddedMemo;
    function AddSpinBox(AX, AY, AWidth: single; AValue: integer): TksListItemEmbeddedSpinBox;
    function AddTrackBar(AX, AY, AWidth: single): TksListItemEmbeddedTrackBar;
    function AddOptionSelectiontBox(AX, AY, AWidth, AHeight, AItemHeight: single; AItems: TStrings): TksListItemOptionSelector; overload;
    function AddOptionSelectiontBox(AX, AY, AWidth, AHeight, AItemHeight: single; AItems: array of string): TksListItemOptionSelector; overload;
    function AddListBox(AX, AY, AWidth, AHeight, AItemHeight: single; AItems: array of string): TksListItemEmbeddedListBox; overload;
    function AddListBox(AX, AY, AWidth, AHeight, AItemHeight: single; AItems: TStrings): TksListItemEmbeddedListBox; overload;
    //function AddEmbeddedButton(AX, AY, AWidth: single; AText: string): TksListItemEmbeddedButton;

    // text functions...
    function TextOut(AText: string; x: single; const AVertAlign: TListItemAlign = TListItemAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, AWidth: single; const AVertAlign: TListItemAlign = TListItemAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, y, AWidth: single; const AVertAlign: TListItemAlign = TListItemAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextBox(AText: string; ARect: TRectF; ATextAlign: TTextAlign; ATextLayout: TTextAlign; const ABackground: TAlphaColor = claNull): TksListItemRowText; overload;
    function TextBoxHtml(AText: string; ARect: TRectF): TksListItemRowText;
    function TextOutRight(AText: string; y, AWidth: single; AXOffset: single; const AVertAlign: TListItemAlign = TListItemAlign.Center): TksListItemRowText; overload;

    function TextWidth(AText: string; AIsHtml: Boolean): single;
    function TextHeight(AText: string; AWordWrap, AIsHtml: Boolean; const AMaxWidth: single): single;
    // font functions...
    procedure SetFontProperties(AName: string; ASize: integer; AColor: TAlphaColor; AStyle: TFontStyles);
    // properties...
    property Checked: Boolean read FChecked write SetChecked;
    property Title: TksListItemRowText read FTitle;
    property SubTitle: TksListItemRowText read FSubTitle;
    property Detail: TksListItemRowText read FDetail;
    property Font: TFont read FFont;
    property TextColor: TAlphaColor read FTextColor write FTextColor;
    property RowObject[AIndex: integer]: TksListItemRowObj read GetRowObject;
    property RowObjectCount: integer read GetRowObjectCount;
    property ID: string read FId write FId;
    property Index: integer read FIndex write FIndex;
    property Cached: Boolean read FCached write FCached;
    property IndicatorColor: TAlphaColor read FIndicatorColor write SetIndicatorColor;
    property Accessory: TksAccessoryType read GetAccessory write SetAccessory;
    property ShowAccessory: Boolean read FShowAccessory write SetShowAccessory default True;
    property AutoCheck: Boolean read FAutoCheck write SetAutoCheck default False;
    property Image: TksListItemRowImage read FImage write FImage;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property SearchIndex: string read GetSearchIndex write SetSearchIndex;
    property CanSelect: Boolean read FCanSelect write SetCanSelect default True;
    property Purpose: TListItemPurpose read GetPurpose write SetPurpose;
    property Selector: TksListItemRowSelector read FSelector write FSelector;
    property SearchText: string read GetSearchText write SetSearchText;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default claNull;
    property PickerItems: TStrings read FPickerItems write SetPickerItems;
    property Data[const AIndex: string]: TValue read GetData write SetData;
    property HasData[const AIndex: string]: Boolean read GetHasData;
    property RowObjectByID[AId: string]: TksListItemRowObj read GetRowObjectByID;
  end;


  TKsListItemRows = class
  private
    [weak]FListView: TksListView;
    {$IFDEF XE10_OR_NEWER}
    [weak]FListViewItems: TAppearanceListViewItems;
    {$ELSE}
    [weak]FListViewItems: TListViewItems;
    {$ENDIF}
    function GetCheckedCount: integer;
    function GetCount: integer;
    function GetItems(index: integer): TKsListItemRow;
    procedure ReindexRows;
    function KsRowFromRow(AIndex: integer): TKsListItemRow;
  public
    constructor Create(AListView: TksListView; AItems: TksListViewItems) ; virtual;

    function AddRow(AText: string; const AAccessoryType: TksAccessoryType = atNone): TKsListItemRow; overload;
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
    function AddRowDateSelector(AText: string; ADate: TDateTime): TKsListItemRow;
    function AddRowItemSelector(AText, ASelected: string; AItems: TStrings): TKsListItemRow; overload;
    function AddRowItemSelector(AText, ASelected: string; AItems: array of string): TKsListItemRow; overload;
    function AddHeader(AText: string): TKsListItemRow;

    procedure UncheckAll;
    procedure CheckAll;
    procedure Clear;
    procedure Delete(index: integer);
    procedure DeleteSelected;
    procedure DeleteFirst;
    procedure DeleteLast;
    property CheckedCount: integer read GetCheckedCount;
    property Count: integer read GetCount;
    property Items[index: integer]: TKsListItemRow read GetItems; default;
  end;

  // ------------------------------------------------------------------------------

  TksListViewAppearence = class(TPersistent)
  private
    [weak]FListView: TksListView;
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
    constructor Create(AListView: TksListView);
  published
    property Background: TAlphaColor read FBackground write SetBackground default claWhite;
    property HeaderColor: TAlphaColor read FHeaderColor write SetHeaderColor default claNull;
    property SeparatorColor: TAlphaColor read FSeparatorColor write SetSeparatorBackground default claNull;
    property ItemBackground: TAlphaColor read FItemBackground write SetItemBackground default claWhite;
    property SelectedColor: TAlphaColor read FSelectedColor write SetSelectedColor default claNull;
    property AlternatingItemBackground: TAlphaColor read FAlternatingItemBackground write SetAlternatingItemBackground default claGainsboro;
  end;

  TksPageCaching = class(TPersistent)
  private
    FEnabled: Boolean;
    FPageSize: integer;
  published
    constructor Create;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property PageSize: integer read FPageSize write FPageSize default C_DEFAULT_PAGE_SIZE;
  end;

  TksDeleteButton = class(TPersistent)
  private
    FEnabled: Boolean;
    FText: string;
    FColor: TAlphaColor;
    FTextSettings: TTextSettings;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Color: TAlphaColor read FColor write FColor default claRed;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Text: string read FText write FText;
    property TextSettings: TTextSettings read FTextSettings write FTextSettings;
  end;

  // ------------------------------------------------------------------------------

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidiOSDevice or pidAndroid)]


  TksListView = class(TCustomListView)
  private
    FScreenScale: single;
    FAppearence: TksListViewAppearence;
    FOnItemClick: TksListViewRowClickEvent;
    FOnItemRightClick: TksListViewRowClickEvent;
    FMouseDownPos: TPointF;
    FCurrentMousepos: TPointF;
    FItemHeight: integer;
    FLastWidth: integer;
    FOnLongClick: TksListViewRowClickEvent;
    FClickedRowObj: TksListItemRowObj;
    FSelectOnRightClick: Boolean;
    FOnSwitchClicked: TksListViewClickSwitchEvent;
    FOnButtonClicked: TksListViewClickButtonEvent;
    FOnSegmentButtonClicked: TksListViewClickSegmentButtonEvent;
    FScrollTimer: TFmxHandle;
    FLastScrollPos: single;
    FLastPaintPos: single;
    FScrolling: Boolean;
    FOnFinishScrolling: TksListViewFinishScrollingEvent;
    FCheckMarks: TksListViewCheckMarks;
    FCheckMarkStyle: TksListViewCheckStyle;
    FUpdateCount: integer;
    FItemImageSize: integer;
    FShowIndicatorColors: Boolean;
    FIsShowing: Boolean;
    FItems: TKsListItemRows;
    FCombo: TComboBox;
    FDateSelector: TDateEdit;
    FOnSelectDate: TksListViewSelectDateEvent;
    FOnSelectPickerItem: TksListViewSelectPickerItem;
    FKeepSelection: Boolean;
    FMouseDownTime: TDateTime;
    FHeaderHeight: integer;
    FScrollDirection: TksScrollDirection;
    FLastRenderedIndex: integer;
    FLoadingBitmap: TBitmap;
    FOnDeleteItem: TksDeleteItemEvent;
    FLastIndex: integer;
    FWidth: single;
    FOnScrollLastItem: TNotifyEvent;
    FOnItemSwipe: TksItemSwipeEvent;
    FActionButtons: TksListItemRowActionButtons;
    FOnItemActionButtonClick: TksItemActionButtonClickEvent;
    FPageCaching: TksPageCaching;
    FFullWidthSeparator: Boolean;
    FDeleteButton: TksDeleteButton;
    FSearchEdit: TSearchBox;
    FOnSearchFilterChanged: TksSearchFilterChange;
    FShowSelection: Boolean;
    FMouseEvents: TksMouseEventList;
    FTimerService: IFMXTimerService;
    FDeselectTimer: TFmxHandle;
    //FOnShowTimer: TFmxHandle;
    {$IFDEF XE10_OR_NEWER}
    FMouseEventsTimer: TFmxHandle;
    {$ENDIF}
    FProcessingMouseEvents: Boolean;
    FBeforeRowCache: TksRowCacheEvent;
    FAfterRowCache: TksRowCacheEvent;

    FActiveEmbeddedControl: TksListItemEmbeddedControl;

    FOnOptionSelection: TksOptionSelectionEvent;
    FOnEmbeddedEditChange: TksEmbeddedEditChange;
    FOnEmbeddedListBoxChange: TksEmbeddedListBoxChange;
    FOnEmbeddedSpinBoxChange: TksEmbeddedSpinBoxChange;
    FOnEmbeddedTrackBarChange: TksEmbeddedTrackBarChange;
    function GetSearchBoxHeight: single;
    function _Items: TksListViewItems;
    procedure DoScrollTimer;
    procedure SetCheckMarks(const Value: TksListViewCheckMarks);
    function RowObjectAtPoint(ARow: TKsListItemRow; x, y: single): TksListItemRowObj;
    procedure ReleaseAllDownButtons;
    procedure SetCheckMarkStyle(const Value: TksListViewCheckStyle);
    procedure SetItemImageSize(const Value: integer);
    procedure SetShowIndicatorColors(const Value: Boolean);
    function AddItem: TListViewItem;
    procedure SelectDate(ARow: TKsListItemRow; ASelected: TDAteTime; AOnSelectDate: TNotifyEvent);
    procedure SelectItem(ARow: TKsListItemRow; AItems: TStrings; ASelected: string; AOnSelectItem: TNotifyEvent);
    procedure DoSelectDate(Sender: TObject);
    procedure DoSelectPickerItem(Sender: TObject);
    procedure ComboClosePopup(Sender: TObject);
    procedure DoOnDeleteItem(Sender: TObject; AIndex: Integer);
    procedure DoRenderRow(ARow: TKsListItemRow);
    //procedure CacheEmbeddedControls;
    procedure CachePages;
    function LoadingBitmap: TBitmap;
    procedure DeselectRow(const ADelay: integer = 0);
    procedure DoSearchFilterChanged(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure SetShowSelection(const Value: Boolean);
    procedure QueueMouseEvent(AType: TksMouseEventType; X, Y: single; AId: string; ARow: TKsListItemRow; AObj: TksListItemRowObj);
    procedure ProcessMouseEvents;
    procedure DoDeselectRow;
    procedure SetupSearchBox;
    //procedure EmbeddedEditChange(ARow: TKsListItemRow; AText: string);
    //procedure EmbeddedListBoxChange(ARow: TKsListItemRow; AItemIndex: integer; ASelected: string);
    procedure StartScrollTimer;
    procedure EmbeddedEditChange(ARow: TKsListItemRow; AText: string);
    procedure EmbeddedListBoxChange(ARow: TKsListItemRow; AItemIndex: integer;
      ASelected: string);
    procedure EmbeddedSpinBoxChange(ARow: TKsListItemRow; AValue: integer);
    procedure EmbeddedTrackBarChange(ARow: TKsListItemRow; AValue: integer);
    procedure RowOptionSelected(ARow: TKsListItemRow; AIndex: integer; ASelected: string);

    //procedure DoAfterShow;
    { Private declarations }
  protected
    procedure SetColorStyle(AName: string; AColor: TAlphaColor);
    procedure ApplyStyle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    {procedure ShowRowActionButtons(ARow: TKsListItemRow;
                                   ASwipeDirection: TksItemSwipeDirection;
                                   AButtons: TksListItemRowActionButtons);  }
    //procedure HideRowActionButtons;
    {$IFNDEF XE10_OR_NEWER}
    procedure DoItemChange(const AItem: TListViewItem); override;
    {$ENDIF}
    function GetRowFromYPos(y: single): TKsListItemRow;
    procedure SetKsItemHeight(const Value: integer);
    procedure SetKsHeaderHeight(const Value: integer);
    function GetMaxScrollPos: single;
    procedure DoActionButtonClicked(AButton: TksListItemRowActionButton);
    procedure HideEmbeddedControls;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure ClearItems;
    procedure RedrawAllRows;
    function ItemsInView: TksVisibleItems;
    procedure BeginUpdate; {$IFDEF XE8_OR_NEWER} override; {$ENDIF}
    procedure EndUpdate; {$IFDEF XE8_OR_NEWER} override; {$ENDIF}
    function IsShowing: Boolean;
    property Items: TKsListItemRows read FItems;
    procedure ShowPopupMenu(APopup: TPopupMenu; x, y: single);
    procedure SelectFirstItem;
    procedure Paint; override;
    property MaxScrollPos: single read GetMaxScrollPos;
    { Public declarations }
  published
    property Appearence: TksListViewAppearence read FAppearence write FAppearence;
    property ItemHeight: integer read FItemHeight write SetKsItemHeight default 44;
    property HeaderHeight: integer read FHeaderHeight write SetKsHeaderHeight default 44;
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
    property SideSpace;
    property OnItemClick: TksListViewRowClickEvent read FOnItemClick write FOnItemClick;
    property OnItemClickRight: TksListViewRowClickEvent read FOnItemRightClick write FOnItemRightClick;
    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property CheckMarks: TksListViewCheckMarks read FCheckMarks write SetCheckMarks default ksCmNone;
    property CheckMarkStyle: TksListViewCheckStyle read FCheckMarkStyle write SetCheckMarkStyle default ksCmsDefault;
    property ClipChildren default True;
    property ClipParent default False;
    property Cursor default crDefault;
    property DeleteButton: TksDeleteButton read FDeleteButton write FDeleteButton;
    property DisableFocusEffect default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property FullWidthSeparator: Boolean read FFullWidthSeparator write FFullWidthSeparator default True;
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
    property ShowIndicatorColors: Boolean read FShowIndicatorColors write SetShowIndicatorColors default False;
    property TabOrder;
    property TabStop;
    property Visible default True;
    property Width;
    property AfterRowCache: TksRowCacheEvent read FAfterRowCache write FAfterRowCache;
    property BeforeRowCache: TksRowCacheEvent read FBeforeRowCache write FBeforeRowCache;

    property OnSelectOption: TksOptionSelectionEvent read FOnOptionSelection write FOnOptionSelection;
    property OnEmbeddedEditChange: TksEmbeddedEditChange read FOnEmbeddedEditChange write FOnEmbeddedEditChange;
    property OnEmbeddedListBoxChange: TksEmbeddedListBoxChange read FOnEmbeddedListBoxChange write FOnEmbeddedListBoxChange;

    property OnSelectDate: TksListViewSelectDateEvent read FOnSelectDate write FOnSelectDate;
    property OnSelectPickerItem: TksListViewSelectPickerItem read FOnSelectPickerItem write FOnSelectPickerItem;
    property OnSearchFilterChanged: TksSearchFilterChange read FOnSearchFilterChanged write FOnSearchFilterChanged;
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

    property PageCaching: TksPageCaching read FPageCaching write FPageCaching;
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
    property OnPullRefresh;

    property AutoTapScroll;
    property AutoTapTreshold;
    //property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection;
    property DisableMouseWheel;

    property SearchVisible;
    property SearchAlwaysOnTop;
    property SelectionCrossfade;
    property PullToRefresh;
    property KeepSelection: Boolean read FKeepSelection write FKeepSelection default False;
    property OnLongClick: TksListViewRowClickEvent read FOnLongClick write FOnLongClick;
    property OnSwitchClick: TksListViewClickSwitchEvent read FOnSwitchClicked write FOnSwitchClicked;
    property OnButtonClicked: TksListViewClickButtonEvent read FOnButtonClicked write FOnButtonClicked;
    property OnSegmentButtonClicked: TksListViewClickSegmentButtonEvent read FOnSegmentButtonClicked write FOnSegmentButtonClicked;
    property OnScrollFinish: TksListViewFinishScrollingEvent read FOnFinishScrolling write FOnFinishScrolling;
    property OnScrollLastItem: TNotifyEvent read FOnScrollLastItem write FOnScrollLastItem;
    property OnItemSwipe: TksItemSwipeEvent read FOnItemSwipe write FOnItemSwipe;
    property OnItemActionButtonClick: TksItemActionButtonClickEvent read FOnItemActionButtonClick write FOnItemActionButtonClick;
  end;

procedure Register;

var
  AccessoryImages: TksAccessoryImageList;


implementation

uses SysUtils, FMX.Platform, ksDrawFunctions, FMX.Ani, FMX.Styles, FMX.Dialogs,
  System.StrUtils, DateUtils, FMX.Forms, Math, ksSlideMenu;

var
  DefaultScrollBarWidth: integer = 7;

  ATextLayout: TTextLayout;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksListView]);
end;


// ------------------------------------------------------------------------------

function CreateGuidStr: string;
var
  AGuid: TGUID;
begin
  Result := '';
  CreateGUID(AGuid);
  Result := GUIDToString(AGuid);
  Result := StringReplace(Result, '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
end;

// ------------------------------------------------------------------------------

{ TksListItemRowObj }

procedure TksListItemRowObj.Assign(ASource: TPersistent);
var
  ASrc: TksListItemRowObj;
begin
  if (ASource is TksListItemRowObj) then
  begin
    ASrc := (ASource as TksListItemRowObj);
    FRect := ASrc.Rect;
    FId := ASrc.ID;
    FPlaceOffset := ASrc.PlaceOffset;
    FRow := ASrc.FRow;
    FAlign := ASrc.Align;
    FVertAlignment := ASrc.VertAlign;
    FTagBoolean := ASrc.TagBoolean;
    FGuid := ASrc.FGuid;
    FWidth := ASrc.Width;
    FHeight := ASrc.Height;
  end;
end;


procedure TksListItemRowObj.CalculateRect(ARowBmp: TBitmap);
var
  w,h: single;
  ABmpWidth: single;
begin
  if FWidth > 0 then Rect.Width := FWidth;
  if FHeight > 0 then Rect.Height := FHeight;

  w := Rect.Width;
  h := Rect.Height;

  ABmpWidth := ARowBmp.Width / GetScreenScale;

  FRect := RectF(0, 0, w, h);
  if FAlign = TListItemAlign.Leading then
    OffsetRect(FRect, FPlaceOffset.X, 0);

  if FAlign = TListItemAlign.Center then
    OffsetRect(FRect, (ABmpWidth - Rect.Width) / 2, 0);

  if FAlign = TListItemAlign.Trailing then
  begin
    OffsetRect(FRect, ABmpWidth - (w + DefaultScrollBarWidth + FPlaceOffset.X + 4), 0);
    if (Self is TKsListItemRowAccessory) = False then
    begin
      if FRow.ShowAccessory then
        OffsetRect(FRect, 0-((FRow.FAccessory.Width)), 0);
    end;
  end;

  case VertAlign of
    TListItemAlign.Center: OffsetRect(FRect, 0, (FRow.Height - FRect.Height) / 2);
    TListItemAlign.Trailing: OffsetRect(FRect, 0, (FRow.Height - FRect.Height));
  end;

  if FAlign = TListItemAlign.Leading then
    OffsetRect(FRect, C_LEFT_MARGIN, FPlaceOffset.Y)
  else
  OffsetRect(FRect, 0, FPlaceOffset.Y)

end;

procedure TksListItemRowObj.Changed;
begin
  //FRow.Cached := False;
  FRow.Changed;
end;

procedure TksListItemRowObj.ChangeSize;
begin
  //
end;

procedure TksListItemRowObj.ProcessClick(x, y: single);
begin
  if (FHitTest) then
  begin
    DoClick(x, y);
    Changed;
    FRow.CacheRow;
  end;
end;

constructor TksListItemRowObj.Create(ARow: TKsListItemRow);
var
  AGuid: TGUID;
begin
  inherited Create;
  FRow := ARow;
  FAlign := TListItemAlign.Leading;
  FPlaceOffset := PointF(0,0);
  FTagBoolean := False;
  CreateGUID(AGuid);
  FGuid := GUIDToString(AGuid);
  FConumesRowClick := GetConsumesRowClick;
  FHitTest := False;
end;

procedure TksListItemRowObj.DoChanged(Sender: TObject);
begin
  Changed;
end;

function TksListItemRowObj.GetConsumesRowClick: Boolean;
begin
  Result := False;
end;

function TksListItemRowObj.GetOffsetX: single;
begin
  Result := FPlaceOffset.X;
end;

function TksListItemRowObj.GetOffsetY: single;
begin
  Result := FPlaceOffset.Y;
end;

function TksListItemRowObj.GetRowRect: TRectF;
begin
  Result := FRow.ListView.GetItemRect(FRow.Index);
end;

{$IFDEF VER290}
   {
function TksListItemRowObj.LocalRect: TRectF;
begin
  Result :=
end;    }

{$ENDIF}

procedure TksListItemRowObj.MouseDown;
begin
  // overridden in descendant classes
end;

procedure TksListItemRowObj.MouseUp;
begin
  // overridden in descendant classes
end;

procedure TksListItemRowObj.DoClick(x, y: single);
begin
  // overridden in descendant classes
end;

function TksListItemRowObj.Render(ACanvas: TCanvas): Boolean;
begin
  Result := True;
end;

procedure TksListItemRowObj.SetAlign(const Value: TListItemAlign);
begin
  if FAlign <> Value then
  begin
    FAlign := Value;
    Changed;
  end;
end;

procedure TksListItemRowObj.SetHeight(const Value: single);
begin
  FHeight := Value;
  ChangeSize;
end;

procedure TksListItemRowObj.SetID(const Value: string);
begin
  if FId <> Value then
  begin
    FId := Value;
    Changed;
  end;
end;

procedure TksListItemRowObj.SetOffsetX(const Value: single);
begin
  FPlaceOffset.X := Value;
end;

procedure TksListItemRowObj.SetOffsetY(const Value: single);
begin
  FPlaceOffset.Y := Value;
end;

procedure TksListItemRowObj.SetRect(const Value: TRectF);
begin
  if Value <> FRect then
  begin
    FRect := Value;
    Changed;
  end;
end;

procedure TksListItemRowObj.SetVertAlign(const Value: TListItemAlign);
begin
  if FVertAlignment <> Value then
  begin
    FVertAlignment := Value;
    Changed;
  end;
end;

procedure TksListItemRowObj.SetWidth(const Value: single);
begin
  FWidth := Value;
  ChangeSize;
end;

// ------------------------------------------------------------------------------

{ TksListItemRowText }

procedure TksListItemRowText.Assign(ASource: TPersistent);
var
  ASrc: TksListItemRowText;
begin
  inherited;
  if (ASource is TksListItemRowText) then
  begin
    ASrc := (ASource as TksListItemRowText);
    FFont.Assign(ASrc.Font);
    FAlignment := ASrc.TextAlignment;
    FTextColor := ASrc.TextColor;
    FText := ASrc.Text;
    FWordWrap := ASrc.WordWrap;
  end;
end;

procedure TksListItemRowText.CalculateRect(ARowBmp: TBitmap);
var
  ASaveFont: TFont;
  AWidthFactor: single;
begin
  if FWidth > 0 then Rect.Width := FWidth;
  if FHeight > 0 then Rect.Height := FHeight;

  if (FWidth = 0) or (FHeight = 0) then
  begin
    ASaveFont := TFont.Create;
    try
      ASaveFont.Assign(ARowBmp.Canvas.Font);
      ARowBmp.Canvas.Font.Assign(FFont);

      if FWidth = 0 then
      begin
        AWidthFactor := 0;
        if FId = C_TITLE then AWidthFactor := 0.5;
        if FId = C_SUBTITLE then AWidthFactor := 0.5;
        if FId = C_DETAIL then AWidthFactor := 0.5;

        if FFullWidth then
          AWidthFactor := 1;

        if FId = C_TITLE then Rect.Width := (FRow.ListView.Width * AWidthFactor) - 32;
        if FId = C_SUBTITLE then Rect.Width := (FRow.ListView.Width * AWidthFactor) - 32;
        if FId = C_DETAIL then Rect.Width := (FRow.ListView.Width * AWidthFactor) - 32;
        if Rect.Width = 0 then
          Rect.Width := ARowBmp.Canvas.TextWidth(FText);
      end;

      if FHeight = 0 then
      begin
        FHeight := CalculateTextHeight;
        if FHeight > FRow.Height  then
        begin
          Rect.Height := FHeight;
          FRow.Height := Round(FHeight);
        end;
      end;
      ARowBmp.Canvas.Font.Assign(ASaveFont);
    finally
      FreeAndNil(ASaveFont);
    end;
  end;
  inherited;
  if (FId = C_TITLE) or (FId = C_SUBTITLE) then
  begin
    if FRow.Image.Bitmap.IsEmpty = False then
      Rect.Offset(FRow.Image.Width+8, 0);
    if (FRow.ListView.ShowIndicatorColors) and (FRow.Purpose = TListItemPurpose.None) then
      Rect.Offset(16, 0);
  end;
end;

function TksListItemRowText.CalculateTextHeight: single;
var
  x: single;
begin
  x := FWidth;
  if FWidth = 0 then
  begin
    if FId = C_TITLE then x := FRow.ListView.Width / 2;
    if FId = C_SUBTITLE then x := FRow.ListView.Width / 2;
    if FId = C_DETAIL then x := FRow.ListView.Width / 2;
  end;
  Result := TextHeight(FText, FFont, FWordWrap, x);
end;

constructor TksListItemRowText.Create(ARow: TKsListItemRow);
begin
  inherited Create(ARow);
  FFont := TFont.Create;
  FTextColor := C_DEFAULT_TEXT_COLOR;
  FWordWrap := False;
  FFullWidth := False;
  VertAlign := TListItemAlign.Center;
  FTextLayout := TTextAlign.Center;
end;

destructor TksListItemRowText.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

function TksListItemRowText.Render(ACanvas: TCanvas): Boolean;
begin
  inherited Render(ACanvas);
  if FBackground <> claNull then
  begin
    ACanvas.Fill.Color := FBackground;
    ACanvas.FillRect(Rect, 0, 0, AllCorners, 1);
  end;
  if FIsHtmlText then
  begin
    RenderHhmlText(ACanvas,
                   Rect.Left,
                   Rect.Top,
                   Rect.Width,
                   Rect.Height,
                   FText,
                   FFont,
                   FTextColor,
                   FWordWrap,
                   FAlignment,
                   FTextLayout,
                   TTextTrimming.Character);
    Result := True;
    Exit;
  end;
  RenderText(ACanvas,
             Rect.Left,
             Rect.Top,
             Rect.Width,
             Rect.Height,
             FText,
             FFont,
             FTextColor,
             FWordWrap,
             FAlignment,
             FTextLayout,
             TTextTrimming.Character);
  Result := True;
end;

procedure TksListItemRowText.SetAlignment(const Value: TTextAlign);
begin
  if FAlignment <> Value then
  begin
  FAlignment := Value;
  Changed;
  end;
end;

procedure TksListItemRowText.SetBackground(const Value: TAlphaColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;

procedure TksListItemRowText.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TksListItemRowText.SetIsHtmlText(const Value: Boolean);
begin
  if FIsHtmlText <> Value then
  begin
    FIsHtmlText := Value;
    Changed;
  end;
end;

procedure TksListItemRowText.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TksListItemRowText.SetTextColor(const Value: TAlphaColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

procedure TksListItemRowText.SetTextLayout(const Value: TTextAlign);
begin
  if FTextLayout <> Value then
  begin
    FTextLayout := Value;
    Changed;
  end;
end;

procedure TksListItemRowText.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

// ------------------------------------------------------------------------------

{ TksListItemRowImage }


procedure TksListItemRowImage.Assign(ASource: TPersistent);
var
  ASrc: TksListItemRowImage;
begin
  inherited;
  if (ASource is TksListItemRowText) then
  begin
    ASrc := (ASource as TksListItemRowImage);
    FBitmap.Assign(ASrc.Bitmap);
    FImageShape := ASrc.ImageShape;
  end;
end;

constructor TksListItemRowImage.Create(ARow: TKsListItemRow);
begin
  inherited Create(ARow);
  FBitmap := TBitmap.Create;
  FBorder := TksListItemStroke.Create;
  FBitmap.OnChange := DoChanged;
  FVertAlignment := TListItemAlign.Center;
  FImageShape := ksRectangleImage;
end;

destructor TksListItemRowImage.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FBorder);
  inherited;
end;

function TksListItemRowImage.Render(ACanvas: TCanvas): Boolean;
var
  ABmp: TBitmap;
  ARectangle: TRectangle;
begin
  try
    Result := inherited Render(ACanvas);
  except
    Result := False;
    Exit; // bug existed pre-seattle where AV's could occur in the TListItemImage.Render
  end;

  ARectangle := TRectangle.Create(FRow.ListView.Parent);
  try
    ARectangle.Width := FBitmap.Width;
    ARectangle.Height := FBitmap.Height;
    ARectangle.Stroke.Kind := TBrushKind.None;
    ARectangle.Fill.Bitmap.Bitmap.Assign(FBitmap);
    ARectangle.Fill.Kind := TBrushKind.Bitmap;
    if FImageShape = ksRoundRectImage then
    begin
      ARectangle.XRadius := FBitmap.Width / 4;;
      ARectangle.YRadius := FBitmap.Width / 4;;
    end;
    if FImageShape = ksCircleImage then
    begin
      ARectangle.XRadius := FBitmap.Width / 2;
      ARectangle.YRadius := FBitmap.Width / 2;
    end;
    ABmp := TBitmap.Create(Round(Rect.Width*4), Round(Rect.Height*4));//(FBitmap.Width, FBitmap.Height);
    try
      ABmp.Clear(claNull);
      ABmp.Canvas.BeginScene;
      ARectangle.PaintTo(ABmp.Canvas, RectF(0, 0, Rect.Width*4, Rect.Height*4), nil);
      ABmp.Canvas.EndScene;
      ACanvas.Stroke.Color := FBorder.Color;

      ACanvas.Stroke.Thickness := FBorder.Thickness;
      ACanvas.DrawEllipse(Rect, 1);

      ACanvas.DrawBitmap(ABmp, RectF(0, 0, Rect.Width*4, Rect.Height*4), Rect, 1);
    finally
      FreeAndNil(ABmp);
    end;
  finally
    ARectangle.DisposeOf;
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

procedure TksListItemRowShape.Assign(ASource: TPersistent);
var
  ASrc: TksListItemRowShape;
begin
  inherited;
  if (ASource is TksListItemRowShape) then
  begin
    ASrc := (ASource as TksListItemRowShape);
    FStroke.Assign(ASrc.Stroke);
    FFill.Assign(ASrc.Fill);
    FShape := ASrc.Shape;
    FCornerRadius := ASrc.CornerRadius;
  end;
end;

constructor TksListItemRowShape.Create(ARow: TKsListItemRow);
begin
  inherited Create(ARow);
  FStroke := TksListItemStroke.Create;
  FFill := TksListItemBrush.Create;
  FShape := ksRectangle;
end;

destructor TksListItemRowShape.Destroy;
begin
  FreeAndNil(FFill);
  FreeAndNil(FStroke);
  inherited;
end;

function TksListItemRowShape.Render(ACanvas: TCanvas): Boolean;
var
  ACorners: TCorners;
  ARect: TRectF;
  ABitmap: TBitmap;
  ARadius: single;
begin
  Result := inherited Render(ACanvas);
  ACorners := [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight];
  ABitmap := TBitmap.Create;
  try
    ABitmap.Width := Round(Width * GetScreenScale);
    ABitmap.Height := Round(Height * GetScreenScale);
    ARect := RectF(0, 0, ABitmap.Width, ABitmap.Height);
    ARadius := Round(FCornerRadius * GetScreenScale);
    ABitmap.Clear(claNull);
    ABitmap.Canvas.BeginScene;
    try
      with ABitmap.Canvas.Fill do
      begin
        Kind := FFill.Kind;
        Color := FFill.Color;
      end;
      with ABitmap.Canvas.Stroke do
      begin
        Kind := FStroke.Kind;
        Color := FStroke.Color;
        Thickness := FStroke.Thickness;
      end;

      if FShape = ksEllipse then
        ABitmap.Canvas.FillEllipse(ARect, 1)
      else
        ABitmap.Canvas.FillRect(ARect, ARadius, ARadius, ACorners, 1);


      if FShape = ksEllipse then
        ABitmap.Canvas.DrawEllipse(ARect, 1)
      else
        ABitmap.Canvas.DrawRect(ARect, ARadius, ARadius, ACorners, 1);
    finally
      ABitmap.Canvas.EndScene;
    end;
    ACanvas.DrawBitmap(ABitmap, ARect, Rect, 1);
  finally
    FreeAndNil(ABitmap);
  end;
end;

procedure TksListItemRowShape.SetCornerRadius(const Value: single);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Value;
    Changed;
  end;
end;

procedure TksListItemRowShape.SetShape(const Value: TksListViewShape);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    Changed;
  end;
end;

// ------------------------------------------------------------------------------

{ TksListItemRow }

procedure TKsListItemRow.CacheRow;
var
  ICount: integer;
  AMargins: TBounds;
  lv: TksListView;
  ADetailHeight: single;
  {$IFDEF XE8_OR_NEWER}
  AImage: TBitmap;
  ASize: TSize;
  {$ENDIF}
  ABmpWidth: single;
  ABeforeCache: TksRowCacheEvent;
  AAfterCache: TksRowCacheEvent;
begin
  if ListView.FUpdateCount > 0 then
    Exit;
  if FCached then
    Exit;


  FCached := False;

  if (OwnsBitmap = False) then
  begin
    Bitmap := TBitmap.Create(1,1);
    Bitmap.BitmapScale := GetScreenScale;
    OwnsBitmap := True;
  end;

  lv := (ListView as TksListView);

  ABeforeCache := lv.BeforeRowCache;
  AAfterCache := lv.AfterRowCache;

  AMargins := lv.ItemSpaces;
  BeginUpdate;
  try



    ABmpWidth := (Round(RowWidth)) - Round((AMargins.Left + AMargins.Right)) * GetScreenScale;
    Bitmap.Height := Trunc(RowHeight);

    ADetailHeight := FDetail.CalculateTextHeight;

    if ADetailHeight >= Bitmap.Height then
    begin
      Owner.Height := Round(ADetailHeight);
      Height := Round(ADetailHeight * GetScreenScale);
      Bitmap.Height := Round(Height);
    end;

    Bitmap.Width := Round(ABmpWidth);

    {$IFDEF MSWINDOWS}
    ScalingMode := TImageScalingMode.Original;
    {$ELSE}
    ScalingMode := TImageScalingMode.StretchWithAspect;
    {$ENDIF}
    Bitmap.Clear(claNull);
    Bitmap.Canvas.BeginScene;

    if Assigned(ABeforeCache) then
      ABeforeCache(lv, Bitmap.Canvas, Self, RectF(0, 0, Bitmap.Width, Bitmap.Height));

    if (FIndicatorColor <> claNull) and (lv.ShowIndicatorColors) then
    begin
      Bitmap.Canvas.Fill.Color := FIndicatorColor;
      Bitmap.Canvas.FillRect(RectF(8, 8, 14, RowHeight(False)-8), 0, 0, [], 1, Bitmap.Canvas.Fill);
    end;

    {$IFDEF XE8_OR_NEWER}
    if (FImageIndex > -1) and (lv.Images <> nil) then
    begin
      ASize.cx := 32;
      ASize.cy := 32;
      AImage := lv.Images.Bitmap(ASize, FImageIndex);
      FImage.Bitmap.Assign(AImage);
    end;
    {$ENDIF}

    if FAutoCheck then
    begin
      FAccessory.AccessoryType := TksAccessoryType.atCheckmark;
      if Checked then
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

    if FImage.Bitmap.IsEmpty = False then
    begin
      FImage.CalculateRect(Bitmap);
      FImage.Render(Bitmap.Canvas);
    end;

    if FTitle.Text <> '' then
    begin
      FTitle.FFullWidth := FDetail.Text = '';
      FTitle.CalculateRect(Bitmap);
      FTitle.Render(Bitmap.Canvas);
    end;

    if FSubTitle.Text <> '' then
    begin
      FSubTitle.FFullWidth := FDetail.Text = '';
      FSubTitle.CalculateRect(Bitmap);
      FSubTitle.Render(Bitmap.Canvas);
    end;

    if FDetail.Text <> '' then
    begin
      FDetail.FFullWidth := (FTitle.Text+FSubTitle.Text) = '';
      FDetail.CalculateRect(Bitmap);
      FDetail.Render(Bitmap.Canvas);
    end;

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

    if Assigned(AAfterCache) then
      AAfterCache(lv, Bitmap.Canvas, Self, RectF(0, 0, Bitmap.Width, Bitmap.Height));

    Bitmap.Canvas.EndScene;
    FCached := True;
  finally
    EndUpdate;
  end;
end;
              {
function TksListItemRow.TextWidth(AText: string): single;
var
  APoint: TPointF;
begin
  ATextLayout.BeginUpdate;
  // Setting the layout MaxSize
  APoint.X := MaxSingle;
  APoint.Y := 100;
  ATextLayout.MaxSize := aPoint;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := False;
  ATextLayout.Font := FFont;
  ATextLayout.HorizontalAlign := TTextAlign.Leading;
  ATextLayout.EndUpdate;
  Result := ATextLayout.Width;
end;   }
      {                  function TksListItemRow.TextHeight(AText: string; AFont: TFont; AWordWrap: Boolean; const AWidth: single = 0): single;
var
  APoint: TPointF;
begin
  ATextLayout.BeginUpdate;
  // Setting the layout MaxSize
  APoint.X := MaxSingle;
  if AWidth > 0 then
    APoint.X := AWidth;
  APoint.Y := 100;

  ATextLayout.MaxSize := aPoint;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := AWordWrap;
  ATextLayout.Font := FFont;
  ATextLayout.HorizontalAlign := TTextAlign.Leading;
  ATextLayout.VerticalAlign := TTextAlign.Leading;
  ATextLayout.EndUpdate;
 // ATextLayout.RenderLayout(nil);
  Result := ATextLayout.Height;
end;
   }

procedure TKsListItemRow.Changed;
begin
  if FUpdating then
    Exit;
  FCached := False;
  if ListView.FUpdateCount = 0 then
  begin
    CacheRow;
    ListView.Invalidate;
  end;
end;

procedure TKsListItemRow.ReleaseAllDownButtons;
var
  ICount: integer;
  AButton: TksListItemRowButton;
begin
  for ICount := 0 to FList.Count-1 do
  begin
    if (FList[ICount] is TksListItemRowButton) then
    begin
      AButton := (FList[ICount] as TksListItemRowButton);
      if AButton.FState <> Unpressed then
      begin
        AButton.FState := Unpressed;
        Changed;
      end;
    end;
  end;
end;

procedure TksListItemRow.ReleaseRow;
begin
  if OwnsBitmap then
  begin
    //Bitmap := ListView.LoadingBitmap;
    OwnsBitmap := False;
    FCached := False;
  end;
end;

procedure TksListItemRow.Render(const Canvas: TCanvas;
  const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
  const SubPassNo: Integer);
var
  ARect: TRectF;
  ANextItem: TKsListItemRow;
  ANextItemIsHeader: Boolean;
  ASeparatorOffset: integer;
  //v: TksListView;
begin
  if SubPassNo <> 0 then
    Exit;

  //lv := ListView;
  if FLastHeight = 0 then
    FLastHeight := Height;

  ANextItemIsHeader := False;
  if Index < ListView.Items.Count-1 then
  begin
    ANextItem := ListView.Items[Index+1];
    ANextItemIsHeader := ANextItem.Purpose = TListItemPurpose.Header;
  end;

  if (OwnsBitmap = True) and (FCached = False) then
    CacheRow;

  if (OwnsBitmap = False) then
  begin
    if (ListView.FPageCaching.Enabled) and (Purpose = TListItemPurpose.None) then
      Bitmap := ListView.LoadingBitmap
    else
      CacheRow;
    ListView.FScrolling := True;
  end;

  {$IFDEF VER290}
  ARect := FLocalRect;
  {$ELSE}
  ARect := LocalRect;
  {$ENDIF}

  if Purpose = TListItemPurpose.Header then
  begin
    if ListView.Appearence.HeaderColor <> claNull then
    begin
      ARect.Top := ARect.Top-1;
      ARect.Bottom := ARect.Bottom+1;
      Canvas.Fill.Color := ListView.Appearence.HeaderColor;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 1);
      ARect.Bottom := ARect.Bottom-1;
      ARect.Top := ARect.Top+1;
    end;
  end;


  if Purpose = TListItemPurpose.None then
  begin
    Canvas.Fill.Color := ListView.Appearence.ItemBackground;
    if FBackgroundColor <> claNull then
      Canvas.Fill.Color := FBackgroundColor;

    begin
      if (Index = ListView.ItemIndex) and (ListView.FActionButtons.FState = ksActionBtnHidden) and  (ListView.FShowSelection) and (CanSelect) then
        Canvas.Fill.Color := GetColorOrDefault(ListView.Appearence.SelectedColor, C_DEFAULT_SELECTED_COLOR);

    end;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 1);
  end;

  ListView.DoRenderRow(Self);

  inherited;
  //Canvas.DrawBitmap(Bitmap, RectF(0, 0, Bitmap.Width, Bitmap.Height), RectF(0, ARect.Top, Bitmap.Width/GetScreenScale, ARect.Top+(Bitmap.Height/GetScreenScale)), 1, True);

  if (Purpose = TListItemPurpose.None) and (ANextItemIsHeader = False) then
  begin
    Canvas.Fill.Color := ListView.Appearence.SeparatorColor;
    if Canvas.Fill.Color = claNull then
      Canvas.Fill.Color := C_DEFAULT_SEPARATOR_COLOR;
    ASeparatorOffset := 0;
    if ListView.FFullWidthSeparator = False then
      ASeparatorOffset := 24;
      Canvas.FillRect(RectF(ASeparatorOffset, ARect.Bottom-1, ARect.Right, ARect.Bottom), 0, 0, AllCorners, 1, Canvas.Fill);
  end;
end;

constructor TKsListItemRow.Create(const AOwner: TListItem);
var
  lv: TksListView;
begin
  inherited Create(AOwner);
  lv := (ListView as TksListView);
  FImage := TksListItemRowImage.Create(Self);
  FAccessory := TKsListItemRowAccessory.Create(Self);
  FTitle := TksListItemRowText.Create(Self);
  FSubTitle := TksListItemRowText.Create(Self);
  FDetail := TksListItemRowText.Create(Self);
  FPickerItems := TStringList.Create;
  (FPickerItems as TStringList).OnChange := PickerItemsChanged;
  {$IFDEF MSWINDOWS}
  ScalingMode := TImageScalingMode.Original;
  {$ENDIF}
  PlaceOffset.X := 0;
  FIndicatorColor := claNull;
  FList := TksListItemObjects.Create(True);
  FList.OnNotify := DoOnListChanged;

  FImage.Width := lv.ItemImageSize;
  FImage.Height := lv.ItemImageSize;

  OwnsBitmap := False;

  FTextColor := C_DEFAULT_TEXT_COLOR;
  FFont := TFont.Create;
  FCached := False;
  FShowAccessory := True;
  FAutoCheck := False;
  FImageIndex := -1;
  FCanSelect := True;
   // title...
  //FTitle.Font.Size := 13;
  FTitle.TextColor := claBlack;
  FTitle.TextAlignment := TTextAlign.Leading;


  FTitle.ID := C_TITLE;
  // sub-title...
  FSubTitle.TextColor := claGray;
  //FSubTitle.Font.Size := 13;
  FSubTitle.TextAlignment := TTextAlign.Leading;
  FSubTitle.ID := C_SUBTITLE;
  // detail...
  FDetail.Align := TListItemAlign.Trailing;
  FDetail.TextColor := claDodgerblue;
  //FDetail.Font.Size := 13;
  FDetail.ID := C_DETAIL;
  FDetail.TextAlignment := TTextAlign.Trailing;
  FRowHeight := lv.ItemHeight;
  FBackgroundColor := claNull;
  FLastHeight := 0;
  FUpdating := False;

end;

destructor TKsListItemRow.Destroy;
begin
  FList.Clear;
  FreeAndNil(FList);
  FreeAndNil(FFont);
  FreeAndNil(FAccessory);
  FreeAndNil(FImage);
  FreeAndNil(FTitle);
  FreeAndNil(FSubTitle);
  FreeAndNil(FDetail);
  FreeAndNil(FPickerItems);
  inherited;
end;

function TKsListItemRow.ScreenWidth: single;
begin
  Result := TksListView(Owner.Parent).Width;
{$IFDEF MSWINDOWS}
  Result := Result - 40;
{$ENDIF}
end;

function TKsListItemRow.RowHeight(const AScale: Boolean = True): single;
var
  lv: TksListView;
begin
  lv := TksListView(Owner.Parent);
  Result := Height;// lv.ItemHeight;
  if Purpose = TListItemPurpose.Header then
    Result := lv.HeaderHeight;
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

function TKsListItemRow.GetAccessory: TksAccessoryType;
begin
  Result := FAccessory.AccessoryType;
end;

function TksListItemRow.GetData(const AIndex: string): TValue;
begin
  Result := (Owner as TListViewItem).Data[AIndex];
end;

function TksListItemRow.GetHasData(const AIndex: string): Boolean;
begin
  Result := (Owner as TListViewItem).HasData[AIndex];
end;

function TKsListItemRow.GetListView: TksListView;
begin
  Result := (Owner.Parent as TksListView);
end;

function TKsListItemRow.GetPurpose: TListItemPurpose;
begin
  Result := (Owner as TListItem).Purpose;
end;

function TKsListItemRow.GetRowObject(AIndex: integer): TksListItemRowObj;
begin
  Result := FList[AIndex];
end;

function TksListItemRow.GetRowObjectByID(AId: string): TksListItemRowObj;
begin
  Result := FList.ObjectByID(AId);
end;

function TKsListItemRow.GetRowObjectCount: integer;
begin
  Result := FList.Count;
end;

function TKsListItemRow.GetSearchIndex: string;
begin
  Result := TListViewItem(Owner).Text;
end;

function TksListItemRow.GetSearchText: string;
begin
  Result := (Owner as TListViewItem).Text;
end;

{$IFDEF VER290}

function TksListItemRow.LocalRect: TRectF;
begin
  Result := FLocalRect;
end;

{$ENDIF}

procedure TksListItemRow.PickerItemsChanged(Sender: TObject);
begin
  FSelector := TksListItemRowSelector.NoSelector;
  if FPickerItems.Count > 0 then
    FSelector := TksListItemRowSelector.ItemPicker;
end;

procedure TKsListItemRow.ProcessClick;
var
  ICount: integer;
begin
  if FSelector = DateSelector then
  begin
    ListView.SelectDate(Self, FSelectionValue, ListView.DoSelectDate);
    Exit;
  end;

  if FSelector = ItemPicker then
  begin
    ListView.SelectItem(Self, FPickerItems, FSelectionValue, ListView.DoSelectPickerItem);
    Exit;
  end;

  if FAutoCheck then
  begin
    Accessory := TksAccessoryType.atCheckmark;
    if ListView.CheckMarks = TksListViewCheckMarks.ksCmSingleSelect  then
    begin
      for ICount := 0 to ListView.Items.Count-1 do
        ListView.Items[ICount].Checked := ListView.Items[ICount] = Self;
    end
    else
      Checked := not Checked;
  end;
end;

procedure TKsListItemRow.DoOnListChanged(Sender: TObject;
  const Item: TksListItemRowObj; Action: TCollectionNotification);
begin
  FCached := False;
end;

procedure TksListItemRow.DoResize;
begin
  inherited;
  FCached := False;
  (Owner as TListViewItem).Height := Round(Height);
end;

// ------------------------------------------------------------------------------

// bitmap drawing functions...

function TKsListItemRow.DrawAccessory(AAccessory: TksAccessoryType; x, y: single): TksListItemRowImage;
var
  AImg: TksAccessoryImage;
begin
  AImg := AccessoryImages.Images[AAccessory];
  Result := DrawBitmap(AImg, x, y, AImg.Width, AImg.Height);
  Result.VertAlign := TListItemAlign.Leading;
end;

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
  Result.Width := AWidth;
  Result.Height := AHeight;
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
  Result.Width := AWidth;
  Result.Height := AHeight;
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

function TksListItemRow.DrawProgressBar(x, y, AWidth, AHeight: single; APercent: integer;
                                        ABarColor: TAlphaColor;
                                        ACornerRadius: single;
                                        const AAlign: TListItemAlign = TListItemAlign.Trailing;
                                        const ABackgroundColor: TAlphaColor = claWhite;
                                        const ABorderColor: TAlphaColor = claBlack): TksListItemRowProgressBar;
begin
  Result := TksListItemRowProgressBar.Create(Self);
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.PlaceOffset := PointF(x,y);
  Result.Align := AAlign;
  Result.VertAlign := TListItemAlign.Center;
  Result.ProgressPercent := APercent;
  Result.BarColor := ABarColor;
  Result.BackgroundColor := ABackgroundColor;
  Result.BorderColor := ABorderColor;
  Result.CornerRadius := ACornerRadius;
  FList.Add(Result);
end;

function TKsListItemRow.AddButton(AStyle: TksImageButtonStyle; const ATintColor: TAlphaColor = claNull): TksListItemRowButton;
var
  AStr: string;
begin
  Result := AddButton(44, '', ATintColor);
  Result.Width := 44;
  Result.Height := 44;
  {case AStyle of
    atAction: AStr := 'actiontoolbutton';
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
  end;   }
  Result.StyleLookup := AStr;
end;

function TKsListItemRow.AddButton(AWidth: integer;
                                  AText: string;
                                  const ATintColor: TAlphaColor = claNull;
                                  const AVertAlign: TListItemAlign = TListItemAlign.Center;
                                  const AYPos: integer = 0): TksListItemRowButton;
begin
  Result := TksListItemRowButton.Create(Self);
  Result.Align := TListItemAlign.Trailing;
  Result.VertAlign := AVertAlign;
  Result.PlaceOffset := PointF(0, AYPos);
  Result.Width := AWidth;
  Result.Height := 32;
  Result.StyleLookup := 'listitembutton';
  if ATintColor <> claNull then
  begin
    Result.TintColor := ATintColor;
  end;
  Result.Text := AText;
  ShowAccessory := False;
  FList.Add(Result);
  Changed;
end;


function TKsListItemRow.AddSegmentButtons(AWidth: integer;
                                          ACaptions: array of string;
                                          const AItemIndex: integer = 0): TksListItemRowSegmentButtons;
begin
  Result := AddSegmentButtons(0, AWidth, ACaptions, TListItemAlign.Trailing, AItemIndex);
end;

function TKsListItemRow.AddSegmentButtons(AXPos, AWidth: integer;
                                          ACaptions: array of string;
                                          AAlign: TListItemAlign;
                                          const AItemIndex: integer = 0): TksListItemRowSegmentButtons;
var
  ICount: integer;
begin
  CanSelect := False;
  Result := TksListItemRowSegmentButtons.Create(Self);
  Result.Align := AAlign;
  Result.VertAlign := TListItemAlign.Center;
  Result.Width := AWidth;
  Result.Height := C_SEGMENT_BUTTON_HEIGHT;

  Result.TintColor := C_DEFAULT_SEGMENT_BUTTON_COLOR;
  for ICount := Low(ACaptions) to High(ACaptions) do
    Result.Captions.Add(ACaptions[ICount]);
  Result.ItemIndex := AItemIndex;
  Result.PlaceOffset := PointF(AXPos, 0);
  ShowAccessory := False;
  FList.Add(Result);
end;

function  TKsListItemRow.AddTable(AX, AY, AColWidth, ARowHeight: single; AColCount, ARowCount: integer): TksListItemRowTable;
begin
  Result := TksListItemRowTable.Create(Self);
  Result.DefaultRowHeight := ARowHeight;
  Result.DefaultColWidth := AColWidth;
  Result.ColCount := AColCount;
  Result.RowCount := ARowCount;
  Result.PlaceOffset := PointF(AX, AY);
  Result.ResizeTable;
  FList.Add(Result);
end;

function TKsListItemRow.AddEdit(AX, AY, AWidth: single; AText: string): TksListItemEmbeddedEdit;
begin
  Result := TksListItemEmbeddedEdit.Create(Self);
  Result.Width := AWidth;
  Result.PlaceOffset := PointF(AX, AY);
  Result.GetEdit.Text := AText;
  FList.Add(Result);
end;

function TKsListItemRow.AddMemo(AX, AY, AWidth, AHeight: single; AText: string): TksListItemEmbeddedMemo;
begin
  Result := TksListItemEmbeddedMemo.Create(Self);
  Result.Width := AWidth;
  Result.Height := AWidth;
  Result.PlaceOffset := PointF(AX, AY);
  Result.GetMemo.Text := AText;
  FList.Add(Result);
end;

function TksListItemRow.AddSpinBox(AX, AY, AWidth: single;
  AValue: integer): TksListItemEmbeddedSpinBox;
begin
  Result := TksListItemEmbeddedSpinBox.Create(Self);
  Result.Width := AWidth;
  Result.PlaceOffset := PointF(AX, AY);
  Result.SpinBox.Value := AValue;
  FList.Add(Result);
end;



function TKsListItemRow.AddTrackBar(AX, AY, AWidth: single): TksListItemEmbeddedTrackBar;
begin
  Result := TksListItemEmbeddedTrackBar.Create(Self);
  Result.Width := AWidth;
  Result.PlaceOffset := PointF(AX, AY);
  FList.Add(Result);
end;

{
function TksListItemRow.AddEmbeddedButton(AX, AY, AWidth: single;
  AText: string): TksListItemEmbeddedButton;
begin
  Result := TksListItemEmbeddedButton.Create(Self);
  Result.Width := AWidth;
  Result.Height := 22;
  Result.PlaceOffset := PointF(AX, AY);
  Result.Button.Text := AText;
  FList.Add(Result);
  CacheRow;
end;      }

function TksListItemRow.AddListBox(AX, AY, AWidth, AHeight, AItemHeight: single;
  AItems: TStrings): TksListItemEmbeddedListBox;
var
  ICount: integer;
begin
  Result := TksListItemEmbeddedListBox.Create(Self);
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.PlaceOffset := PointF(AX, AY);
  for ICount := 0 to AItems.Count-1 do
  begin
    with Result.GetListBox.Items.Add do
    begin
      Text := AItems[ICount];
      ShowAccessory := False;
    end;
  end;
  FList.Add(Result);
  CacheRow;
end;

function TksListItemRow.AddListBox(AX, AY, AWidth, AHeight, AItemHeight: single;
  AItems: array of string): TksListItemEmbeddedListBox;
var
  AStrings: TStrings;
  ICount: integer;
begin
  AStrings := TStringList.Create;
  try
    for ICount := Low(AItems) to High(AItems) do
      AStrings.Add(AItems[ICount]);
    Result := AddListBox(AX, AY, AWidth, AHeight, AItemHeight, AStrings);
  finally
    AStrings.Free;
  end;
end;

function TKsListItemRow.AddOptionSelectiontBox(AX, AY, AWidth, AHeight, AItemHeight: single; AItems: TStrings): TksListItemOptionSelector;
begin
  Result := TksListItemOptionSelector.Create(Self);
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.PlaceOffset := PointF(AX, AY);
  Result.FItems.Assign(AItems);
  Result.ItemHeight := AItemHeight;
  FList.Add(Result);
  CacheRow;
end;

function TKsListItemRow.AddOptionSelectiontBox(AX, AY, AWidth, AHeight, AItemHeight: single; AItems: array of string): TksListItemOptionSelector;
var
  AStrings: TStrings;
  ICount: integer;
begin
  AStrings := TStringList.Create;
  try
    for ICount := Low(AItems) to High(AItems) do
      AStrings.Add(AItems[ICount]);
    Result := AddOptionSelectiontBox(AX, AY, AWidth, AHeight, AItemHeight, AStrings);
  finally
    AStrings.Free;
  end;
end;

function TKsListItemRow.AddSwitch(x: single;
                                  AIsChecked: Boolean;
                                  const AAlign: TListItemAlign = TListItemAlign.Trailing): TksListItemRowSwitch;
var
  ASize: TSizeF;
begin
  ASize.Width := 50;
  ASize.Height := 30;
  Result := TksListItemRowSwitch.Create(Self);
  Result.Width := ASize.Width;
  Result.Height := ASize.Height;
  Result.Align := AAlign;
  Result.VertAlign := TListItemAlign.Center;
  Result.PlaceOffset := PointF(x, 0);
  Result.IsChecked := AIsChecked;
  FCanSelect := False;
  FList.Add(Result);
end;

function TksListItemRow.AddSwitchRight(AMargin: integer; AIsChecked: Boolean): TksListItemRowSwitch;
begin
  Result := AddSwitch(AMargin, AIsChecked, TListItemAlign.Trailing)
end;

procedure TksListItemRow.Assign(Source: TPersistent);
begin
  //
end;

procedure TKsListItemRow.SetAccessory(const Value: TksAccessoryType);
begin
  FAccessory.AccessoryType := Value;
end;

procedure TKsListItemRow.SetAutoCheck(const Value: Boolean);
begin
  FAutoCheck := Value;
  if FAutoCheck then
    FAccessory.AccessoryType := TksAccessoryType.atCheckmark;
  Changed;
end;

procedure TksListItemRow.SetBackgroundColor(const Value: TAlphaColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TKsListItemRow.SetCanSelect(const Value: Boolean);
begin
  if FCanSelect <> Value then
  begin
    FCanSelect := Value;
    ListView.Repaint;
  end;
end;

procedure TKsListItemRow.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Changed;
  end;
end;

procedure TksListItemRow.SetData(const AIndex: string; const Value: TValue);
begin
  (Owner as TListViewItem).Data[AIndex] := Value;
end;

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
  if FIndicatorColor <> Value then
  begin
    FIndicatorColor := Value;
    Changed;
  end;
end;

procedure TksListItemRow.SetPickerItems(const Value: TStrings);
begin
  if Value <> nil then
    FPickerItems.Assign(Value);
end;

procedure TKsListItemRow.SetPurpose(const Value: TListItemPurpose);
begin
  (Owner as TListItem).Purpose := Value;
end;

procedure TksListItemRow.SetRowFontStyle(AFontStyle: TFontStyles);
var
  ICount: integer;
begin
  FFont.Style := AFontStyle;
  for ICount := 0 to FList.Count-1 do
  begin
    if (FList[ICount] is TksListItemRowText) then
      (FList[ICount] as TksListItemRowText).Font.Style := AFontStyle;
  end;
  FTitle.Font.Style := AFontStyle;
  FSubTitle.Font.Style := AFontStyle;
  FDetail.Font.Style := AFontStyle;
end;

procedure TksListItemRow.SetRowTextColor(AColor: TAlphaColor);
var
  ICount: integer;
begin
  FTextColor := AColor;
  for ICount := 0 to FList.Count-1 do
  begin
    if (FList[ICount] is TksListItemRowText) then
      (FList[ICount] as TksListItemRowText).TextColor := AColor;
  end;
  FTitle.TextColor := AColor;
  FSubTitle.TextColor:= AColor;
  FDetail.TextColor := AColor;
  Changed;
end;

procedure TKsListItemRow.SetSearchIndex(const Value: string);
begin
  TListViewItem(Owner).Text := Value;
end;

procedure TksListItemRow.SetSearchText(const Value: string);
begin
  (Owner as TListViewItem).Text := Value;
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
  const AVertAlign: TListItemAlign = TListItemAlign.Center;
  const AWordWrap: Boolean = False): TksListItemRowText;
var
  AWidth: single;
begin
  AWidth := TextWidth(AText, False);
  Result := TextOut(AText, x,  AWidth, AVertAlign, AWordWrap);
end;

function TKsListItemRow.TextOut(AText: string; x, AWidth: single;
  const AVertAlign: TListItemAlign = TListItemAlign.Center;
  const AWordWrap: Boolean = False): TksListItemRowText;
begin
  Result := TextOut(AText, x, 0, AWidth, AVertAlign, AWordWrap);
end;


function TKsListItemRow.TextOut(AText: string; x, y, AWidth: single;
  const AVertAlign: TListItemAlign = TListItemAlign.Center;
  const AWordWrap: Boolean = False): TksListItemRowText;
var
  AHeight: single;
begin
  Result := TksListItemRowText.Create(Self);

  Result.Font.Assign(Font);
  Result.FPlaceOffset := PointF(x, y);

  AHeight := ksDrawFunctions.TextHeight(AText, FFont, AWordWrap, AWidth);

  if AWidth = 0 then
    AWidth := ksDrawFunctions.TextWidth(AText, Font);

  Result.Width := AWidth;
  Result.Height := AHeight;



  Result.VertAlign := AVertAlign;
  Result.TextAlignment := TTextAlign.Leading;
  Result.TextColor := FTextColor;
  Result.Text := AText;
  Result.WordWrap := AWordWrap;
  if SearchIndex = '' then
    SearchIndex := AText;
  FList.Add(Result);
  Changed;
end;

function TKsListItemRow.TextOutRight(AText: string; y, AWidth: single;
  AXOffset: single; const AVertAlign: TListItemAlign = TListItemAlign.Center): TksListItemRowText;
begin
  Result := TextOut(AText, AXOffset, y, AWidth, AVertAlign);
  Result.Align := TListItemAlign.Trailing;
  Result.TextAlignment := TTextAlign.Trailing;
end;


function TksListItemRow.TextWidth(AText: string; AIsHtml: Boolean): single;
begin
  if AIsHtml then
    Result := ksDrawFunctions.TextSizeHtml(AText, FFont, 0).X
  else
    Result := ksDrawFunctions.TextWidth(AText, FFont);
end;

// ------------------------------------------------------------------------------

function TksListItemRow.TextBox(AText: string;
                                ARect: TRectF;
                                ATextAlign: TTextAlign;
                                ATextLayout: TTextAlign;
                                const ABackground: TAlphaColor = claNull): TksListItemRowText;
begin
  FUpdating := True;
  try
    Result := TextOut(AText, ARect.Left, ARect.Top, ARect.Width, TListItemAlign.Leading, True);
    Result.Background := ABackground;
    Result.Height := ARect.Height;
    Result.TextAlignment := ATextAlign;
    Result.TextLayout := ATextLayout;
  finally
    FUpdating := False;
  end;
  Changed;
end;

function TksListItemRow.TextBoxHtml(AText: string; ARect: TRectF): TksListItemRowText;
begin
  FUpdating := True;
  try
    Result := TextOut(AText, ARect.Left, ARect.Top, ARect.Width, TListItemAlign.Leading, True);
    Result.IsHtmlText := True;
    Result.Height := ARect.Height;
  finally
    FUpdating := False;
  end;
  Changed;
end;

function TksListItemRow.TextHeight(AText: string; AWordWrap, AIsHtml: Boolean; const AMaxWidth: single): single;
begin
  if AIsHtml then
    Result := ksDrawFunctions.TextSizeHtml(AText, FFont, AMaxWidth).Y
  else
    Result := ksDrawFunctions.TextHeight(AText, FFont, AWordWrap, AMaxWidth);
end;

{ TksListViewAppearence }

constructor TksListViewAppearence.Create(AListView: TksListView);
begin
  inherited Create;
  FListView := AListView;
  FBackground := claWhite;
  FItemBackground := claWhite;
  FSeparatorColor := $FFE0E0E0;
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

procedure TksListViewAppearence.SetHeaderColor(const Value: TAlphaColor);
begin
  FHeaderColor := Value;
  FListView.ApplyStyle;
end;

procedure TksListViewAppearence.SetItemBackground(const Value: TAlphaColor);
begin
  FItemBackground := Value;
  FListView.ApplyStyle;
end;

procedure TksListViewAppearence.SetSelectedColor(const Value: TAlphaColor);
begin
  FSelectedColor := Value;
  FListView.ApplyStyle;
end;

procedure TksListViewAppearence.SetSeparatorBackground(
  const Value: TAlphaColor);
begin
  FSeparatorColor := Value;
  FListView.ApplyStyle;
end;

// ------------------------------------------------------------------------------

{ TksListView }

procedure TKsListItemRows.CheckAll;
var
  ICount: integer;
begin
  for ICount := 0 to Count-1 do
    Items[ICount].Checked := True;
  FListView.RedrawAllRows;
end;

procedure TKsListItemRows.Clear;
begin
  FListViewItems.Clear;
end;

{
procedure TksListView.CacheEmbeddedControls;
var
  ICount: integer;
begin
  for ICount := 0 to FEmbeddedControls.Count-1 do
  begin
    if FEmbeddedControls[ICount].FInitialized = False then
      FEmbeddedControls[ICount].CacheToBitmap;
  end;
end;  }

procedure TksListView.CachePages;
var
  ICount: integer;
  AFilteredIndex: integer;
begin
  //CacheEmbeddedControls;

  if _Items.Count = 0 then
    Exit;
  AFilteredIndex := 0;
  for ICount := 0 to _Items.Count-1 do
  begin
    if _Items[ICount].Index = FLastRenderedIndex then
      AFilteredIndex := ICount;
  end;

  for ICount := 0 to _Items.Count-1 do
  begin
    if (ICount >= (AFilteredIndex - FPageCaching.FPageSize)) and (ICount <= (AFilteredIndex + FPageCaching.FPageSize)) then
      Items[_Items[ICount].Index].CacheRow
    else
      Items[_Items[ICount].Index].ReleaseRow;
  end;
  Invalidate;
end;

procedure TksListView.ClearItems;
begin
  TListView(Self).Items.Clear;
  if FSearchEdit <> nil then
  FSearchEdit.Text := '';
  Items.Clear;
end;

procedure TksListView.ComboClosePopup(Sender: TObject);
begin
  (Sender as TStyledControl).Width := 0;
  RemoveObject(Sender as TFmxObject);
end;

constructor TksListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService);
  {$IFDEF XE10_OR_NEWER}
  FMouseEventsTimer := 0;
  {$ENDIF}
  TListView(Self).OnDeleteItem := DoOnDeleteItem;
  FMouseEvents := TksMouseEventList.Create;
  FItems := TKsListItemRows.Create(Self, inherited Items);
  FCombo := nil;
  FActionButtons := TksListItemRowActionButtons.Create(Self);
  FPageCaching := TksPageCaching.Create;
  FDeleteButton := TksDeleteButton.Create;
  FScreenScale := GetScreenScale;
  FAppearence := TksListViewAppearence.Create(Self);
  //FEmbeddedControls := TList<TksListItemEmbeddedControl>.Create;
  {FEmbeddedEditControl := TEdit.Create(nil);
  FEmbeddedEditControl.Position.X := 0;
  FEmbeddedEditControl.Width := 1;

  FEmbeddedListBoxControl := TListBox.Create(nil);
  FEmbeddedListBoxControl.Position.X := 0;
  FEmbeddedListBoxControl.Width := 1;     }

  {if csDesigning in ComponentState = False then
  begin
    (Owner as TFmxObject).InsertObject(0,FEmbeddedEditControl);
    (Owner as TFmxObject).InsertObject(0,FEmbeddedListBoxControl);
  end;   }

  FItemHeight := 44;
  FHeaderHeight := 44;
  FLastWidth := 0;
  FSelectOnRightClick := False;
  FLastScrollPos := 0;
  FScrolling := False;



  //FScrollTimer := TTimer.Create(Self);
  //FScrollTimer.Interval := 500;
  //FScrollTimer.OnTimer := DoScrollTimer;
  //FScrollTimer.Enabled := True;


  FCheckMarks := ksCmNone;
  FCheckMarkStyle := ksCmsDefault;
  FItemImageSize := 32;
  FShowIndicatorColors := False;
  FKeepSelection := False;
  ItemSpaces.Right := 0;
  ItemSpaces.Left := 0;
  FScrollDirection := sdDown;
  CanSwipeDelete := False;
  inherited ShowSelection := False;
  FShowSelection := True;
  FMouseDownPos := PointF(-1, -1);
  //FDisableMouseMove := False;
  FFullWidthSeparator := True;
  FProcessingMouseEvents := False;
  FCombo := nil;
  FDateSelector := nil;
  //FLastSelectedControl := nil;
  FActiveEmbeddedControl := nil;
  FScrollTimer := 0;
end;

procedure TksListView.DoDeselectRow;
begin
  FLastIndex := -1;
  ItemIndex := -1;
  Invalidate;
  FTimerService.DestroyTimer(FDeselectTimer);
end;

procedure TksListView.DeselectRow(const ADelay: integer = 0);
begin
  if ADelay > 0 then
  begin
    FTimerService.DestroyTimer(FDeselectTimer);
    FDeselectTimer := FTimerService.CreateTimer(ADelay, DoDeselectRow)
  end
  else
    DoDeselectRow;
end;
{var
  ATask: ITask;
begin
  if ItemIndex = -1 then
    Exit;
  ATask := TTask.Run(
  procedure
  begin
    TThread.Queue(Nil,
      procedure
      begin
        if ADelay > 0 then
          Sleep(ADelay);
        FLastIndex := -1;
        ItemIndex := -1;
        Invalidate;
      end
    );
  end
);
  TTask.WaitForAll([ATask]);
end; }

destructor TksListView.Destroy;
begin
  FreeAndNil(FAppearence);
  FreeAndNil(FItems);
  if FLoadingBitmap <> nil then
    FreeAndNil(FLoadingBitmap);
  FreeAndNil(FActionButtons);
  FreeAndNil(FPageCaching);
  FreeAndNil(FDeleteButton);
  FreeAndNil(FMouseEvents);
  //FreeAndNil(FEmbeddedControls);
  // destroy FMX timers...

  {$IFDEF XE10_OR_NEWER}
  FTimerService.DestroyTimer(FMouseEventsTimer);

  {$ENDIF}

  FTimerService.DestroyTimer(FScrollTimer);
  {$IFDEF NEXTGEN}
  if FCombo <> nil then FCombo.DisposeOf;
  if FDateSelector <> nil then FDateSelector.DisposeOf;
  {$ELSE}
  if FCombo <> nil then FCombo.Free;
  if FDateSelector <> nil then FDateSelector.Free;
  {$ENDIF}
  inherited;
end;

function TKsListItemRows.AddRowDateSelector(AText: string;
  ADate: TDateTime): TKsListItemRow;
begin
  Result := AddRow(AText, '', FormatDateTime('ddd, dd mmmm, yyyy', ADate), atMore);
  Result.Selector := DateSelector;
  Result.FSelectionValue := ADate;
end;

function TKsListItemRows.AddRowItemSelector(AText, ASelected: string; AItems: TStrings): TKsListItemRow;
begin
  Result := AddRow(AText, '', ASelected, atMore);
  Result.Selector := ItemPicker;
  Result.FPickerItems.Assign(AItems);
  Result.FSelectionValue := ASelected;
end;

function TKsListItemRows.AddRowItemSelector(AText, ASelected: string; AItems: array of string): TKsListItemRow;
var
  AStrings: TStrings;
  ICount: integer;
begin
  AStrings := TStringList.Create;
  try
    for ICount := Low(AItems) to High(AItems) do
      AStrings.Add(AItems[ICount]);
    Result := AddRowItemSelector(AText, ASelected, AStrings);
  finally
    FreeAndNil(AStrings);
  end;
end;

function TKsListItemRows.AddHeader(AText: string): TKsListItemRow;
begin
  Result := AddRow('', '', '', atNone);
  Result.Owner.Purpose := TListItemPurpose.Header;
  Result.CanSelect := False;

  //Result.Font.Assign();

  Result.Height := FListView.HeaderHeight;
  Result.Owner.Height := FListView.HeaderHeight;
  Result.Title.Text := AText;
  Result.Title.TextColor := claBlack;
  Result.VertAlign := TListItemAlign.Trailing;
  if FListView.FUpdateCount = 0 then
    Result.CacheRow;
end;

function TKsListItemRows.AddRow(AText: string; const AAccessoryType: TksAccessoryType = atNone): TKsListItemRow;
begin
  Result := AddRow(AText, '', '', AAccessoryType);
end;

function TKsListItemRows.AddRow(AText, ASubTitle, ADetail: string; AAccessory: TksAccessoryType;
  const AImageIndex: integer = -1; const AFontSize: integer = 14;
  AFontColor: TAlphaColor = C_DEFAULT_TEXT_COLOR): TKsListItemRow;
var
  ABmp: TBitmap;
  {$IFDEF XE8_OR_NEWER}
  ASize: TSize;
  {$ENDIF}
begin
  ABmp := nil;
  {$IFDEF XE8_OR_NEWER}
  if FListView.Images <> nil then
    ABmp := FListView.Images.Bitmap(ASize, AImageIndex);
  {$ENDIF}
  Result := AddRow(AText, ASubTitle, ADetail, AAccessory, ABmp, AFontSize, AFontColor);
end;

function TKsListItemRows.AddRow(AText, ASubTitle, ADetail: string;
  AAccessory: TksAccessoryType; AImage: TBitmap; const AFontSize: integer;
  AFontColor: TAlphaColor): TKsListItemRow;
var
  r: TListViewItem;
begin
  FListView.BeginUpdate;
  try
    r := FListView.AddItem;
    r.Height := FListView.ItemHeight;
    Result := TKsListItemRow.Create(r);
    Result.Index := Count-1;
    Result.Height := FListView.ItemHeight;
    Result.Purpose := TListItemPurpose.None;

    if FListView.CheckMarks <> ksCmNone then
      Result.AutoCheck := True;
    Result.Name := 'ksRow';
    Result.ShowAccessory := AAccessory <> atNone;
    Result.Accessory := AAccessory;

    Result.SetFontProperties('', AFontSize, AFontColor, []);
    Result.Image.Bitmap.Assign(AImage);


    Result.Title.Text := AText;
    Result.SubTitle.Text := ASubTitle;
    Result.Detail.Text := ADetail;
    if ASubTitle <> '' then
    begin
      Result.Title.PlaceOffset := PointF(0, -9);
      Result.SubTitle.PlaceOffset := PointF(0,9);
    end;
    if (FListView.FUpdateCount = 0) then
      Result.CacheRow;

    Result.SearchText := AText+ASubtitle+ADetail;

    if FListView.PageCaching.Enabled then
    begin
      if (Count > FListView.PageCaching.PageSize) then
        FListView.StartScrollTimer;
      //FScrollTimer := FTimerService.CreateTimer(500, DoScrollTimer);
    end;
  finally
    FListView.EndUpdate;
  end;
end;

function TksListView.AddItem: TListViewItem;
begin
  Result := inherited Items.Add;
end;

procedure TksListView.SelectDate(ARow: TKsListItemRow; ASelected: TDateTime; AOnSelectDate: TNotifyEvent);
begin
  if FDateSelector = nil then
  begin
    FDateSelector := TDateEdit.Create(nil);
    FDateSelector.OnClosePicker := ComboClosePopup;
  end;
  FDateSelector.OnChange := nil;
  FDateSelector.TagObject := ARow;
  FDateSelector.Width := 0;
  {$IFDEF MSWINDOWS}
  FDateSelector.Width := 200;
  {$ENDIF}
  FDateSelector.Align := TAlignLayout.Center;
  AddObject(FDateSelector);
  Application.ProcessMessages;
  FDateSelector.Date := ASelected;
  FDateSelector.OnChange := AOnSelectDate;
  FDateSelector.OpenPicker;
end;

procedure TksListView.SelectFirstItem;
var
  AIndex: integer;
  ICount: integer;
begin
  AIndex := -1;
  for ICount := 0 to Items.Count-1 do
  begin
    if Items[ICount].Purpose = TListItemPurpose.None then
    begin
      AIndex := ICount;
      Break;
    end;
  end;
  ItemIndex := AIndex;
end;

procedure TksListView.SelectItem(ARow: TKsListItemRow; AItems: TStrings; ASelected: string; AOnSelectItem: TNotifyEvent);
begin
  if FCombo = nil then
  begin
    FCombo := TComboBox.Create(nil);
    FCombo.OnClosePopup := ComboClosePopup;
  end;
  FCombo.OnChange := nil;
  FCombo.TagObject := ARow;
  FCombo.Items.Assign(AItems);
  FCombo.ItemIndex := AItems.IndexOf(ASelected);
  FCombo.Width := 0;
  {$IFDEF MSWINDOWS}
  FCombo.Width := 200;
  {$ENDIF}
  FCombo.OnChange := AOnSelectItem;
  FCombo.Align := TAlignLayout.Center;
  AddObject(FCombo);
  Application.ProcessMessages;
  FCombo.DropDown;
end;
procedure TksListView.SetCheckMarks(const Value: TksListViewCheckMarks);
begin
  if FCheckMarks <> Value then
  begin
    FCheckMarks := Value;
    FItems.UncheckAll;
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

procedure TksListView.SetKsHeaderHeight(const Value: integer);
begin
  BeginUpdate;
  try
    FHeaderHeight := Value;
    ItemAppearance.HeaderHeight := Value;
    RedrawAllRows;
  finally
    EndUpdate;
  end;
  Repaint;
end;

procedure TksListView.SetKsItemHeight(const Value: integer);
begin
  BeginUpdate;
  try
    FItemHeight := Value;
    ItemAppearance.ItemHeight := Value;
    RedrawAllRows;
  finally
    EndUpdate;
  end;
  Repaint;
end;



procedure TksListView.SetItemImageSize(const Value: integer);
begin
  BeginUpdate;
  try
    FItemImageSize := Value;
  finally
    ItemAppearance.ItemHeight := Value;
    EndUpdate;
  end;
  Repaint;
end;

procedure TksListView.SetShowIndicatorColors(const Value: Boolean);
begin
  FShowIndicatorColors := Value;
  RedrawAllRows;
end;

procedure TksListView.SetShowSelection(const Value: Boolean);
begin
  FShowSelection := Value;
  Invalidate;
end;

procedure TksListView.SetupSearchBox;
var
  ICount: integer;
begin
  if FSearchEdit = nil then
  begin
    for ICount := 0 to Children.Count-1 do
    begin
      if Children[ICount] is TSearchBox then
      begin
        FSearchEdit := (Children[ICount] as TSearchBox);
        FSearchEdit.OnKeyUp := DoSearchFilterChanged;
      end;
    end;
  end;
end;

procedure TksListView.ShowPopupMenu(APopup: TPopupMenu; x, y: single);
var
  APoint: TPointF;
begin
  APoint := LocalToAbsolute(TPointF.Create(x, y));
  APoint := Scene.LocalToScreen(APoint);
  APopup.Popup(Round(APoint.X), Round(APoint.Y));
end;
 procedure TksListView.StartScrollTimer;
begin
  if (FPageCaching.Enabled = False) or (FScrollTimer > 0) then
    Exit;
  FScrollTimer := FTimerService.CreateTimer(500, DoScrollTimer);
end;

{
procedure TksListView.ShowRowActionButtons(ARow: TKsListItemRow;
  ASwipeDirection: TksItemSwipeDirection; AButtons: TksListItemRowActionButtons);
var
  ARect: TRectF;
  i: integer;
  AButton: TksListItemRowActionButton;
  ICount: integer;
  AStartX: single;
begin
  ScrollTo(ARow.Index);
  ARect := GetItemRect(ARow.Index);

  if ASwipeDirection = sdRightToLeft then
  begin
    AStartX := ARect.Right;

  end
  else
  begin
    AStartX := ARect.Left - C_DEFAULT_ACTION_BUTTON_WIDTH;
  end;

  for i := 0 to AButtons.Count-1 do
  begin
    AButton := AButtons[i];
    if (ARect.Top < FSearchBoxHeight) and (SearchVisible) then
      ARect.Top := FSearchBoxHeight;
    AButton.FBackground.Height := ARect.Height;

    AButton.FBackground.Position.X := AStartX;
    AButton.FBackground.Position.Y := ARect.Top;
    AButton.FBackground.Width := C_DEFAULT_ACTION_BUTTON_WIDTH;
    AddObject(AButton.FBackground);
  end;
  for ICount := 1 to AButtons.Count do
  begin
    AButton := AButtons[ICount-1];
    case ASwipeDirection of
      sdRightToLeft: TAnimator.AnimateFloat(AButton.FBackground, 'Position.X', (AStartX-(C_DEFAULT_ACTION_BUTTON_WIDTH*ICount)), 0.2);
      sdLeftToRight: TAnimator.AnimateFloat(AButton.FBackground, 'Position.X', (AStartX+(C_DEFAULT_ACTION_BUTTON_WIDTH*ICount)), 0.2);
    end;
  end;
end;  }
      {
procedure TksListView.HideRowActionButtons;
begin
  //for ICount := 0 to FActionButtons.Count-1 do
  //  FActionButtons[ICount].Visible := False;
  FActionButtons.Hide;
end; }
                      {
procedure TksListView.UnlockScrolling;
begin
  FScrollLockPosition := -1;
end;
                       }
function TksListView._Items: TksListViewItems;
begin
  Result := inherited Items;
end;

procedure TKsListItemRows.UncheckAll;
var
  ICount: integer;
begin
  for ICount := 0 to Count-1 do
    Items[ICount].FChecked := False;
  FListView.RedrawAllRows;
end;


  function GetColorFromStyle(const ObjectName: string; const DefaultColor: TAlphaColor): TAlphaColor;
  var
    StyleObject: TFmxObject;
  begin
    StyleObject := FindStyleResource(ObjectName);
    if StyleObject is TColorObject then
      Result := TColorObject(StyleObject).Color
    else if StyleObject is TText then
      Result := TText(StyleObject).Color
    else
      Result := DefaultColor;
  end;

procedure TksListView.ApplyStyle;
begin
  SetColorStyle('background', FAppearence.Background);
  SetColorStyle('itembackground', FAppearence.ItemBackground);
  if FAppearence.SeparatorColor = claNull then
    SetColorStyle('frame', C_DEFAULT_SEPARATOR_COLOR)
  else
    SetColorStyle('frame', FAppearence.SeparatorColor);
  SetColorStyle('alternatingitembackground', FAppearence.AlternatingItemBackground);
  inherited;
end;

procedure TksListView.BeginUpdate;
begin
  inherited;
  Inc(FUpdateCount);
end;

procedure TksListView.DoActionButtonClicked(AButton: TksListItemRowActionButton);
var
  ACanDelete: Boolean;
begin
  if Assigned(FOnItemActionButtonClick) then
  begin
    FOnItemActionButtonClick(Self, AButton.FRow, AButton);
  end;
  if AButton.ButtonType = btDelete then
  begin
    ACanDelete := True;
    if Assigned(OnDeletingItem) then
      OnDeletingItem(Self, AButton.FRow.Index, ACanDelete);
    if ACanDelete then
    begin
      Items.Delete(AButton.FRow.Index);
      if Assigned(OnDeleteItem) then
        OnDeleteItem(Self, AButton.FRow.Index);
    end;
  end;

  FActionButtons.Hide(True);
  //HideRowActionButtons;
  //FActionButtons.Clear;
end;
   {
procedure TksListView.DoAfterShow;
begin
  FTimerService.DestroyTimer(FOnShowTimer);
  RedrawAllRows;
  Invalidate;

end; }

{
procedure TksListView.DoDelaySelect(Sender: TObject);
begin
  FDelaySelect.Enabled := False;
  FDelaySelection := False;
  Repaint;
end;
        }
{procedure TksListView.DoDeselectTimer(Sender: TObject);
begin
  FDeselectTimer.Enabled := False;
  DeselectRow(0);
end; }

{$IFNDEF XE10_OR_NEWER}

procedure TksListView.DoItemChange(const AItem: TListViewItem);
var
  ARow: TKsListItemRow;
begin
  inherited DoItemChange(AItem);
  ARow := Items[AItem.Index];
  ARow.FCached := False;
  ARow.CacheRow;
end;
{$ENDIF}

procedure TksListView.DoOnDeleteItem(Sender: TObject; AIndex: Integer);
begin
  FItems.ReindexRows;
  if Assigned(FOnDeleteItem) then
    FOnDeleteItem(Sender, AIndex);
end;

procedure TksListView.DoRenderRow(ARow: TKsListItemRow);
begin
  FLastRenderedIndex := TListViewItem(ARow.Owner).Index;
end;

procedure TksListView.DoScrollTimer;
var
  AVisibleItems: TksVisibleItems;
  ASearchHeight: single;
begin
  if FUpdating > 0 then
    Exit;
  try
    if FScrolling = False then
    begin
      if ScrollViewPos <> FLastScrollPos then
      begin
        if (FKeepSelection = False) and (ItemIndex > -1) then
          DeselectRow;

        FScrolling := True;
        FLastScrollPos := ScrollViewPos;
        if ScrollViewPos > FLastScrollPos then
          FScrollDirection := sdDown
        else
          FScrollDirection := sdUp;
        Exit;
      end;
    end
    else
    begin

      if FLastScrollPos = ScrollViewPos then
      begin
        CachePages;

        if Assigned(FOnFinishScrolling) then
        begin
          FOnFinishScrolling(Self, AVisibleItems.IndexStart, AVisibleItems.Count);
        end;
        ASearchHeight := GetSearchBoxHeight;
        if ScrollViewPos-ASearchHeight = GetMaxScrollPos then
        begin
          if Assigned(FOnScrollLastItem) then
            FOnScrollLastItem(Self);
        end;
        FScrolling := False;
        //FScrollTimer.Interval := 500;
      end;
    end;
    FLastScrollPos := ScrollViewPos;
  except
    // catch occasional exceptions.
  end;
end;

procedure TksListView.DoSearchFilterChanged(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Assigned(FOnSearchFilterChanged) then
    FOnSearchFilterChanged(Self, FSearchEdit.Text);
end;

procedure TksListView.DoSelectDate(Sender: TObject);
var
  AAllow: Boolean;
  ARow: TKsListItemRow;
begin
  AAllow := True;
  ARow := TKsListItemRow(FDateSelector.TagObject);
  if Assigned(FOnSelectDate) then
    FOnSelectDate(Self, ARow, FDateSelector.Date, AAllow);
  if AAllow then
  begin
    ARow.FSelectionValue := FDateSelector.Date;
    ARow.Detail.Text := FormatDateTime('ddd, dd mmmm, yyyy', FDateSelector.Date);
    ARow.Cached := False;
    ARow.CacheRow;
  end;
end;

procedure TksListView.DoSelectPickerItem(Sender: TObject);
var
  AAllow: Boolean;
  ASelected: string;
  ARow: TKsListItemRow;
begin
  ASelected := '';
  ARow := TKsListItemRow(FCombo.TagObject);
  if FCombo.ItemIndex > -1 then
    ASelected := FCombo.Items[FCombo.ItemIndex];
  AAllow := True;
  if Assigned(FOnSelectPickerItem) then
    FOnSelectPickerItem(Self, ARow, ASelected, AAllow);
  if AAllow then
  begin
    ARow.FSelectionValue := ASelected;
    ARow.Detail.Text := ASelected;
    ARow.Cached := False;
    ARow.CacheRow;
  end;
end;

procedure TksListView.RedrawAllRows;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  BeginUpdate;
  for ICount := 0 to Items.Count-1 do
  begin
    ARow := Items[ICount];
    if ARow <> nil then
    begin
      ARow.Cached := False;
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
    ARow := Items[ICount];
    ARow.ReleaseAllDownButtons;
  end;
end;

procedure TksListView.Resize;
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  if FActionButtons <> nil then
    FActionButtons.Hide(False);
  FWidth := Width;
  RedrawAllRows;
end;

function TksListView.RowObjectAtPoint(ARow: TKsListItemRow; x, y: single): TksListItemRowObj;
var
  ICount: integer;
  AObjRect: TRectF;
  AObj: TksListItemRowObj;
begin
  Result := nil;
  for ICount := ARow.RowObjectCount - 1 downto 0 do
  begin
    AObj := ARow.RowObject[ICount];
    if AObj.HitTest then
    begin
      AObjRect := AObj.Rect;
      InflateRect(AObjRect, 4, 4);
      if (PtInRect(AObjRect, PointF(X, Y))) then
      begin
        Result := AObj;
        Exit;
      end;
    end;
  end;
end;

procedure TksListView.RowOptionSelected(ARow: TKsListItemRow; AIndex: integer;
  ASelected: string);
begin
  if Assigned(FOnOptionSelection) then
    FOnOptionSelection(Self, ARow, AIndex, ASelected);
end;

procedure TksListView.EmbeddedEditChange(ARow: TKsListItemRow; AText: string);
begin
  if Assigned(FOnEmbeddedEditChange) then
    FOnEmbeddedEditChange(Self, ARow, AText);
end;

procedure TksListView.EmbeddedListBoxChange(ARow: TKsListItemRow;
  AItemIndex: integer; ASelected: string);
begin
  if Assigned(FOnEmbeddedListBoxChange) then
    FOnEmbeddedListBoxChange(Self, ARow, AItemIndex, ASelected);
end;

procedure TksListView.EmbeddedSpinBoxChange(ARow: TKsListItemRow;
  AValue: integer);
begin
  if Assigned(FOnEmbeddedSpinBoxChange) then
    FOnEmbeddedSpinBoxChange(Self, ARow, AValue);
end;

procedure TksListView.EmbeddedTrackBarChange(ARow: TKsListItemRow;
  AValue: integer);
begin
  if Assigned(FOnEmbeddedTrackBarChange) then
    FOnEmbeddedTrackBarChange(Self, ARow, AValue);
end;

procedure TksListView.EndUpdate;
begin
  inherited EndUpdate;
  Dec(FUpdateCount);
  if FUpdateCount > 0 then
    Exit;
  if Items.Count = 0 then
    Exit;
  if FIsShowing = False then
    Exit;
  CachePages;
  Invalidate;
end;

function TksListView.GetMaxScrollPos: single;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to Items.Count-1 do
    Result := Result + Items[ICount].Height;
  Result := Result - Height;
end;

function TksListView.GetRowFromYPos(y: single): TKsListItemRow;
var
  ICount: integer;
begin
  Result := nil;
  for Icount := 0 to _Items.Count-1 do
  begin
    if PtInRect(GetItemRect(ICount), PointF(1,y)) then
    begin
      {$IFDEF XE10_OR_NEWER}
      Result := _Items[ICount].Objects.FindDrawable('ksRow') as TKsListItemRow;
      {$ELSE}
      Result := _Items[ICount].Objects.FindObject('ksRow') as TKsListItemRow;
      {$ENDIF}
      Exit;
    end;
  end;
end;


function TksListView.GetSearchBoxHeight: single;
begin
  Result := 0;
  if SearchVisible then
  begin
    if FSearchEdit = nil then
      SetupSearchBox;
    Result := FSearchEdit.Height;
  end;
end;

procedure TksListView.HideEmbeddedControls;
begin
  if FActiveEmbeddedControl <> nil then
  begin
    FActiveEmbeddedControl.HideControl;
    FActiveEmbeddedControl := nil;
  end;
  {
  for ICount := FEmbeddedControls.Count-1 downto 0 do
  begin
    AControl := TksListItemEmbeddedControl(FEmbeddedControls[ICount]);
    //if AControl.ControlActive then
    //  FLastSelectedControl := AControl.Control;
    AControl.HideControl;
  end;
  //FEmbeddedEditControl.Visible := False;
  //FEmbeddedListBoxControl.Visible := False;  }
end;

function TksListView.IsShowing: Boolean;
begin
  FIsShowing := False;
  Repaint;
  Result := FIsShowing;
end;

function TksListView.ItemsInView: TksVisibleItems;
var
  ICount: integer;
  r: TRectF;
  cr: TRectF;
begin
  cr := RectF(0, 0, Width, Height);;
  cr.Top := GetSearchBoxHeight;

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

function TksListView.LoadingBitmap: TBitmap;
begin
  if FLoadingBitmap = nil then
  begin
    FLoadingBitmap := TBitmap.Create;
    FLoadingBitmap.BitmapScale := GetScreenScale;
    FLoadingBitmap.Width := Round(Width * GetScreenScale);
    FLoadingBitmap.Height := Round(FItemHeight * GetScreenScale);
    FLoadingBitmap.Clear(claNull);
    FLoadingBitmap.Canvas.BeginScene;
    FLoadingBitmap.Canvas.StrokeThickness := 4;
    FLoadingBitmap.Canvas.Stroke.Color := $FFDDDDDD;
    FLoadingBitmap.Canvas.DrawRect(RectF(16, 16, 46, 46), 0, 0, AllCorners, 1, FLoadingBitmap.Canvas.Stroke);
    FLoadingBitmap.Canvas.DrawLine(PointF(60, 20), PointF(150, 20), 1);
    FLoadingBitmap.Canvas.DrawLine(PointF(60, 40), PointF(120, 40), 1);
    FLoadingBitmap.Canvas.EndScene;
  end;
  Result := FLoadingBitmap;
end;


procedure TksListView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
var
  ARow: TKsListItemRow;
  ARowRect: TRectF;
begin
  //HideEmbeddedControls;

  if FActionButtons.State = ksActionBtnAnimIn then
    Exit;

  if FActionButtons.State = ksActionBtnVisible then
  begin
    ItemIndex := -1;
    FActionButtons.Hide(True);
    Invalidate;
    Exit;
  end;

  //HideEmbeddedControls;

  inherited;

  ARow := GetRowFromYPos(y);
  if (y < 0) or (ARow = nil) then
    Exit;

  FMouseDownPos := PointF(x-ItemSpaces.Left, y);
  FMouseDownTime := Now;

  if (ARow.CanSelect = False) and (ItemIndex = ARow.Index) then
    DeselectRow;

  ARowRect := GetItemRect(ARow.Index);

  FLastIndex := ItemIndex;
  FClickedRowObj := RowObjectAtPoint(ARow, x, y - ARowRect.Top);
  if FClickedRowObj <> nil then
  begin
    if FClickedRowObj.ConsumesRowClick then
      DeselectRow;
  end;


  if FClickedRowObj <> nil then
  begin
    if FClickedRowObj.ConsumesRowClick then
    begin
      DeselectRow;
      Invalidate;
    end;
    FClickedRowObj.MouseDown;

    {if not (FClickedRowObj is TksListItemEmbeddedControl) then
    begin
      ARow.Cached := False;
      ARow.CacheRow;
    end; }
  end;

  if (Button = TMouseButton.mbRight) and (SelectOnRightClick) then
  begin
    ItemIndex := ARow.Index;
  end;

  if ARow.CanSelect = False then
  begin
    ItemIndex := FLastIndex;
    Invalidate;
  end;
end;

procedure TksListView.MouseMove(Shift: TShiftState; X, Y: Single);
var
  ASwipeDirection: TksItemSwipeDirection;
  ARow: TKsListItemRow;
  AMouseDownTime: integer;
  AMouseDownRow: TKsListItemRow;
begin
  if FActionButtons.State = ksActionBtnAnimIn then
    Exit;

  if FActionButtons.State <> ksActionBtnHidden then
    Exit; // prevent scrolling when action buttons are visible.

  inherited;

  if y < 0 then
    Exit;


  ARow := GetRowFromYPos(Y);


  AMouseDownRow := GetRowFromYPos(FMouseDownPos.Y);
  AMouseDownTime := MilliSecondsBetween(FMouseDownTime, Now);
  if ARow <> nil then
  begin
    if (ssLeft in Shift) and (ARow.Purpose = TListItemPurpose.None) then
    begin
      if ((AMouseDownTime > 0) and (AMouseDownTime < 1000)) and (AMouseDownRow <> nil) and (AMouseDownRow = ARow) then
      begin
        if (x < (FMouseDownPos.X-C_SWIPE_DISTANCE)) or (x > (FMouseDownPos.X+C_SWIPE_DISTANCE)) then
        begin
          ReleaseAllDownButtons;
          //FDisableMouseMove := True; //LockScrolling(ScrollViewPos);
          ASwipeDirection := TksItemSwipeDirection.sdLeftToRight;
          if (x < (FMouseDownPos.X - C_SWIPE_DISTANCE)) then ASwipeDirection := TksItemSwipeDirection.sdRightToLeft;
          if (x > (FMouseDownPos.X + C_SWIPE_DISTANCE)) then ASwipeDirection := TksItemSwipeDirection.sdLeftToRight;
          FActionButtons.Clear;
          FActionButtons.FRow := AMouseDownRow;

          if Assigned(FOnItemSwipe) then
            FOnItemSwipe(Self, AMouseDownRow, ASwipeDirection, FActionButtons);
          if (FDeleteButton.Enabled) and (ASwipeDirection = sdRightToLeft) then
            FActionButtons.AddDeleteButton;
          DeselectRow;

          FActionButtons.InitializeActionButtons(AMouseDownRow, ASwipeDirection);
          Exit;
        end;
      end;
    end;
  end;

  FCurrentMousepos := PointF(x-ItemSpaces.Left, y);

  begin
    if (ssLeft in Shift) then
    begin
      if (FMouseDownPos = PointF(-1, -1)) then
      begin
        FMouseDownPos := FCurrentMousepos;
        FMouseDownTime := Now;
      end;
      if (Y < (FMouseDownPos.Y-40)) or (Y > (FMouseDownPos.Y + 40)) then
        FScrolling := True;
    end;
  end;
  if (ssLeft in Shift) = False  then
  begin
      FMouseDownPos := PointF(-1, -1);
      FMouseDownTime := 0;
  end;
end;


procedure TksListView.MouseUp(Button: TMouseButton; Shift: TShiftState; x,
  y: single);
var
  AId: string;
  ARow: TKsListItemRow;
 // AMouseDownRow: TKsListItemRow;
  AMouseDownRect: TRectF;
  ARowRect: TRectF;
  AMouseDownTime: integer;
  AObjectConsumesClick: Boolean;
begin
  if FActionButtons.State in [ksActionBtnVisible, ksActionBtnAnimIn] then
    Exit;

  //if FActionButtons.State <> ksActionBtnHidden then
  //  Exit; // prevent scrolling when action buttons are visible.

  inherited;
  try
    if y < 0 then
      Exit;

    AObjectConsumesClick := False;

    AMouseDownTime := MilliSecondsBetween(FMouseDownTime, Now);
    AMouseDownRect := RectF(FMouseDownPos.X-8, FMouseDownPos.Y-8, FMouseDownPos.X+8, FMouseDownPos.Y+8);
    x := x - ItemSpaces.Left;

    if FClickedRowObj <> nil then
    begin
      AObjectConsumesClick := (FClickedRowObj.ConsumesRowClick);
      if (AObjectConsumesClick) then
        DeselectRow
      else
        Invalidate;
    end;

    if PtInRect(AMouseDownRect, PointF(x, y)) then
    begin
      // process a mouse click...
      ARow := GetRowFromYPos(y);
      if ARow = nil then
        Exit;

      ARowRect := GetItemRect(ARow.Index);


      AId := ARow.ID;

      if (AMouseDownTime >= 500) and (Assigned(FOnLongClick)) then
      begin
        // long tap...
        FOnLongClick(Self, x, y, ARow, AId, FClickedRowObj);
      end

      else
      begin
        if ARow.CanSelect = False then
          DeselectRow
        else
          Invalidate;
        // remove row selection?
        Application.ProcessMessages;
        ARow.ProcessClick;
        if (ARow.CanSelect) and (AObjectConsumesClick = False) then
        begin
          // left click...
          if (Button = TMouseButton.mbLeft) then
            QueueMouseEvent(ksMouseItemClick, X, Y, AId, ARow, FClickedRowObj);
          // right click...
          if (Assigned(FOnItemRightClick)) and (Button = TMouseButton.mbRight) then
            QueueMouseEvent(ksMouseItemRightClick, X, Y, AId, ARow, FClickedRowObj);

        end;

        if FClickedRowObj <> nil then
        begin
          ARow.CacheRow;
          Invalidate;
          FClickedRowObj.ProcessClick(X - FClickedRowObj.Rect.Left, (Y - FClickedRowObj.Rect.Top) - ARowRect.Top);
          if (FClickedRowObj is TksListItemRowSwitch) then
          begin

            if Assigned(FOnSwitchClicked) then
              FOnSwitchClicked(Self, ARow, (FClickedRowObj as TksListItemRowSwitch), AId);
          end;
          if (FClickedRowObj is TksListItemRowButton) then
          begin
            if Assigned(FOnButtonClicked) then
              FOnButtonClicked(Self, ARow, (FClickedRowObj as TksListItemRowButton), AId);
          end;
          if (FClickedRowObj is TksListItemRowSegmentButtons) then
          begin
            if Assigned(FOnSegmentButtonClicked) then
              FOnSegmentButtonClicked(Self, ARow, (FClickedRowObj as TksListItemRowSegmentButtons), AId);
          end;
        end;
        Invalidate;
      end;
    end
    else
    begin
      // mouse up was after scrolling...
    end;
  finally
    if (FKeepSelection = False) and (ItemIndex > -1) then
    begin
      case FScrolling of
        True: DeselectRow(0);
        False: DeselectRow(200);
      end;
    end;
    ReleaseAllDownButtons;
    //FDisableMouseMove := False;
    FMouseDownPos := PointF(-1, -1);
  end;
end;

procedure TksListView.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  HideEmbeddedControls;
  inherited;
end;

procedure TksListView.Paint;
begin
  if SlideMenuAnimating then
    Exit;

  if FLastPaintPos <> ScrollViewPos then
  begin
    HideEmbeddedControls;
    FLastPaintPos := ScrollViewPos;
  end;

  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if (ScrollViewPos <> FLastScrollPos) and (FActionButtons.State <> ksActionBtnHidden) then
    FActionButtons.Hide(True);

  if FIsShowing = False then
  begin
    RedrawAllRows;
    CachePages;
    SetupSearchBox;
    FIsShowing := True;
  end;
  inherited;
end;

procedure TksListView.ProcessMouseEvents;
var
  AEvent: TksMouseEvent;
begin
  if FProcessingMouseEvents then
    Exit;
  FProcessingMouseEvents := True;
  try
    while FMouseEvents.Count > 0 do
    begin
      AEvent := FMouseEvents[0];
      case AEvent.EventType of
        ksMouseItemClick      : if Assigned(FOnItemClick) then FOnItemClick(Self, AEvent.x, AEvent.y, AEvent.Row, AEvent.ID, AEvent.RowObj);
        ksMouseItemRightClick : if Assigned(FOnItemRightClick) then FOnItemRightClick(Self, AEvent.x, AEvent.y, AEvent.Row, AEvent.ID, AEvent.RowObj);
        ksMouseLongPress      : if Assigned(FOnLongClick) then FOnLongClick(Self, AEvent.x, AEvent.y, AEvent.Row, AEvent.ID, AEvent.RowObj);
      end;
      FMouseEvents.Delete(0);
    end;
  finally
    FProcessingMouseEvents := False;
  end;
end;

procedure TksListView.QueueMouseEvent(AType: TksMouseEventType; X, Y: single;
  AId: string; ARow: TKsListItemRow; AObj: TksListItemRowObj);
var
  AEvent: TksMouseEvent;
  //ATask: ITask;
begin
  {$IFDEF XE10_OR_NEWER}
  if FMouseEventsTimer = 0 then
  FMouseEventsTimer := FTimerService.CreateTimer(250, ProcessMouseEvents);
  {$ENDIF}

  AEvent.EventType := AType;
  AEvent.X := X;
  AEvent.Y := Y;
  AEvent.ID := AId;
  AEvent.Row := ARow;
  AEvent.RowObj := AObj;
  FMouseEvents.Add(AEvent);
  {$IFNDEF XE10_OR_NEWER}
  ProcessMouseEvents;
  {$ENDIF}

  {ATask := TTask.Run(
  procedure
  begin
    TThread.Queue(Nil,
      procedure
      begin
        case AType of
          ksMouseItemClick      : if Assigned(FOnItemClick) then FOnItemClick(Self, x, y, ARow, AId, AObj);
          ksMouseItemRightClick : if Assigned(FOnItemRightClick) then FOnItemRightClick(Self, x, y, ARow, AId, AObj);
          ksMouseLongPress      : if Assigned(FOnLongClick) then FOnLongClick(Self, x, y, ARow, AId, AObj);
        end;
      end
    );
  end);
  TTask.WaitForAny(ATask);}
end;

{ TksListItemRowSwitch }


constructor TksListItemRowSwitch.Create(ARow: TKsListItemRow);
begin
  inherited Create(ARow);
  FActiveColor := C_DEFAULT_ACTIVE_SWITCH_COLOR;
  HitTest := True;
end;

procedure TksListItemRowSwitch.DoClick(x, y: single);
begin
  inherited;
  IsChecked := not IsChecked;
end;

function TksListItemRowSwitch.GetConsumesRowClick: Boolean;
begin
  Result := True;
end;

function TksListItemRowSwitch.Render(ACanvas: TCanvas): Boolean;
begin
  Result := inherited Render(ACanvas);
  DrawSwitch(ACanvas, Rect, FIsChecked, True, FActiveColor);
end;

procedure TksListItemRowSwitch.SetActiveColor(const Value: TAlphaColor);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    Changed;
  end;
end;

procedure TksListItemRowSwitch.SetIsChecked(const Value: Boolean);
begin
  if FIsChecked <> Value then
  begin
    FIsChecked := Value;
    Changed;
  end;
end;

{ TKsListItemRowAccessory }

procedure TKsListItemRowAccessory.CalculateRect(ARowBmp: TBitmap);
var
  AAccessoryImg: TksAccessoryImage;
begin
  AAccessoryImg := AccessoryImages[FAccessoryType];
  Width := AAccessoryImg.Width / Round(GetScreenScale);
  Height := AAccessoryImg.Height / Round(GetScreenScale);
  //Width := 14;
  //Height := 14;
  inherited;
end;

constructor TKsListItemRowAccessory.Create(ARow: TKsListItemRow);
begin
  inherited;
  FAlign := TListItemAlign.Trailing;
  FVertAlignment := TListItemAlign.Center;
  FWidth := 0;
  FHeight := 0;
  {$IFNDEF ANDROID}
  FWidth := FWidth + 8;
  {$ENDIF}
end;

function TKsListItemRowAccessory.Render(ACanvas: TCanvas): Boolean;
var
  ARect: TRectF;
  ABmp: TBitmap;
  APath: TPathData;
  ADestRect: TRectF;
begin
  inherited Render(ACanvas);
  if (FAccessoryType = TksAccessoryType.atCheckmark) and
     (TksListView(FRow.ListView).CheckMarkStyle <> ksCmsDefault)  then
  begin
    ARect := RectF(Rect.Left, Rect.Top, Rect.Left + (64*GetScreenScale), Rect.Top + (64*GetScreenScale));
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
        FreeAndNil(APath);
      end;

      ABmp.Canvas.EndScene;
      ADestRect := Rect;
      ACanvas.DrawBitmap(ABmp,
                         RectF(0, 0, ABmp.Width, ABmp.Height),
                         ADestRect,
                         1);

    finally
      FreeAndNil(ABmp);
    end;
  end
  else
  begin
    AccessoryImages[FAccessoryType].DrawToCanvas(ACanvas, Rect);
    { TODO : draw accessory }
    //DrawAccessory(ACanvas, Rect, C_PLATFORM_ACCESSORY_COLOR, FAccessoryType);
  end;
  Result := True;
end;

procedure TKsListItemRowAccessory.SetAccessoryType(const Value: TksAccessoryType);
begin
  if FAccessoryType <> Value then
  begin
    FAccessoryType := Value;
    Changed;
  end;
end;

{ TksListItemRowSegmentButtons }


procedure TksListItemRowSegmentButtons.DoClick(x, y: single);
var
  ABtnWidth: single;
begin
  inherited;
  ABtnWidth := FWidth / FCaptions.Count;
  ItemIndex := Trunc(x / ABtnWidth);
end;

constructor TksListItemRowSegmentButtons.Create(ARow: TKsListItemRow);
begin
  inherited;
  FCaptions := TStringList.Create;
  FItemIndex := -1;
  HitTest := True;
end;

destructor TksListItemRowSegmentButtons.Destroy;
begin
  FreeAndNil(FCaptions);
  inherited;
end;

function TksListItemRowSegmentButtons.GetConsumesRowClick: Boolean;
begin
  Result := True;
end;

function TksListItemRowSegmentButtons.Render(ACanvas: TCanvas): Boolean;
var
  ABtnWidth: integer;
  ABtnRect: TRectF;
  ICount: integer;
  AStyle: TksButtonStyle;
begin
  inherited Render(ACanvas);
  ABtnWidth := Trunc(FWidth / FCaptions.Count);
  ABtnRect := RectF(Rect.Left, Rect.Top, Rect.Left + ABtnWidth, Rect.Bottom);
  for ICount := 0 to FCaptions.Count-1 do
  begin
    ACanvas.Fill.Color := claRed;
    AStyle := ksButtonSegmentLeft;
    if ICount > 0 then
      AStyle := ksButtonSegmentMiddle;
    if ICount = FCaptions.Count-1 then
      AStyle := ksButtonSegmentRight;


    DrawButton(ACanvas, ABtnRect, FCaptions[ICount], FItemIndex = ICount, FTintColor, AStyle);
    OffsetRect(ABtnRect, ABtnWidth-1, 0);
  end;
  Result := True;
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
  if FTintColor <> Value then
  begin
    FTintColor := Value;
    Changed;
  end;
end;

{ TksListItemRowButton }

constructor TksListItemRowButton.Create(ARow: TKsListItemRow);
begin
  inherited;
  FTintColor := claNull;
  FState := Unpressed;
  HitTest := True;
end;

function TksListItemRowButton.GetConsumesRowClick: Boolean;
begin
  Result := True;
end;

procedure TksListItemRowButton.MouseDown;
begin
  if FHitTest = False then
    Exit;
  inherited;
  if FState <> Pressed then
  begin
    FState := Pressed;
    Changed;
    FRow.CacheRow;
  end;
end;

procedure TksListItemRowButton.MouseUp;
begin
  inherited;
  if FState <> Unpressed then
  begin
    FState := Unpressed;
    Changed;
    FRow.CacheRow;
  end;
end;

function TksListItemRowButton.Render(ACanvas: TCanvas): Boolean;
begin
  inherited Render(ACanvas);
  DrawButton(ACanvas, Rect, FText, FState = Pressed, FTintColor, ksButtonDefault);
  Result := True;
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

{ TksListItemStroke }

procedure TksListItemStroke.Assign(ASource: TPersistent);
begin
  inherited;
  FColor := (ASource as TksListItemStroke).Color;
  FKind := (ASource as TksListItemStroke).Kind;
  FThickness := (ASource as TksListItemStroke).Thickness;
end;

constructor TksListItemStroke.Create;
begin
  FColor := claNull;
  FKind := TBrushKind.Solid;
  FThickness := 1;
end;

procedure TksListItemStroke.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

procedure TksListItemStroke.SetKind(const Value: TBrushKind);
begin
  FKind := Value;
end;

procedure TksListItemStroke.SetThickness(const Value: single);
begin
  FThickness := Value;
end;

{ TksListItemBrush }

procedure TksListItemBrush.Assign(ASource: TPersistent);
begin
  inherited;
  FColor := (ASource as TksListItemBrush).Color;
  FKind := (ASource as TksListItemBrush).Kind;
end;

constructor TksListItemBrush.Create;
begin
  FColor := claNull;
  FKind := TBrushKind.Solid;
end;

procedure TksListItemBrush.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

procedure TksListItemBrush.SetKind(const Value: TBrushKind);
begin
  FKind := Value;
end;

{ TKsListItemRows }

function TKsListItemRows.GetCheckedCount: integer;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to Count-1 do
    if Items[ICount].Checked then
      Result := Result + 1;
end;



function TKsListItemRows.GetCount: integer;
begin
  Result := FListView._Items.Count;
end;

function TKsListItemRows.GetItems(index: integer): TKsListItemRow;
begin
  Result := KsRowFromRow(index);
end;

function TKsListItemRows.KsRowFromRow(AIndex: integer): TKsListItemRow;
begin
  {$IFDEF XE10_OR_NEWER}
  Result := FListView._Items[AIndex].Objects.FindDrawable('ksRow') as TKsListItemRow;
  {$ELSE}
  Result := FListView._Items[AIndex].Objects.FindObject('ksRow') as TKsListItemRow;
  {$ENDIF}
end;

procedure TKsListItemRows.ReindexRows;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  for ICount := 0 to FListView._Items.Count-1 do
  begin
    ARow := KsRowFromRow(Icount);
    ARow.Index := ICount;
  end;
end;

constructor TKsListItemRows.Create(AListView: TksListView; AItems: TksListViewItems);
begin
  inherited Create;
  FListView := AListView;
  FListViewItems := AItems;
end;

procedure TKsListItemRows.Delete(index: integer);
begin
  if (index > -1) and (index <= (Count-1)) then
  begin
    FListView._Items.Delete(index);
    ReindexRows;
  end;
end;

procedure TKsListItemRows.DeleteFirst;
begin
  if Count > 0 then
    Delete(0);
end;

procedure TKsListItemRows.DeleteLast;
begin
  if Count > 0 then
    Delete(Count-1);
end;

procedure TKsListItemRows.DeleteSelected;
begin
  if FListView.ItemIndex > -1 then
    Delete(FListView.ItemIndex);
end;

{ TksListItemRowProgressBar }

constructor TksListItemRowProgressBar.Create(ARow: TKsListItemRow);
begin
  inherited;
  FCornerRadius := 0;
end;

function TksListItemRowProgressBar.Render(ACanvas: TCanvas): Boolean;
var
  ARect: TRectF;
  ABmp: TBitmap;
  ABarRect: TRectF;
begin
  Result := inherited Render(ACanvas);
  ABmp := TBitmap.Create(Round(Rect.Width*GetScreenScale), Round(Rect.Height*GetScreenScale));
  ARect := RectF(0, 0, ABmp.Width, ABmp.Height);
  ABmp.Clear(claNull);
  ABmp.Canvas.BeginScene;
  try
    ABmp.Canvas.Fill.Color := FBorderColor;
    ABmp.Canvas.FillRect(ARect, FCornerRadius*GetScreenScale, FCornerRadius*GetScreenScale, AllCorners, 1);
    InflateRect(ARect, -(2/GetScreenScale), -(2/GetScreenScale));
    ABmp.Canvas.Fill.Color := FBackgroundColor;
    ABmp.Canvas.FillRect(ARect, FCornerRadius*GetScreenScale, FCornerRadius*GetScreenScale, AllCorners, 1);
    ABarRect := ARect;
    ABarRect.Width := (ABarRect.Width / 100) * FProgressPercent;

    ABmp.Canvas.Fill.Color := FBarColor;
    ABmp.Canvas.FillRect(ABarRect, FCornerRadius*GetScreenScale, FCornerRadius*GetScreenScale, [TCorner.TopLeft, TCorner.BottomLeft], 1);

    ABmp.Canvas.Fill.Color := claBlack;
    ABmp.Canvas.Stroke.Color := claBlack;
    ABmp.Canvas.Font.Size := 12 * GetScreenScale;
    ABmp.Canvas.FillText(ARect, IntToStr(FProgressPercent)+'%', False, 1, [], TTextAlign.Center, TTextAlign.Center);

    ABmp.Canvas.EndScene;
    ARect := RectF(0, 0, ABmp.Width, ABmp.Height);
    ACanvas.DrawBitmap(ABmp, ARect, Rect, 1, False);
  finally
    FreeAndNil(ABmp);
  end;
end;

procedure TksListItemRowProgressBar.SetBackgroundColorColor(
  const Value: TAlphaColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TksListItemRowProgressBar.SetBarColor(const Value: TAlphaColor);
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    Changed;
  end;
end;

procedure TksListItemRowProgressBar.SetBorderColor(const Value: TAlphaColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TksListItemRowProgressBar.SetCornerRadius(const Value: single);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Value;
    Changed;
  end;
end;

procedure TksListItemRowProgressBar.SetProgressPercent(const Value: integer);
begin
  if FProgressPercent <> Value then
  begin
    FProgressPercent := Value;
    Changed;
  end;
end;

{ TksListItemRowActionButton }

procedure TksListItemRowActionButton.AddToObject(AObject: TFmxObject);
begin
  AObject.AddObject(FBackground);
end;

constructor TksListItemRowActionButton.Create(AOwner: TksListItemRowActionButtons; AID: string);
begin
  FOwner := AOwner;
  FBackground := TRectangle.Create(nil);
  //FBackground.Fill.Color := claRed;
  FBackground.Stroke.Kind := TBrushKind.None;
  FBackground.HitTest := True;
  FBackground.OnClick := DoClick;
  FLabel := TLabel.Create(FBackground);
  FLabel.Align := TAlignLayout.Client;
  FLabel.StyledSettings := [];
  FLabel.FontColor := C_DEFAULT_ACTION_BUTTON_TEXT_COLOR;
  FWidth := C_DEFAULT_ACTION_BUTTON_WIDTH;
  FLabel.Font.Size := 13;
  FLabel.Text := Text;
  FLabel.TextSettings.HorzAlign := TTextAlign.Center;
  //FLabel.TextAlign := TTextAlign.Center;
  FBackground.AddObject(FLabel);
  FId := AID;
  FVisible := True;
  FButtonType := btCustom;
end;

destructor TksListItemRowActionButton.Destroy;
begin
  {$IFDEF NEXTGEN}
  FLabel.DisposeOf;
  FBackground.DisposeOf;
  {$ELSE}
  FLabel.Free;
  FBackground.Free;
  {$ENDIF}
  inherited;
end;

procedure TKsListItemRowActionButton.DoClick(Sender: TObject);
begin
  FOwner.FListView.DoActionButtonClicked(Self);
end;

function TksListItemRowActionButton.GetColor: TAlphaColor;
begin
  Result := FBackground.Fill.Color;
end;

function TksListItemRowActionButton.GetText: string;
begin
  Result := FLabel.Text;
end;

function TKsListItemRowActionButton.GetTextSettings: TTextSettings;
begin
  Result := FLabel.TextSettings;
end;

procedure TksListItemRowActionButton.SetColor(const Value: TAlphaColor);
begin
  FBackground.Fill.Color := Value;
end;

procedure TksListItemRowActionButton.SetText(const Value: string);
begin
  FLabel.Text := Value;
end;


procedure TKsListItemRowActionButton.SetTextSettings(
  const Value: TTextSettings);
begin
  FLabel.TextSettings.Assign(Value);
end;

procedure TKsListItemRowActionButton.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  FBackground.Visible := Value;
end;

{ TksListItemRowOptionButtons }

function TksListItemRowActionButtons.AddButton(AText: string;
  AColor, ATextColor: TAlphaColor; const AButtonID: string = ''): TksListItemRowActionButton;
begin
  Result := InsertButton(Count, AText, AColor, ATextColor, AButtonID);
end;

procedure TksListItemRowActionButtons.InitializeActionButtons(ARow: TKsListItemRow; ASwipeDirection: TksItemSwipeDirection);
var
  ARect: TRectF;
  ABtn: TksListItemRowActionButton;
  ICount: integer;
begin
  FSwipeDirection := ASwipeDirection;
  ARect := FListView.GetItemRect(ARow.Index);

  for ICount := 0 to Count-1 do
  begin
    if ASwipeDirection = sdRightToLeft then
    begin
      Items[ICount].FOffscreenXPos := ARect.Right+DefaultScrollBarWidth;
    end
    else
    begin
      Items[ICount].FOffscreenXPos := ARect.Left - Items[ICount].Width;
    end;
    Items[ICount].FOnScreenXPos := GetVisibleXPos(Items[ICount], ARect, ASwipeDirection);
    //Items[Icount].Text := floattostr(Items[ICount].FOnScreenXPos);
  end;

  for ICount := 0 to Count-1 do
  begin
    ABtn := Items[ICount];
    if (ARect.Top < FListView.GetSearchBoxHeight) then
      ARect.Top := FListView.GetSearchBoxHeight;
    ABtn.FBackground.Height := ARect.Height;
    ABtn.FLabel.Text := ABtn.Text;
    ABtn.FBackground.Position.X := Items[ICount].FOffscreenXPos;
    ABtn.FBackground.Position.Y := ARect.Top;
    ABtn.FBackground.Width := ABtn.Width;// C_DEFAULT_ACTION_BUTTON_WIDTH;
    FListView.AddObject(ABtn.FBackground);
  end;

  Show;
end;

procedure TksListItemRowActionButtons.AddDeleteButton;
var
 AButton: TksListItemRowActionButton;
 ASettings: TksDeleteButton;
begin
  ASettings := FListView.DeleteButton;
  AButton := InsertButton(0,
                          ASettings.Text,
                          ASettings.Color,
                          ASettings.FTextSettings.FontColor);
  //AButton.TextSettings.HorzAlign := TTextAlign.Center;
  AButton.TextSettings.Assign(ASettings.FTextSettings);
  AButton.FButtonType := btDelete;
end;

constructor TksListItemRowActionButtons.Create(AOwner: TksListView);
begin
  inherited Create(True);
  FListView := AOwner;
  FTimerService := FListView.FTimerService;
  FState := ksActionBtnHidden;
end;

procedure TksListItemRowActionButtons.DoAfterHide;
begin
  FTimerService.DestroyTimer(FHideTimer);
  FState := ksActionBtnHidden;
end;

procedure TksListItemRowActionButtons.DoAfterShow;
begin
  FTimerService.DestroyTimer(FShowTimer);
  FState := ksActionBtnVisible;
end;

function TksListItemRowActionButtons.GetIsAnimating: Boolean;
begin
  Result := FState in [ksActionBtnAnimIn, ksActopmBtnAnimOut];
end;

function TksListItemRowActionButtons.GetVisibleXPos(
  ABtn: TksListItemRowActionButton; ARowRect: TRectF;
  ASwipeDirection: TksItemSwipeDirection): single;
var
  ICount: integer;
begin
  if ASwipeDirection = sdLeftToRight then
  begin
    Result := 0;
    for ICount := 0 to Count-1 do
    begin
      if Items[ICount] = ABtn then
        Exit
      else
        Result := Result + Items[ICount].Width;
    end;
  end
  else
  begin
    Result := ARowRect.Right;
    for ICount := 0 to Count-1 do
    begin
      Result := Result - Items[ICount].Width;
      if Items[ICount] = ABtn then
        Exit
    end;
  end;
end;

procedure TksListItemRowActionButtons.Hide(AAnimate: Boolean);
var
  ICount: integer;
  ABtn: TksListItemRowActionButton;
  //ATask: ITask;
begin
  if AAnimate = False then
  begin
    for ICount := 0 to Count-1 do
      Items[ICount].Visible := False;
    Exit;
  end;

  if (FState <> ksActionBtnVisible) or (Count = 0) then
    Exit;
  FState := ksActopmBtnAnimOut;
  for ICount := 0 to Count-1 do
  begin
    ABtn := Items[ICount];
    TAnimator.AnimateFloat(ABtn.FBackground, 'Position.X', ABtn.FOffscreenXPos, C_ACTION_BTN_ANIMATION_SPEED)
  end;

  while Items[0].FBackground.Position.X <> Items[0].FOnScreenXPos do
  begin
    Application.ProcessMessages;
    Sleep(0);
  end;

  FTimerService.DestroyTimer(FHideTimer);
  FHideTimer := FTimerService.CreateTimer(Round(C_ACTION_BTN_ANIMATION_SPEED*1000), DoAfterHide);

  //AHideTimer :=

  {ATask := TTask.Create (procedure ()
   begin
     sleep (Round(C_ACTION_BTN_ANIMATION_SPEED*1000));
     FState := ksActionBtnHidden;
   end);
   aTask.Start;  }
end;

function TksListItemRowActionButtons.InsertButton(AIndex: integer;
  AText: string; AColor, ATextColor: TAlphaColor; const AButtonID: string = ''): TksListItemRowActionButton;
begin
  Result := TksListItemRowActionButton.Create(Self, AButtonID);
  Result.Text := AText;
  Result.Color := AColor;
  Result.TextSettings.FontColor := ATextColor;
  Result.FRow := FRow;
  Result.FButtonType := btCustom;
  Insert(AIndex, Result);
end;

procedure TksListItemRowActionButtons.Show;
var
  //ATask: ITask;
  ICount: integer;
  ABtn: TksListItemRowActionButton;
begin
  if (FState <> ksActionBtnHidden) or (Count = 0) then
    Exit;
  FState := ksActionBtnAnimIn;
  for ICount := 0 to Count-1 do
  begin
    ABtn := Items[ICount];
    ABtn.Visible := True;
    TAnimator.AnimateFloat(ABtn.FBackground, 'Position.X', (ABtn.FOnScreenXPos), C_ACTION_BTN_ANIMATION_SPEED);
  end;

  FTimerService.DestroyTimer(FShowTimer);
  FShowTimer := FTimerService.CreateTimer(Round(C_ACTION_BTN_ANIMATION_SPEED*1000), DoAfterShow);

 {ATask := TTask.Create (procedure ()
   begin
     sleep (Round(C_ACTION_BTN_ANIMATION_SPEED*1000)); // 3 seconds
     FState := ksActionBtnVisible;
   end);
 aTask.Start;  }
end;


{ TksPageCaching }

constructor TksPageCaching.Create;
begin
  inherited Create;
  FEnabled := False;
  FPageSize := C_DEFAULT_PAGE_SIZE;
end;

{ TksDeleteButton }

constructor TksDeleteButton.Create;
begin
  FTextSettings := TTextSettings.Create(nil);
  FEnabled := True;
  FText := C_DEFAULT_DELETE_BUTTON_TEXT;
  FColor := claRed;
  FTextSettings.FontColor := claWhite;
  FTextSettings.HorzAlign := TTextAlign.Center;
end;

destructor TksDeleteButton.Destroy;
begin
  FreeAndNil(FTextSettings);
  inherited;
end;

//var
//  ASearchBox: TSearchBox;
{ TksListItemRowTableCell }

procedure TksListItemRowTableCell.Changed;
begin
  // need to implement.
end;

constructor TksListItemRowTableCell.Create(ATable: TksListItemRowTable);
begin
  inherited Create;
  FTable := ATable;
  FTextSettings := TTextSettings.Create(nil);
  FFill := TksListItemBrush.Create;
  FStroke := TksListItemStroke.Create;
  FPadding := TBounds.Create(RectF(0, 0, 0, 0));
  FTextSettings.FontColor := claBlack;
  FTextSettings.Font.Family := 'Arial';
  FTextSettings.HorzAlign := TTextAlign.Center;
  FTextSettings.VertAlign := TTextAlign.Center;
  FTextSettings.Font.Size := 12;
  FSides := AllSides;
  FVisible := True;
  {$IFDEF DEBUG}
  //FText := 'CELL';
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

procedure TksListItemRowTableCell.DrawToCanvas(x, y: single; ACanvas: TCanvas;
  ACol, ARow: integer; AShadow: TksListItemTableShadow; AText: Boolean);
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

      ACanvas.Stroke.Color :=  GetColorOrDefault(FStroke.FColor, claDimgray);
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


function TksListItemRowTableCell.GetShadow: TksListItemTableShadow;
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
  FTextSettings := Value;
end;

procedure TksListItemRowTableCell.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Changed;
end;

{ TksListItemRowTable }

procedure TksListItemRowTable.CalculateRect(ARowBmp: TBitmap);
var
  AShadowWidth: integer;
begin
  AShadowWidth := 0;
  if FShadow.Visible then
    AShadowWidth := FShadow.Offset;
  Rect.Width := GetTableSize.cx + AShadowWidth + (4*GetScreenScale);
  Rect.Height := GetTableSize.cy + AShadowWidth + (4*GetScreenScale);
  inherited;
end;

procedure TksListItemRowTable.Clear;
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

constructor TksListItemRowTable.Create(ARow: TKsListItemRow);
begin
  inherited;
  FShadow := TksListItemTableShadow.Create;
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

destructor TksListItemRowTable.Destroy;
begin
  Clear;
  FreeAndNil(FShadow);
  FreeAndNil(FBanding);
  inherited;
end;


function TksListItemRowTable.GetCells(ACol,
  ARow: integer): TksListItemRowTableCell;
begin
  Result := FRows[ARow, ACol];
end;

function TksListItemRowTable.GetColWidths(ACol: integer): single;
begin
  Result := FRows[0, ACol].Width;
end;

function TksListItemRowTable.GetTableSize: TSizeF;
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

procedure TksListItemRowTable.MergeRowCells(x, y, AMergeCount: integer);
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

function TksListItemRowTable.Render(ACanvas: TCanvas): Boolean;
begin
  Result := inherited Render(ACanvas);
  RenderTableContents(ACanvas, False); // render the grid.
  RenderTableContents(ACanvas, True);  // render the cell text
end;


procedure TksListItemRowTable.RenderTableContents(ACanvas: TCanvas; AText: Boolean);
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
    AXPos := Rect.Left;
    AYPos := Rect.Top;

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
      AXpos := Rect.Left;
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
                       RectF(Rect.Left, Rect.Top, Rect.Left+FWidth+FShadow.Offset+1, Rect.Top+FHeight+FShadow.Offset+1), 1, True);
  finally
    FreeAndNil(ABmp);
  end;
end;

procedure TksListItemRowTable.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackground := Value;
end;

procedure TksListItemRowTable.SetBanding(
  const Value: TksListItemRowTableBanding);
begin
  FBanding.Assign(Value);
end;

procedure TksListItemRowTable.SetBorderColor(const Value: TAlphaColor);
begin
  FBorderColor := Value;
end;

procedure TksListItemRowTable.SetColColor(ACol: integer; AColor: TAlphaColor);
var
  ICount: integer;
begin
  for ICount := Low(FRows) to High(FRows) do
    FRows[ICount, ACol].Fill.Color := AColor;
end;

procedure TksListItemRowTable.SetColCount(const Value: integer);
begin
  FColCount := Value;
  ResizeTable;
end;

procedure TksListItemRowTable.SetColFont(ACol: integer; AFontName: TFontName;
  AColor: TAlphaColor; ASize: integer; AStyle: TFontStyles);
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

procedure TksListItemRowTable.SetRowColor(ARow: integer; AColor: TAlphaColor);
var
  ICount: integer;
begin
  for ICount := Low(FRows[ARow]) to High(FRows[ARow]) do
    FRows[ARow, ICount].Fill.Color := AColor;
end;

procedure TksListItemRowTable.SetRowCount(const Value: integer);
begin
  FRowCount := Value;
  ResizeTable;
end;

procedure TksListItemRowTable.SetRowFont(ARow: integer; AFontName: TFontName;
  AColor: TAlphaColor; ASize: integer; AStyle: TFontStyles);
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

procedure TksListItemRowTable.SetColWidths(ACol: integer; const Value: single);
var
  ICount: integer;
begin
  for ICount := Low(FRows) to High(FRows) do
    FRows[ICount, ACol].Width := Value;
  ResizeTable;
end;

procedure TksListItemRowTable.SetDefaultColWidth(const Value: single);
begin
  FDefaultColWidth := Value;
end;

procedure TksListItemRowTable.SetDefaultRowHeight(const Value: single);
begin
  FDefaultRowHeight := Value;
end;

procedure TksListItemRowTable.SetFixedCellColor(const Value: TAlphaColor);
begin
  FFixedCellColor := Value;
end;

procedure TksListItemRowTable.ResizeTable;
var

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
end;

{ TksListItemRowTableShadow }

constructor TksListItemTableShadow.Create;
begin
  FOffset := 3;
  FColor := claSilver;
  FVisible := True;
end;

procedure TksListItemTableShadow.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

{ TksListItemRowTableAlternatatingColors }

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

{ TksListItemEmbeddedTEdit }
 {
function TksListItemEmbeddedTEdit.GetControl: TControl;
begin
  Result := AEmbeddedEditControl; //FRow.ListView.FEmbeddedEditControl;
end;
        }
        {
function TksListItemEmbeddedTEdit.GetEdit: TEdit;
begin
  Result := (GetControl as TEdit);
end;    }

{
procedure TksListItemEmbeddedTEdit.InitializeControl(AClickX, AClickY: single);
begin
  Edit.Text := FText;
  Edit.OnTyping := DoChange;
end;    }

{
procedure TksListItemEmbeddedTEdit.StoreControlProperties;
begin
  FText := Edit.Text;
  Edit.OnTyping := nil;
end;         }

{
procedure TksListItemEmbeddedTEdit.AfterControlFocus;
begin
  inherited;
  Edit.SelStart := Edit.Text.Length;
end;      {

function TksListItemInputBox.Render(ACanvas: TCanvas): Boolean;
var
  ABmp: TBitmap;
  ATextRect: TRectF;
begin
  inherited Render(ACanvas);
  ABmp := TBitmap.Create;
  try
    ABmp.Width := Round(Rect.Width * GetScreenScale);
    ABmp.Height := Round(Rect.Height * GetScreenScale);
    ABmp.Clear(claNull);
    ABmp.Canvas.BeginScene;

    ABmp.Canvas.Stroke.Color := claDimgray;
    ABmp.Canvas.StrokeThickness := 1;
    ABmp.Canvas.DrawRect(RectF(0, 0, ABmp.Width, ABmp.Height), 0, 0, AllCorners, 1);
    Result := True;
    ABmp.Canvas.EndScene;
    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), Rect, 1, True);
    ACanvas.Fill.Color := claBlack;
    ATextRect := Rect;
    InflateRect(ATextRect, -2, 0);
    ACanvas.FillText(ATextRect, FText, False, 1, [], TTextAlign.Leading, TTextAlign.Center);
  finally
    FreeAndNil(ABmp);
  end;
end;

procedure TksListItemInputBox.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;       }

constructor TksListItemOptionSelector.Create(ARow: TKsListItemRow);
begin
  inherited;
  FItems := TStringList.Create;
  FItemHeight := 30;
  FItemIndex := -1;
  HitTest := True;
end;

destructor TksListItemOptionSelector.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;


function TksListItemOptionSelector.Render(ACanvas: TCanvas): Boolean;
var
  //ABmp: TBitmap;
  ICount: integer;
  r: TRectF;
  ACheckRect: TRectF;
  ATextRect: TRectF;
  AYPos: single;
  ABmp: TBitmap;
  ABmpRect: TRectF;
begin
  inherited Render(ACanvas);
  r := RectF(Rect.Left, Rect.Top, Rect.Right, Rect.Top+FItemHeight);
  for ICount := 0 to FItems.Count-1 do
  begin
    if ICount = FItemIndex then
    begin
      ABmp := AccessoryImages.GetAccessoryImage(atRadioButtonChecked);
      ACanvas.Fill.Color := claBlack;
    end
    else
    begin
      ACanvas.Fill.Color := claDimgray;
      ABmp := AccessoryImages.GetAccessoryImage(atRadioButton);
    end;
    //ACanvas.DrawRect(r, 0, 0, AllCorners, 1);
    ABmpRect := RectF(r.Left, r.Top, r.Left+(ABmp.Width / GetScreenScale), r.Top+(ABmp.Height / GetScreenScale));
    OffsetRect(ABmpRect, 0, (r.Height - ABmpRect.Height) / 2);
    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ABmpRect, 1);
    ACanvas.FillText(RectF(r.Left+(ABmp.Width / GetScreenScale)+8, r.Top, r.Right, r.Bottom), FItems[ICount], False, 1, [], TTextAlign.Leading, TTextAlign.Center);
    OffsetRect(r, 0, FItemHeight);
  end;
  Result := True;
end;

procedure TksListItemOptionSelector.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Changed;
  end;
end;

procedure TksListItemOptionSelector.SetItemIndex(const Value: integer);
begin
  if FItemIndex <> Value then
  begin
    FItemIndex := Value;
    Changed;
  end;
end;

function TksListItemOptionSelector.GetConsumesRowClick: Boolean;
begin
  Result := True;
end;

procedure TksListItemOptionSelector.DoClick(x, y: single);
var
  ICount: integer;
  AIndex: integer;
begin
  inherited;
  AIndex := (Round(y) div Round(FItemHeight));
  if (AIndex >= 0) and (AIndex <= FItems.Count-1) then
  begin
    if AIndex <> ItemIndex then
    begin
      ItemIndex := AIndex;
      FRow.ListView.RowOptionSelected(FRow, AIndex, FItems[AIndex]);
    end;
  end;
end;

{ TksAccessoryImageList }

function TksAccessoryImageList.GetAccessoryImage(AAccessory: TksAccessoryType): TksAccessoryImage;
begin
  if Count = 0 then
    Initialize;
   Result := Items[Ord(AAccessory)];
end;

constructor TksAccessoryImageList.Create;
begin
  inherited Create(True);
  FImageMap := TBitmap.Create;
  //Initialize;
end;

destructor TksAccessoryImageList.Destroy;
begin
  FreeAndNil(FImageMap);
  inherited;
end;

function TksAccessoryImageList.GetAccessoryFromResource(
  AStyleName: string; const AState: string = ''): TksAccessoryImage;
var
  ActiveStyle: TFmxObject;
  AStyleObj: TStyleObject;
  AImgRect: TBounds;
  AIds: TStrings;
  r: TRectF;
  ABitmapLink: TBitmapLinks;
  AImageMap: TBitmap;
begin
  Result := TksAccessoryImage.Create;
  AIds := TStringList.Create;
  try
    AIds.Text := StringReplace(AStyleName, '.', #13, [rfReplaceAll]);
    ActiveStyle := TStyleManager.ActiveStyle(Nil);

    AStyleObj := TStyleObject(ActiveStyle);

    while AIds.Count > 0 do
    begin
      AStyleObj := TStyleOBject(AStyleObj.FindStyleResource(AIds[0]));
      AIds.Delete(0);
    end;

    if AStyleObj <> nil then
    begin
      if FImageMap.IsEmpty then
      begin
        AImageMap := ((AStyleObj as TStyleObject).Source.MultiResBitmap.Bitmaps[Round(GetScreenScale)]);

        FImageMap.SetSize(Round(AImageMap.Width), Round(AImageMap.Height));
        FImageMap.Clear(claNull);

        FImageMap.Canvas.BeginScene;
        try
          FImageMap.Canvas.DrawBitmap(AImageMap,
                                      RectF(0, 0, AImageMap.Width, AImageMap.Height),
                                      RectF(0, 0, FImageMap.Width, FImageMap.Height),
                                      1, True);
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
      AImgRect := ABitmapLink.LinkByScale(Round(GetScreenScale), True).SourceRect;
      {$ELSE}
      AImgRect := ABitmapLink.LinkByScale(Round(GetScreenScale)).SourceRect;
      {$ENDIF}

      Result.SetSize(Round(AImgRect.Width), Round(AImgRect.Height));
      Result.Clear(claNull);
      Result.Canvas.BeginScene;

      r := AImgRect.Rect;




      Result.Canvas.DrawBitmap(FImageMap,
                               r,
                               RectF(0, 0, Result.Width, Result.Height),
                               1,
                               True);
      Result.Canvas.EndScene;

      {FImageMap.Canvas.BeginScene;
      FImageMap.Canvas.Stroke.Color := claRed;
      FImageMap.Canvas.DrawRect(r,0, 0, AllCorners, 1);
      FImageMap.Canvas.EndScene;    }
    end;
  finally
    {$IFDEF NEXTGEN}
    FreeAndNil(AIds);
    {$ELSE}
    AIds.Free;
    {$ENDIF}
  end;
end;

procedure TksAccessoryImageList.Initialize;
var
  ICount: TksAccessoryType;
begin
  for ICount := Low(TksAccessoryType) to High(TksAccessoryType) do
  begin
    case ICount of
      atNone              : Add(GetAccessoryFromResource('none'));
      atMore              : Add(GetAccessoryFromResource('listviewstyle.accessorymore'));
      atCheckmark         : Add(GetAccessoryFromResource('listviewstyle.accessorycheckmark'));
      atDetail            : Add(GetAccessoryFromResource('listviewstyle.accessorydetail'));
      atBack              : Add(GetAccessoryFromResource('backtoolbutton.icon'));
      atRefresh           : Add(GetAccessoryFromResource('refreshtoolbutton.icon'));
      atAction            : Add(GetAccessoryFromResource('actiontoolbutton.icon'));
      atPlay              : Add(GetAccessoryFromResource('playtoolbutton.icon'));
      atRewind            : Add(GetAccessoryFromResource('rewindtoolbutton.icon'));
      atForward           : Add(GetAccessoryFromResource('forwardtoolbutton.icon'));
      atPause             : Add(GetAccessoryFromResource('pausetoolbutton.icon'));
      atStop              : Add(GetAccessoryFromResource('stoptoolbutton.icon'));
      atAdd               : Add(GetAccessoryFromResource('addtoolbutton.icon'));
      atPrior             : Add(GetAccessoryFromResource('priortoolbutton.icon'));
      atNext              : Add(GetAccessoryFromResource('nexttoolbutton.icon'));
      atArrowUp           : Add(GetAccessoryFromResource('arrowuptoolbutton.icon'));
      atArrowDown         : Add(GetAccessoryFromResource('arrowdowntoolbutton.icon'));
      atArrowLeft         : Add(GetAccessoryFromResource('arrowlefttoolbutton.icon'));
      atArrowRight        : Add(GetAccessoryFromResource('arrowrighttoolbutton.icon'));
      atReply             : Add(GetAccessoryFromResource('replytoolbutton.icon'));
      atSearch            : Add(GetAccessoryFromResource('searchtoolbutton.icon'));
      atBookmarks         : Add(GetAccessoryFromResource('bookmarkstoolbutton.icon'));
      atTrash             : Add(GetAccessoryFromResource('trashtoolbutton.icon'));
      atOrganize          : Add(GetAccessoryFromResource('organizetoolbutton.icon'));
      atCamera            : Add(GetAccessoryFromResource('cameratoolbutton.icon'));
      atCompose           : Add(GetAccessoryFromResource('composetoolbutton.icon'));
      atInfo              : Add(GetAccessoryFromResource('infotoolbutton.icon'));
      atPagecurl          : Add(GetAccessoryFromResource('pagecurltoolbutton.icon'));
      atDetails           : Add(GetAccessoryFromResource('detailstoolbutton.icon'));
      atRadioButton       : Add(GetAccessoryFromResource('radiobuttonstyle.background'));
      atRadioButtonChecked: Add(GetAccessoryFromResource('radiobuttonstyle.background', 'checked'));
      atCheckBox          : Add(GetAccessoryFromResource('checkboxstyle.background'));
      atCheckBoxChecked   : Add(GetAccessoryFromResource('checkboxstyle.background', 'checked'));
      atUserDefined1      : Add(GetAccessoryFromResource('userdefined1'));
      atUserDefined2      : Add(GetAccessoryFromResource('userdefined2'));
      atUserDefined3      : Add(GetAccessoryFromResource('userdefined3'));
    end;
  end;
end;

{ TksAccessoryImage }


procedure TksAccessoryImage.DrawToCanvas(ACanvas: TCanvas; ADestRect: TRectF);
begin
  ACanvas.DrawBitmap(Self,
                     RectF(0, 0, Width, Height),
                     ADestRect,
                     1,
                     True);
end;

procedure TksAccessoryImage.SetBitmap(ASource: TBitmap);
begin
  Assign(ASource);
end;

{ TksListItemEmbeddedControl }

procedure TksListItemEmbeddedControl.ChangeSize;
begin
  inherited;
  FControl.Width := FWidth;
  FControl.Height := FHeight;
end;

constructor TksListItemEmbeddedControl.Create(ARow: TKsListItemRow);
begin
  inherited;
  FCached := TBitmap.Create;
  //FCached.BitmapScale := GetScreenScale;
  FControl := CreateControl;
  FFocused := False;
  HitTest := True;
  FHeight := FControl.Height;
end;

destructor TksListItemEmbeddedControl.Destroy;
begin
  FreeAndNil(FCached);
  {$IFDEF NEXTGEN}
  FControl.DisposeOf;
  {$ELSE}
  FControl.Free;
  {$ENDIF}
  inherited;
end;

procedure TksListItemEmbeddedControl.DoClick(x, y: single);
begin
  inherited;
  if FFocused = False then
    ShowControl;
  SimulateClick(x, y);
end;

function TksListItemEmbeddedControl.GetConsumesRowClick: Boolean;
begin
  Result := True;
end;

function TksListItemEmbeddedControl.GetControlBitmap: Boolean;
var
  ABmp: TBitmap;
begin
  FControl.Position.X := -1000;
  if FControl.Parent = nil then
    FControl.Parent := FRow.ListView;

  InitializeControl;
  FCached.SetSize(Round(FWidth * GetScreenScale), Round(FHeight * GetScreenScale));
  FCached.Clear(claNull);

  ABmp := FControl.MakeScreenshot;
  FCached.Assign(ABmp);
  ABmp.DisposeOf;

  Result := True;
  FControl.Parent := nil;
end;

procedure TksListItemEmbeddedControl.HideControl;
begin
  FRow.ListView.SetFocus;
  GetControlBitmap;
  FFocused := False;
  FRow.Changed;
  FRow.ListView.Invalidate;
  FControl.Parent := nil;
end;

procedure TksListItemEmbeddedControl.InitializeControl;

  procedure ApplyStyle(AControl: TFmxObject);
  var
    ICount: integer;
  begin
    if (AControl is TStyledControl) then
    begin
      (AControl as TStyledControl).RecalcSize;
      (AControl as TStyledControl).UpdateEffects;
      (AControl as TStyledControl).ApplyStyleLookup;

    end;
    for ICount := 0 to AControl.ChildrenCount-1 do
      ApplyStyle(AControl.Children[ICount]);
  end;

begin
  FControl.Width := Rect.Width;
  FControl.Height := Rect.Height;
  FControl.RecalcSize;
  ApplyStyle(FControl);
  FControl.UpdateEffects;
end;


function TksListItemEmbeddedControl.Render(ACanvas: TCanvas): Boolean;
begin
  if FFocused = True then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
  if GetControlBitmap then
  begin
    ACanvas.DrawBitmap(FCached, RectF(0, 0, FCached.Width, FCached.Height), Rect, 1, True);

    Result := True;
    FRow.ListView.Invalidate;
  end;
end;

procedure TksListItemEmbeddedControl.ShowControl;
begin
  FRow.ListView.HideEmbeddedControls;

  FControl.SetBounds( Rect.Left, FRow.LocalRect.Top + Rect.Top,Rect.Width, Rect.Height);
  FControl.Parent := FRow.ListView;

  FControl.BringToFront;
  FRow.ListView.FActiveEmbeddedControl := Self;
  FFocused := True;
  FControl.SetFocus;
  FRow.Changed;
end;

procedure TksListItemEmbeddedControl.SimulateClick(x, y: single);
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

{ TksListItemEmbeddedListBox }

function TksListItemEmbeddedListBox.CreateControl: TStyledControl;
begin
  Result := TListView.Create(nil);
  (Result as TListView).OnItemClick := DoSelect;
end;

procedure TksListItemEmbeddedListBox.DoSelect(const Sender: TObject; const AItem: TListViewItem);
var
  ASelected: string;
begin
  ASelected := '';
  if GetListBox.ItemIndex > -1 then
    ASelected := TListViewItem(GetListBox.Selected).Text;
  FRow.ListView.EmbeddedListBoxChange(FRow, GetListBox.ItemIndex, ASelected);
end;

function TksListItemEmbeddedListBox.GetListBox: TListView;
begin
  Result := (FControl as TListView);
end;

{ TksListItemEmbeddedEdit }

function TksListItemEmbeddedEdit.CreateControl: TStyledControl;
begin
  Result := TEdit.Create(nil);
  FHeight := Result.Height;
  (Result as TEdit).OnTyping := DoTyping;
end;

function TksListItemEmbeddedEdit.GetEdit: TEdit;
begin
  Result := (FControl as TEdit);
end;

procedure TksListItemEmbeddedEdit.DoTyping(Sender: TObject);
begin
  FRow.ListView.EmbeddedEditChange(FRow, GetEdit.Text);
end;

{ TksListItemEmbeddedTrackBar }


function TksListItemEmbeddedTrackBar.CreateControl: TStyledControl;
begin
  Result := TTrackBar.Create(FRow.ListView);
  (Result as TTrackBar).OnChange := DoChange;
end;

procedure TksListItemEmbeddedTrackBar.DoChange(Sender: TObject);
begin
  FRow.ListView.EmbeddedTrackBarChange(FRow, Round(GetTrackBar.Value));
end;

function TksListItemEmbeddedTrackBar.GetTrackBar: TTrackBar;
begin
  Result := (FControl as TTrackBar);
end;

{ TksListItemEmbeddedSpinBox }

function TksListItemEmbeddedSpinBox.CreateControl: TStyledControl;
begin
  Result := TSpinBox.Create(nil);
  (Result as TSpinBox).OnChange := DoChange;
end;

procedure TksListItemEmbeddedSpinBox.DoChange(Sender: TObject);
begin
  FRow.ListView.EmbeddedSpinBoxChange(FRow, Round(GetSpinBox.Value));
end;

function TksListItemEmbeddedSpinBox.GetSpinBox: TSpinBox;
begin
  Result := (FControl as TSpinBox);
end;

{ TksListItemObjects }

function TksListItemObjects.ObjectByID(AId: string): TksListItemRowObj;
var
  ICount: integer;
begin
  Result := nil;
  if AId = '' then
    Exit;
  for ICount := 0 to Count-1 do
  begin
    if Items[ICount].ID = AId  then
    begin
      Result := Items[ICount];
      Exit;
    end;
  end;
end;

{ TksListItemEmbeddedMemo }

function TksListItemEmbeddedMemo.CreateControl: TStyledControl;
begin
  Result := TMemo.Create(nil);
  (Result as TMemo).WordWrap := True;
  (Result as TMemo).OnChange := DoTyping;
end;

procedure TksListItemEmbeddedMemo.DoTyping(Sender: TObject);
begin
  FRow.ListView.EmbeddedEditChange(FRow, GetMemo.Text);
end;

function TksListItemEmbeddedMemo.GetMemo: TMemo;
begin
  Result := (FControl as TMemo);
end;

initialization

  ATextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  AccessoryImages := TksAccessoryImageList.Create;

  {$IFDEF MSWINDOWS}
  DefaultScrollBarWidth := 16;
  {$ENDIF}


finalization

  FreeAndNil(ATextLayout);
  FreeAndNil(AccessoryImages);


end.
