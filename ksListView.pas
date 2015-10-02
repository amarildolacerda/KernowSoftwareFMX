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
  {$IFDEF XE8_OR_NEWER} FMX.ImgList, {$ENDIF}
  System.UIConsts, FMX.StdCtrls, FMX.Styles.Objects, System.Generics.Collections,
  FMX.ListBox, FMX.DateTimeCtrls, FMX.Menus, FMX.Objects
  {$IFDEF XE10_OR_NEWER}, FMX.ListView.Appearances {$ENDIF}

  ;

const
  C_LONG_TAP_DURATION     = 5;  // 500 ms
  C_BUTTON_HEIGHT = 29;
  {$IFDEF ANDROID}
  C_DEFAULT_ACTIVE_SWITCH_COLOR = claDodgerBlue;
  C_SEGMENT_BUTTON_HEIGHT = 30;
  {$ELSE}
  C_DEFAULT_ACTIVE_SWITCH_COLOR = claLime;
  C_SEGMENT_BUTTON_HEIGHT = 30;
  {$ENDIF}
  C_SWIPE_DISTANCE = 60;

  C_DEFAULT_TEXT_COLOR = claBlack;
  C_DEFAULT_HEADER_TEXT_COLOR = claBlack;
  C_DEFAULT_SEGMENT_BUTTON_COLOR = claNull;

  C_TITLE    = 'TITLE';
  C_SUBTITLE = 'SUBTITLE';
  C_DETAIL   = 'DETAIL';

  C_DEFAULT_PAGE_SIZE = 100;

  C_LEFT_MARGIN = 10;
  C_DEFAULT_SEPARATOR_COLOR = $FFE0E0E0;
  C_PLATFORM_ACCESSORY_COLOR = claSilver;

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

  TksListViewCheckMarks = (ksCmNone, ksCmSingleSelect, ksCmMultiSelect);
  TksListViewCheckStyle = (ksCmsDefault, ksCmsRed, ksCmsGreen, ksCmsBlue);
  TksListViewShape = (ksRectangle, ksRoundRect, ksEllipse);
  TksItemImageShape = (ksRectangleImage, ksRoundRectImage, ksCircleImage);
  TksAccessoryType = (None, More, Checkmark, Detail);
  TksImageButtonStyle = (Action, Add, Camara, Compose, Information, ArrowLeft,
    ArrowDown, ArrowRight, ArrowUp, Delete, Details, Organise, PageCurl, Pause,
    Play, Refresh, Reply, Search, Stop, Trash);
  TksButtonState = (Pressed, Unpressed);
  TksListItemRowSelector = (NoSelector, DateSelector, ItemPicker);
  TksScrollDirection = (sdUp, sdDown);
  TksItemSwipeDirection = (sdLeftToRight, sdRightToLeft);

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
  // ------------------------------------------------------------------------------

  TksVisibleItems = record
    Count: integer;
    IndexStart: integer;
    IndexEnd: integer;
  end;



  TksListItemRowObj = class(TPersistent)
  strict private
    FRect: TRectF;
  private
    FId: string;
    FPlaceOffset: TPointF;
    FRow: TKsListItemRow;
    FAlign: TListItemAlign;
    FVertAlignment: TListItemAlign;
    FTagBoolean: Boolean;
    FGuid: string;
    FWidth: single;
    FHeight: single;
    FConumesRowClick: Boolean;
    FEnabled: Boolean;
    procedure SetRect(const Value: TRectF);
    procedure SetID(const Value: string);
    procedure Changed;
    procedure SetAlign(const Value: TListItemAlign);
    procedure SetVertAlign(const Value: TListItemAlign);
    procedure SetHeight(const Value: single);
    procedure SetWidth(const Value: single);
    function GetOffsetX: single;
    function GetOffsetY: single;
    procedure SetOffsetX(const Value: single);
    procedure SetOffsetY(const Value: single);
    procedure SetEnabled(const Value: Boolean);
  protected
    function GetConsumesRowClick: Boolean; virtual;
    procedure CalculateRect(ARowBmp: TBitmap); virtual;
    procedure DoChanged(Sender: TObject);
  public
    constructor Create(ARow: TKsListItemRow); virtual;
    procedure Assign(ASource: TPersistent); override;
    function Render(ACanvas: TCanvas): Boolean; virtual;
    procedure MouseDown; virtual;
    procedure MouseUp; virtual;
    procedure ProcessClick(x, y: single);
    procedure DoClick(x, y: single); virtual;
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
    property Enabled: Boolean read FEnabled write SetEnabled default True;
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
    procedure SetFont(const Value: TFont);
    procedure SetAlignment(const Value: TTextAlign);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetText(const Value: string);
    procedure SetWordWrap(const Value: Boolean);
    function CalculateTextHeight: single;
    procedure SetTextLayout(const Value: TTextAlign);
    procedure SetBackground(const Value: TAlphaColor);
  protected
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
    FAccessoryType: TAccessoryType;
    procedure SetAccessoryType(const Value: TAccessoryType);
  protected
    procedure CalculateRect(ARowBmp: TBitmap); override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property AccessoryType: TAccessoryType read FAccessoryType write SetAccessoryType;
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

  TksListItemObjects = class(TObjectList<TksListItemRowObj>);

  TKsListItemRowActionButton = class
  private
    FOwner: TksListItemRowActionButtons;
    FBackground: TRectangle;
    FLabel: TLabel;
    FRow: TksListItemRow;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure DoClick(Sender: TObject);
  public
    constructor Create(AOwner: TksListItemRowActionButtons);
    destructor Destroy; override;
    procedure AddToObject(AObject: TFmxObject);
    property Color: TAlphaColor read GetColor write SetColor;
    property Text: string read GetText write SetText;
  end;

  TksListItemRowActionButtons = class(TObjectList<TksListItemRowActionButton>)
  private
    FListView: TksListView;
    FRow: TksListItemRow;
  public
    constructor Create(AOwner: TksListView);
    procedure AddButton(AText: string; AColor: TAlphaColor);
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
    function RowHeight(const AScale: Boolean = True): single;
    function RowWidth(const AScale: Boolean = True): single;
    function GetListView: TksListView;
    function GetRowObject(AIndex: integer): TksListItemRowObj;
    function GetRowObjectCount: integer;
    procedure SetAccessory(const Value: TAccessoryType);
    procedure SetShowAccessory(const Value: Boolean);
    function GetAccessory: TAccessoryType;
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
    property ListView: TksListView read GetListView;
    procedure DoOnListChanged(Sender: TObject; const Item: TksListItemRowObj; Action: TCollectionNotification);
    function ScreenWidth: single;
    procedure ProcessClick;
    procedure Changed;
    procedure ReleaseAllDownButtons;
    function TextWidth(AText: string): single;
    function TextHeight(AText: string; AWordWrap: Boolean; const AWidth: single = 0): single;
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
    // text functions...
    function TextOut(AText: string; x: single; const AVertAlign: TListItemAlign = TListItemAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, AWidth: single; const AVertAlign: TListItemAlign = TListItemAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, y, AWidth: single; const AVertAlign: TListItemAlign = TListItemAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextBox(AText: string; ARect: TRectF; ATextAlign: TTextAlign; ATextLayout: TTextAlign; const ABackground: TAlphaColor = claNull): TksListItemRowText; overload;
    function TextOutRight(AText: string; y, AWidth: single; AXOffset: single; const AVertAlign: TListItemAlign = TListItemAlign.Center): TksListItemRowText; overload;
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
    property Accessory: TAccessoryType read GetAccessory write SetAccessory;
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
  end;


  TKsListItemRows = class
  private
    FListView: TksListView;
    {$IFDEF XE10_OR_NEWER}
    FListViewItems: TAppearanceListViewItems;
    {$ELSE}
    FListViewItems: TListViewItems;
    {$ENDIF}
    function GetCheckedCount: integer;
    function GetCount: integer;
    function GetItems(index: integer): TKsListItemRow;
    procedure ReindexRows;
    function KsRowFromRow(AIndex: integer): TKsListItemRow;
  public
    constructor Create(AListView: TksListView; AItems: TksListViewItems) ; virtual;

    function AddRow(AText: string; const AAccessoryType: TksAccessoryType = None): TKsListItemRow; overload;
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
    FListView: TksListView;
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
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property PageSize: integer read FPageSize write FPageSize default C_DEFAULT_PAGE_SIZE;
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
    //FClickTimer: TTimer;
    FLastWidth: integer;
    FOnLongClick: TksListViewRowClickEvent;
    FClickedRowObj: TksListItemRowObj;
    FSelectOnRightClick: Boolean;
    FOnSwitchClicked: TksListViewClickSwitchEvent;
    FOnButtonClicked: TksListViewClickButtonEvent;
    FOnSegmentButtonClicked: TksListViewClickSegmentButtonEvent;
    FScrollTimer: TTimer;
    FLastScrollPos: Extended;
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
    FCanSwipeDelete: Boolean;
    FActionButtons: TksListItemRowActionButtons;
    FOnItemActionButtonClick: TksItemActionButtonClickEvent;
    FSearchBoxHeight: single;
    FDisableMouseMove: Boolean;
    FPageCaching: TksPageCaching;
    function _Items: TksListViewItems;

    procedure DoScrollTimer(Sender: TObject);
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
    procedure CachePages;
    function LoadingBitmap: TBitmap;
    procedure CalculateSearchBoxHeight;
    procedure DeselectRow;
    { Private declarations }
  protected
    procedure SetColorStyle(AName: string; AColor: TAlphaColor);
    procedure ApplyStyle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure ShowRowOptionButtons(ARow: TKsListItemRow;
                                   ASwipeDirection: TksItemSwipeDirection;
                                   AButtons: TksListItemRowActionButtons);
    {$IFNDEF XE10_OR_NEWER}
    procedure DoItemChange(const AItem: TListViewItem); override;
    {$ENDIF}
    function GetRowFromYPos(y: single): TKsListItemRow;
    procedure SetKsItemHeight(const Value: integer);
    procedure SetKsHeaderHeight(const Value: integer);
    function GetMaxScrollPos: single;
    procedure DoActionButtonClicked(AButton: TksListItemRowActionButton);
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
    property ShowIndicatorColors: Boolean read FShowIndicatorColors write SetShowIndicatorColors default False;
    property TabOrder;
    property TabStop;
    property Visible default True;
    property Width;
    property OnSelectDate: TksListViewSelectDateEvent read FOnSelectDate write FOnSelectDate;
    property OnSelectPickerItem: TksListViewSelectPickerItem read FOnSelectPickerItem write FOnSelectPickerItem;
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
    property DeleteButtonText;

    property AutoTapScroll;
    property AutoTapTreshold;
    property ShowSelection;
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

implementation

uses SysUtils, FMX.Platform, FMX.SearchBox, ksDrawFunctions, FMX.Ani,
  System.StrUtils, DateUtils, FMX.Forms, Math, ksSlideMenu;

var
  DefaultScrollBarWidth: integer = 7;

  ATextLayout: TTextLayout;


procedure Register;
begin
  RegisterComponents('kernow Software FMX', [TksListView]);
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

  if FAlign = TListItemAlign.Trailing then
  begin
    OffsetRect(FRect, ABmpWidth - (4 + w+ DefaultScrollBarWidth + FPlaceOffset.X {+ FRow.ListView.ItemSpaces.Right}), 0);
    if (Self is TKsListItemRowAccessory) = False then
    begin
      if FRow.ShowAccessory then
        OffsetRect(FRect, 0-FRow.FAccessory.Width, 0);
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
  FRow.Cached := False;
end;

procedure TksListItemRowObj.ProcessClick(x, y: single);
begin
  if FEnabled then
    DoClick(x, y);
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
  FEnabled := True;
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

procedure TksListItemRowObj.SetEnabled(const Value: Boolean);
begin
 if FEnabled <> Value then
 begin
   FEnabled := Value;
   Changed;
 end;
end;

procedure TksListItemRowObj.SetHeight(const Value: single);
begin
  FHeight := Value;
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
    if FRow.ListView.ShowIndicatorColors then
      Rect.Offset(16, 0);
  end;
end;

function TksListItemRowText.CalculateTextHeight: single;
var
  APoint: TPointF;
begin
  ATextLayout.BeginUpdate;

  // Setting the layout MaxSize
  APoint.X := FWidth;
  if FWidth = 0 then
  begin
    if FId = C_TITLE then APoint.X := FRow.ListView.Width / 2;
    if FId = C_SUBTITLE then APoint.X := FRow.ListView.Width / 2;
    if FId = C_DETAIL then APoint.X := FRow.ListView.Width / 2;
  end;

  APoint.Y := 1000;
  ATextLayout.MaxSize := aPoint;

  ATextLayout.Text := FText;
  ATextLayout.WordWrap := FWordWrap;
  ATextLayout.Font := FFont;
  ATextLayout.HorizontalAlign := FAlignment;
  ATextLayout.EndUpdate;
  Result := ATextLayout.Height;

end;

constructor TksListItemRowText.Create(ARow: TKsListItemRow);
begin
  inherited Create(ARow);
  FFont := TFont.Create;
  FTextColor := C_DEFAULT_TEXT_COLOR;
  FWordWrap := False;
  FFullWidth := False;
  VertAlign := TListItemAlign.Center;
end;

destructor TksListItemRowText.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

function TksListItemRowText.Render(ACanvas: TCanvas): Boolean;
var
  APoint: TPointF;
begin
  inherited Render(ACanvas);
  ATextLayout.BeginUpdate;


  // Setting the layout MaxSize
  APoint.X := FWidth;
  if FWidth = 0 then
  begin
    if FId = C_TITLE then APoint.X := FRow.ListView.Width / 2;
    if FId = C_SUBTITLE then APoint.X := FRow.ListView.Width / 2;
    if FId = C_DETAIL then APoint.X := FRow.ListView.Width / 2;
  end;

  APoint.Y := 1000;
  ATextLayout.MaxSize := aPoint;

  ATextLayout.Text := FText;
  ATextLayout.WordWrap := FWordWrap;
  ATextLayout.Font := FFont;
  ATextLayout.Color := FTextColor;
  ATextLayout.HorizontalAlign := FAlignment;
  ATextLayout.VerticalAlign := FTextLayout;

  ATextLayout.EndUpdate;

  ATextLayout.Trimming := TTextTrimming.Character;
  ATextLayout.TopLeft := Rect.TopLeft;
  ATextLayout.MaxSize := PointF(Rect.Width, Rect.Height);

  if FBackground <> claNull then
  begin
    ACanvas.Fill.Color := FBackground;
    ACanvas.FillRect(Rect, 0, 0, AllCorners, 1);
  end;
  ATextLayout.RenderLayout(ACanvas);
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
    {$IFDEF NEXTGEN}
    ARectangle.DisposeOf;
    {$ELSE}
    ARectangle.Free;
    {$ENDIF}
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
begin
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

    if (FIndicatorColor <> claNull) and (lv.ShowIndicatorColors) then
    begin
      Bitmap.Canvas.Fill.Color := FIndicatorColor;
      Bitmap.Canvas.FillRect(RectF(8, 8, 14, RowHeight(False)-8), 0, 0, [], 1, Bitmap.Canvas.Fill);
    end;

    {$IFDEF XE8_OR_NEWER}
    if FImageIndex > -1 then
    begin
      ASize.cx := 32;
      ASize.cy := 32;
      AImage := lv.Images.Bitmap(ASize, FImageIndex);
      FImage.Bitmap.Assign(AImage);
    end;
    {$ENDIF}

    if FAutoCheck then
    begin
      FAccessory.AccessoryType := TAccessoryType.Checkmark;
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
    Bitmap.Canvas.EndScene;
    FCached := True;
  finally
    EndUpdate;
  end;
end;

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
end;

function TksListItemRow.TextHeight(AText: string; AWordWrap: Boolean; const AWidth: single = 0): single;
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

procedure TKsListItemRow.Changed;
begin
  if FUpdating then
    Exit;
  FCached := False;
  if not ListView.IsUpdating then
  begin
    CacheRow;
    ListView.Repaint;
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
    Bitmap := ListView.LoadingBitmap;
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
begin
  if FLastHeight = 0 then
    FLastHeight := Height;

  if ListView.FUpdateCount > 0 then
    Exit;

  {if Height <> FLastHeight then
  begin
    FCached := False;
    CacheRow;
    FLastHeight := Height;
  end; }

  ANextItemIsHeader := False;
  if Index < ListView.Items.Count-1 then
  begin
    ANextItem := ListView.Items[Index+1];
    ANextItemIsHeader := ANextItem.Purpose = TListItemPurpose.Header;
  end;

  //if Index < C_PAGE_SIZE then
  //  CacheRow;

  if (OwnsBitmap = False)  then
  begin
    //ListView.CachePages;
    if ListView.FPageCaching.Enabled then
      Bitmap := ListView.LoadingBitmap
    else
      CacheRow;
    //ListView.FScrolling := True;
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
      ARect.Bottom := ARect.Bottom+1;
      Canvas.Fill.Color := ListView.Appearence.HeaderColor;
      Canvas.FillRect(ARect, 0, 0, AllCorners, 1);
      ARect.Bottom := ARect.Bottom-1;
    end;
  end;

  if Purpose = TListItemPurpose.None then
  begin
    Canvas.Fill.Color := ListView.Appearence.ItemBackground;
    if FBackgroundColor <> claNull then
      Canvas.Fill.Color := FBackgroundColor;

    if (Index = ListView.ItemIndex) and (ListView.ShowSelection) then
      Canvas.Fill.Color := ListView.Appearence.SelectedColor;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 1);
  end;

  ListView.DoRenderRow(Self);
  inherited;

  if (Purpose = TListItemPurpose.None) and (ANextItemIsHeader = False) then
  begin
    Canvas.Fill.Color := ListView.Appearence.SeparatorColor;
    if Canvas.Fill.Color = claNull then
      Canvas.Fill.Color := C_DEFAULT_SEPARATOR_COLOR;
    Canvas.FillRect(RectF(40, ARect.Bottom-1, ARect.Right, ARect.Bottom), 0, 0, AllCorners, 1, Canvas.Fill);
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
  FTitle.Font.Size := 13;
  FTitle.TextColor := claDimgray;
  FTitle.TextAlignment := TTextAlign.Leading;
  FTitle.ID := C_TITLE;
  // sub-title...
  FSubTitle.TextColor := claGray;
  FSubTitle.Font.Size := 13;
  FSubTitle.TextAlignment := TTextAlign.Leading;
  FSubTitle.ID := C_SUBTITLE;
  // detail...
  FDetail.Align := TListItemAlign.Trailing;
  FDetail.TextColor := claDodgerblue;
  FDetail.Font.Size := 13;
  FDetail.ID := C_DETAIL;
  FDetail.TextAlignment := TTextAlign.Trailing;
  FRowHeight := lv.ItemHeight;
  FBackgroundColor := claNull;
  FLastHeight := 0;
  FUpdating := False;
end;

destructor TKsListItemRow.Destroy;
begin
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

function TKsListItemRow.GetAccessory: TAccessoryType;
begin
  Result := FAccessory.AccessoryType;
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
    Accessory := TAccessoryType.Checkmark;
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
  AWidth := TextWidth(AText);
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
  Result.Font.Assign(FFont);
  AHeight := TextHeight(AText, AWordWrap, AWidth);
  Result.FPlaceOffset := PointF(x, y);
  //if AWordWrap then
  //  AHeight := RowHeight(False);
  if AWidth = 0 then
    AWidth := TextWidth(AText);

  Result.Width := AWidth;
  Result.Height := AHeight;

  case AVertAlign of
    TListItemAlign.Leading: Result.VertAlign := TListItemAlign.Leading;
    TListItemAlign.Center: Result.VertAlign := TListItemAlign.Center;
    TListItemAlign.Trailing: Result.VertAlign := TListItemAlign.Trailing;
  end;
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

procedure TksListView.CachePages;
var
  ICount: integer;
  AFilteredIndex: integer;
begin
  //Application.ProcessMessages;
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
end;

procedure TksListView.CalculateSearchBoxHeight;
var
  ASearch: TSearchBox;
begin
  ASearch := TSearchBox.Create(nil);
  try
    FSearchBoxHeight := ASearch.Height;
  finally
    {$IFDEF NEXTGEN}
    ASearch.DisposeOf;
    {$ELSE}
    ASearch.Free;
    {$ENDIF}
  end;
end;

procedure TksListView.ClearItems;
begin
  TListView(Self).Items.Clear;
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
  TListView(Self).OnDeleteItem := DoOnDeleteItem;

  FItems := TKsListItemRows.Create(Self, inherited Items);
  FCombo := TComboBox.Create(nil);
  FCombo.OnClosePopup := ComboClosePopup;
  FActionButtons := TksListItemRowActionButtons.Create(Self);
  FDateSelector := TDateEdit.Create(nil);
  FDateSelector.OnClosePicker := ComboClosePopup;
  FPageCaching := TksPageCaching.Create;

  FScreenScale := GetScreenScale;
  FAppearence := TksListViewAppearence.Create(Self);

  FItemHeight := 44;
  FHeaderHeight := 44;

  //FClickTimer := TTimer.Create(Self);
  FLastWidth := 0;
  FSelectOnRightClick := False;
  FLastScrollPos := 0;
  FScrolling := False;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Interval := 500;
  FScrollTimer.OnTimer := DoScrollTimer;
  FScrollTimer.Enabled := True;
  FCheckMarks := ksCmNone;
  FCheckMarkStyle := ksCmsDefault;
  FItemImageSize := 32;
  FShowIndicatorColors := False;
  FKeepSelection := False;
  ItemSpaces.Right := 0;
  ItemSpaces.Left := 0;
  FScrollDirection := sdDown;
  CanSwipeDelete := False;
  FCanSwipeDelete := False;
  CalculateSearchBoxHeight;
  FMouseDownPos := PointF(-1, -1);
  //FScrollLockPosition := -1;
  FDisableMouseMove := False;
end;

procedure TksListView.DeselectRow;
begin
  ItemIndex := -1;
  Invalidate;
end;

destructor TksListView.Destroy;
begin
  FreeAndNil(FAppearence);
  FreeAndNil(FItems);
  if FLoadingBitmap <> nil then
    FreeAndNil(FLoadingBitmap);
  FreeAndNil(FActionButtons);
  FreeAndNil(FPageCaching);

  {$IFDEF NEXTGEN}
  FScrollTimer.DisposeOf;
  FCombo.DisposeOf;
  FDateSelector.DisposeOf;
  {$ELSE}
  FScrollTimer.Free;
  FCombo.Free;
  FDateSelector.Free;
  FActionButtons.Free;
  {$ENDIF}
  inherited;
end;

function TKsListItemRows.AddRowDateSelector(AText: string;
  ADate: TDateTime): TKsListItemRow;
begin
  Result := AddRow(AText, '', FormatDateTime('ddd, dd mmmm, yyyy', ADate), More);
  Result.Selector := DateSelector;
  Result.FSelectionValue := ADate;
end;

function TKsListItemRows.AddRowItemSelector(AText, ASelected: string; AItems: TStrings): TKsListItemRow;
begin
  Result := AddRow(AText, '', ASelected, More);
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
  Result := AddRow('', '', '', None);
  Result.Owner.Purpose := TListItemPurpose.Header;
  Result.CanSelect := False;
  Result.Height := FListView.HeaderHeight;
  Result.Owner.Height := FListView.HeaderHeight;
  Result.Title.Text := AText;
  Result.Title.TextColor := claBlack;
  Result.VertAlign := TListItemAlign.Trailing;
  if FListView.FUpdateCount = 0 then
    Result.CacheRow;
end;

function TKsListItemRows.AddRow(AText: string; const AAccessoryType: TksAccessoryType = None): TKsListItemRow;
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
  r := FListView.AddItem;
  r.Height := FListView.ItemHeight;
  Result := TKsListItemRow.Create(r);
  Result.Index := Count-1;
  Result.Height := FListView.ItemHeight;
  Result.Purpose := TListItemPurpose.None;

  if FListView.CheckMarks <> ksCmNone then
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
  if ASubTitle <> '' then
  begin
    Result.Title.PlaceOffset := PointF(0, -9);
    Result.SubTitle.PlaceOffset := PointF(0,9);
  end;
  if (FListView.FUpdateCount = 0) then
    Result.CacheRow;

  Result.SearchText := AText+ASubtitle+ADetail;
end;

function TksListView.AddItem: TListViewItem;
begin
  Result := inherited Items.Add;
end;

procedure TksListView.SelectDate(ARow: TKsListItemRow; ASelected: TDateTime; AOnSelectDate: TNotifyEvent);
begin
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

procedure TksListView.ShowPopupMenu(APopup: TPopupMenu; x, y: single);
var
  APoint: TPointF;
begin
  APoint := LocalToAbsolute(TPointF.Create(x, y));
  APoint := Scene.LocalToScreen(APoint);
  APopup.Popup(Round(APoint.X), Round(APoint.Y));
end;

procedure TksListView.ShowRowOptionButtons(ARow: TKsListItemRow;
  ASwipeDirection: TksItemSwipeDirection; AButtons: TksListItemRowActionButtons);
var
  ARect: TRectF;
  i: integer;
  AButton: TksListItemRowActionButton;
  ICount: integer;
begin
  ARect := GetItemRect(ARow.Index);
  for i := 0 to AButtons.Count-1 do
  begin
    AButton := AButtons[i];
    AButton.FBackground.Height := ARect.Height;
    AButton.FBackground.Position.X := ARect.Right;
    AButton.FBackground.Position.Y := ARect.Top;
    AButton.FBackground.Width := 60;
    AddObject(AButton.FBackground);
  end;

  for ICount := 1 to AButtons.Count do
  begin
    AButton := AButtons[ICount-1];
   // if ICount < AButtons.Count then
      TAnimator.AnimateFloat(AButton.FBackground, 'Position.X', (ARect.Right-(60*ICount)), 0.2)
    //else
    //  TAnimator.AnimateFloatWait(AButton.FBackground, 'Position.X', (ARect.Right-(60*ICount)), 0.2)
  end;
end;
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

procedure TksListView.DoActionButtonClicked(
  AButton: TksListItemRowActionButton);
begin
  if Assigned(FOnItemActionButtonClick) then
  begin
    FOnItemActionButtonClick(Self, AButton.FRow, AButton);
  end;
  FActionButtons.Clear;
end;

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

procedure TksListView.DoScrollTimer(Sender: TObject);
var
  AVisibleItems: TksVisibleItems;
  ASearchHeight: single;
begin
  if FScrolling = False then
  begin
    if ScrollViewPos <> FLastScrollPos then
    begin
      if (FKeepSelection = False) and (ItemIndex > -1) then
        DeselectRow;

      FScrolling := True;
      FScrollTimer.Interval := 50;
      //FScrollTimer.Enabled := True;
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
      ASearchHeight := 0;
      if SearchVisible then
        ASearchHeight := FSearchBoxHeight;
      if ScrollViewPos-ASearchHeight = GetMaxScrollPos then
      begin
        if Assigned(FOnScrollLastItem) then
          FOnScrollLastItem(Self);
      end;
      FScrolling := False;
      FScrollTimer.Interval := 500;
    end;
  end;
  FLastScrollPos := ScrollViewPos;
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
      //ARow.CacheRow;
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
  FWidth := Width;
  RedrawAllRows;

end;

function TksListView.RowObjectAtPoint(ARow: TKsListItemRow; x, y: single): TksListItemRowObj;
var
  ICount: integer;
  AObjRect: TRectF;
begin
  Result := nil;
  for ICount := ARow.RowObjectCount - 1 downto 0 do
  begin
    AObjRect := ARow.RowObject[ICount].Rect;
    InflateRect(AObjRect, 4, 4);
    if PtInRect(AObjRect, PointF(X, Y)) then
    begin
      Result := ARow.RowObject[ICount];
      Exit;
    end;
  end;
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
  if SearchVisible then
  begin
    cr.Top := FSearchBoxHeight;
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
    //FLoadingBitmap.Canvas.Fill.Color := claWhite;
    //FLoadingBitmap.Canvas.FillRect(RectF(16, 16, 46, 46), 0, 0, AllCorners, 1, FLoadingBitmap.Canvas.Fill);
    //FLoadingBitmap.Canvas.FillText(RectF(8, 0, 200, FItemHeight), 'PLEASE WAIT...', False, 1, [], TTextAlign.Leading);
    FLoadingBitmap.Canvas.EndScene;
  end;
  Result := FLoadingBitmap;
end;
                      {
procedure TksListView.LockScrolling(AScrollPos: single);
begin
  FScrollLockPosition := AScrollPos;
  ScrollViewPos := AScrollPos;
end;                   }

procedure TksListView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
var
  ARow: TKsListItemRow;
  ARowRect: TRectF;
begin
  FActionButtons.Clear;
  //UnlockScrolling;
  ARow := GetRowFromYPos(y);
  if (y < 0) or (ARow = nil) then
    Exit;

  FMouseDownPos := PointF(x-ItemSpaces.Left, y);
  FMouseDownTime := Now;



  if ARow.CanSelect = False then
    DeselectRow;

  ARowRect := GetItemRect(ARow.Index);
  FClickedRowObj := RowObjectAtPoint(ARow, x, y - ARowRect.Top);
  if FClickedRowObj <> nil then
  begin
    FClickedRowObj.MouseDown;
    if FClickedRowObj.ConsumesRowClick then
    begin
      Invalidate;
      Exit;
    end;
  end;

  FActionButtons.Clear;
  FLastIndex := ItemIndex;

  if ARow.CanSelect = False then
    DeselectRow;

  inherited;

  if (Button = TMouseButton.mbRight) and (SelectOnRightClick) then
  begin
    ItemIndex := ARow.Index;
  end;

  if ARow.CanSelect = False then
  begin
    ItemIndex := FLastIndex;
    Invalidate;
    //Application.ProcessMessages;
    Exit;
  end;
end;

procedure TksListView.MouseMove(Shift: TShiftState; X, Y: Single);
var
  ASwipeDirection: TksItemSwipeDirection;
  ARow: TKsListItemRow;
  AMouseDownTime: integer;
  AMouseDownRow: TKsListItemRow;

begin
  if FDisableMouseMove then
    Exit;
  if y < 0 then
    Exit;
  inherited;
  ARow := GetRowFromYPos(Y);


  AMouseDownRow := GetRowFromYPos(FMouseDownPos.Y);
  AMouseDownTime := MilliSecondsBetween(FMouseDownTime, Now);
  if (ssLeft in Shift) then
  begin
    if ((AMouseDownTime > 0) and (AMouseDownTime < 1000)) and (AMouseDownRow <> nil) and (AMouseDownRow = ARow) then
    begin
      if (x < (FMouseDownPos.X-C_SWIPE_DISTANCE)) or (x > (FMouseDownPos.X+C_SWIPE_DISTANCE)) then
      begin
        ReleaseAllDownButtons;
        FDisableMouseMove := True; //LockScrolling(ScrollViewPos);
        ASwipeDirection := TksItemSwipeDirection.sdLeftToRight;
        if (x < (FMouseDownPos.X - C_SWIPE_DISTANCE)) then ASwipeDirection := TksItemSwipeDirection.sdRightToLeft;
        if (x > (FMouseDownPos.X + C_SWIPE_DISTANCE)) then ASwipeDirection := TksItemSwipeDirection.sdLeftToRight;
        FActionButtons.Clear;
        FActionButtons.FRow := AMouseDownRow;
        if Assigned(FOnItemSwipe) then
          FOnItemSwipe(Self, AMouseDownRow, ASwipeDirection, FActionButtons);
        DeselectRow;
        ShowRowOptionButtons(AMouseDownRow, ASwipeDirection, FActionButtons);
        Exit;
      end;
    end;
  end;



  FCurrentMousepos := PointF(x-ItemSpaces.Left, y);
  if (FMouseDownPos = PointF(-1, -1)) then
  begin
    if (ssLeft in Shift) then
    begin
      FMouseDownPos := FCurrentMousepos;
      FMouseDownTime := Now;
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
  try
    if y < 0 then
      Exit;
    if FActionButtons.Count > 0 then
      Exit;

    inherited;
    AObjectConsumesClick := False;

    AMouseDownTime := MilliSecondsBetween(FMouseDownTime, Now);
    AMouseDownRect := RectF(FMouseDownPos.X-8, FMouseDownPos.Y-8, FMouseDownPos.X+8, FMouseDownPos.Y+8);
    x := x - ItemSpaces.Left;

    if FClickedRowObj <> nil then
    begin
      FClickedRowObj.MouseUp;
      AObjectConsumesClick := (FClickedRowObj.ConsumesRowClick);
      // ItemIndex only needs to be set to -1 if the RowObj consumes
      // the row click otherwise it doesn't highlight if you click
      // on some text.
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
        ARow.ProcessClick;
        if (ARow.CanSelect) and (AObjectConsumesClick = False) then
        begin
          // left click...
          if (Assigned(FOnItemClick)) and (Button = TMouseButton.mbLeft) then
            FOnItemClick(Self, x, y, ARow, AId, FClickedRowObj);
          // right click...
          if (Assigned(FOnItemRightClick)) and (Button = TMouseButton.mbRight) then
            FOnItemRightClick(Self, x, y, ARow, AId, FClickedRowObj);
        end;

        if FClickedRowObj <> nil then
        begin
          ARow.CacheRow;
          Invalidate;
          FClickedRowObj.ProcessClick(X - FClickedRowObj.Rect.Left, Y - FClickedRowObj.Rect.Top);
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


        //Application.ProcessMessages;
        if (FKeepSelection = False) and (ItemIndex > -1) and (FScrolling = False) then
        begin
          DeselectRow;
        end;
      end;
    end
    else
    begin
      // mouse up was after scrolling...
    end;
  finally
    if (FKeepSelection = False) and (ItemIndex > -1) then
      DeselectRow;
    ReleaseAllDownButtons;
    FDisableMouseMove := False;
    FMouseDownPos := PointF(-1, -1);
  end;
end;

procedure TksListView.Paint;
begin
  if (ScrollViewPos <> FLastScrollPos) and (FActionButtons.Count > 0) then
    FActionButtons.Clear;
  if FIsShowing = False then
  begin
    CachePages;
    FIsShowing := True;
  end;
  inherited;
end;

{ TksListItemRowSwitch }


constructor TksListItemRowSwitch.Create(ARow: TKsListItemRow);
begin
  inherited Create(ARow);
  FActiveColor := C_DEFAULT_ACTIVE_SWITCH_COLOR;
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
  DrawSwitch(ACanvas, Rect, FIsChecked, FEnabled, FActiveColor);
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
begin
  Width := 14;
  Height := 14;
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
  if (FAccessoryType = TAccessoryType.Checkmark) and
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
    DrawAccessory(ACanvas, Rect, C_PLATFORM_ACCESSORY_COLOR, FAccessoryType);
  end;
  Result := True;
end;

procedure TKsListItemRowAccessory.SetAccessoryType(const Value: TAccessoryType);
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
end;

function TksListItemRowButton.GetConsumesRowClick: Boolean;
begin
  Result := True;
end;

procedure TksListItemRowButton.MouseDown;
begin
  if FEnabled = False then
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

constructor TksListItemRowActionButton.Create(AOwner: TksListItemRowActionButtons);
begin
  FOwner := AOwner;
  FBackground := TRectangle.Create(nil);
  FBackground.Fill.Color := claRed;
  FBackground.Stroke.Kind := TBrushKind.None;
  FBackground.HitTest := True;
  FBackground.OnClick := DoClick;
  FLabel := TLabel.Create(FBackground);
  FLabel.Align := TAlignLayout.Client;
  FLabel.StyledSettings := [];
  FLabel.FontColor := claWhite;
  FLabel.Font.Size := 13;
  FLabel.Text := Text;
  FLabel.TextAlign := TTextAlign.Center;
  FBackground.AddObject(FLabel);
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

procedure TksListItemRowActionButton.SetColor(const Value: TAlphaColor);
begin
  FBackground.Fill.Color := Value;
end;

procedure TksListItemRowActionButton.SetText(const Value: string);
begin
  FLabel.Text := Value;
end;

{ TksListItemRowOptionButtons }

procedure TksListItemRowActionButtons.AddButton(AText: string;
  AColor: TAlphaColor);
var
  AButton: TksListItemRowActionButton;
begin
  AButton := TksListItemRowActionButton.Create(Self);
  AButton.Text := AText;
  AButton.Color := AColor;
  AButton.FRow := FRow;
  Add(AButton);
end;

constructor TksListItemRowActionButtons.Create(AOwner: TksListView);
begin
  inherited Create(True);
  FListView := AOwner;
end;

{ TksPageCaching }

constructor TksPageCaching.Create;
begin
  inherited Create;
  FEnabled := True;
  FPageSize := C_DEFAULT_PAGE_SIZE;
end;

initialization

  ATextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  {$IFDEF MSWINDOWS}
  DefaultScrollBarWidth := 16;
  {$ENDIF}

finalization

  FreeAndNil(ATextLayout);


end.
