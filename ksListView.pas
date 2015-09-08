﻿{ *******************************************************************************
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

{$IFDEF VER300}
  {$DEFINE XE8_OR_NEWER}
  {$DEFINE XE10_OR_NEWER}
{$ENDIF}

uses
  Classes, FMX.Types, FMX.Controls, FMX.ListView, Types, FMX.TextLayout,
  FMX.ListView.Types, FMX.Graphics, Generics.Collections, System.UITypes,
  {$IFDEF XE8_OR_NEWER} FMX.ImgList, {$ENDIF}
  System.UIConsts, FMX.StdCtrls, FMX.Styles.Objects, System.Generics.Collections,
  FMX.ListBox, FMX.DateTimeCtrls, FMX.Menus
  {$IFDEF XE10_OR_NEWER}, FMX.ListView.Appearances {$ENDIF}

  ;

const
  C_LONG_TAP_DURATION     = 5;  // 500 ms
  C_BUTTON_HEIGHT = 29;
  {$IFDEF ANDROID}
  C_SEGMENT_BUTTON_HEIGHT = 42;
  {$ELSE}
  C_SEGMENT_BUTTON_HEIGHT = 29;
  {$ENDIF}

  C_DEFAULT_TEXT_COLOR = claBlack;
  C_DEFAULT_HEADER_TEXT_COLOR = claBlack;
  C_DEFAULT_SEGMENT_BUTTON_COLOR = claNull;

  C_TITLE    = 'TITLE';
  C_SUBTITLE = 'SUBTITLE';
  C_DETAIL   = 'DETAIL';

  {$IFDEF ANDROID}
  C_PAGE_SIZE = 30;
  {$ENDIF}
  {$IFDEF IOS}
  C_PAGE_SIZE = 50;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  C_PAGE_SIZE = 30;
  {$ENDIF}

type
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



  TksListView = class;
  TKsListItemRow = class;
  TKsListItemRows = class;
  TksListItemRowObj = class;
  TksListItemRowSwitch = class;
  TksListItemRowButton = class;
  TksListItemRowSegmentButtons = class;
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
  protected
    procedure CalculateRect(ARowBmp: TBitmap); virtual;
    procedure DoChanged(Sender: TObject);
  public
    constructor Create(ARow: TKsListItemRow); virtual;
    procedure Assign(ASource: TPersistent); override;
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
    property Width: single read FWidth write SetWidth;
    property Height: single read FHeight write SetHeight;
    property OffsetY: single read GetOffsetY write SetOffsetY;
    property OffsetX: single read GetOffsetX write SetOffsetX;
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
    function CalculateTextHeight(ACanvas: TCanvas): single;
  protected
    procedure CalculateRect(ARowBmp: TBitmap); override;
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function Render(ACanvas: TCanvas): Boolean; override;
    property Font: TFont read FFont write SetFont;
    property TextAlignment: TTextAlign read FAlignment write SetAlignment;
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
    FResources: TListItemStyleResources;
    FAccessoryType: TAccessoryType;
    FImage: TStyleObject;
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
    FState: TksButtonState;
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
    function TextHeight(AText: string): single;
    function TextWidth(AText: string): single;
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
    property ListView: TksListView read GetListView;
    procedure DoOnListChanged(Sender: TObject; const Item: TksListItemRowObj;
      Action: TCollectionNotification);
    function ScreenWidth: single;
    procedure ProcessClick;
    procedure Changed;
    procedure ReleaseAllDownButtons;
  protected
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
    procedure Render(const Canvas: TCanvas; const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
      const SubPassNo: Integer = 0); override;
    procedure Assign(Source: TPersistent); override;
    procedure CacheRow;
    procedure ReleaseRow;
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
    function AddButton(AWidth: integer; AText: string; const ATintColor: TAlphaColor = claNull): TksListItemRowButton; overload;
    function AddButton(AStyle: TksImageButtonStyle; const ATintColor: TAlphaColor = claNull): TksListItemRowButton; overload;
    function AddSegmentButtons(AWidth: integer;
                               ACaptions: array of string;
                               const AItemIndex: integer = -1): TksListItemRowSegmentButtons; overload;
    function AddSegmentButtons(AXPos, AWidth: integer;
                               ACaptions: array of string;
                               AAlign: TListItemAlign;
                               const AItemIndex: integer = -1): TksListItemRowSegmentButtons; overload;
    // text functions...
    function TextOut(AText: string; x: single; const AVertAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, y, AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOutRight(AText: string; y, AWidth: single; AXOffset: single; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText; overload;
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
  end;


  TKsListItemRows = class
  private
    FRows: TObjectList<TKsListItemRow>;
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
  public
    constructor Create(AListView: TksListView; AItems: TksListViewItems) ; virtual;
    destructor Destroy; override;

    //procedure CacheRowsInThread;
    procedure Add(ARow: TKsListItemRow);
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
    function AddRowDateSelector(AText: string; ADate: TDateTime): TKsListItemRow;
    function AddRowItemSelector(AText, ASelected: string; AItems: TStrings): TKsListItemRow; overload;
    function AddRowItemSelector(AText, ASelected: string; AItems: array of string): TKsListItemRow; overload;
    function AddHeader(AText: string): TKsListItemRow;

    procedure UncheckAll;
    procedure CheckAll;
    procedure Clear;
    procedure Delete(index: integer);
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
    FClickTimer: TTimer;
    FLastWidth: integer;
    FOnLongClick: TksListViewRowClickEvent;
    FClickedRowObj: TksListItemRowObj;
    FSelectOnRightClick: Boolean;
    FOnSwitchClicked: TksListViewClickSwitchEvent;
    FOnButtonClicked: TksListViewClickButtonEvent;
    FOnSegmentButtonClicked: TksListViewClickSegmentButtonEvent;
    FCacheTimer: TTimer;
    FScrollTimer: TTimer;
    FLastScrollPos: integer;
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
    FOnDeleteItem: TksDeleteItemEvent;
    FHeaderHeight: integer;
    FScrollDirection: TksScrollDirection;
    FLastRenderedIndex: integer;
    FLoadingBitmap: TBitmap;
    function PageFromRowIndex(AIndex: integer): integer;
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
    //procedure CachePage(APage: integer);
    //procedure ReleasePage(APage: integer);
    { Private declarations }
  protected
    procedure SetColorStyle(AName: string; AColor: TAlphaColor);
    procedure ApplyStyle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    {$IFNDEF XE10_OR_NEWER}
    procedure DoItemChange(const AItem: TListViewItem); override;
    {$ENDIF}
    function GetRowFromYPos(y: single): TKsListItemRow;
    procedure SetKsItemHeight(const Value: integer);
    procedure SetKsHeaderHeight(const Value: integer);
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure ClearItems;
    procedure RedrawAllRows;
    function ItemsInView: TksVisibleItems;
    procedure BeginUpdate; {$IFDEF XE8_OR_NEWER} override; {$ENDIF}
    procedure EndUpdate; {$IFDEF XE8_OR_NEWER} override; {$ENDIF}
    function IsShowing: Boolean;
    property Items: TKsListItemRows read FItems;
    procedure ShowPopupMenu(APopup: TPopupMenu; x, y: single);

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
    property ItemSpaces;
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
    property KeepSelection: Boolean read FKeepSelection write FKeepSelection default False;
    property OnLongClick: TksListViewRowClickEvent read FOnLongClick write FOnLongClick;
    property OnSwitchClick: TksListViewClickSwitchEvent read FOnSwitchClicked write FOnSwitchClicked;
    property OnButtonClicked: TksListViewClickButtonEvent read FOnButtonClicked write FOnButtonClicked;
    property OnSegmentButtonClicked: TksListViewClickSegmentButtonEvent read FOnSegmentButtonClicked write FOnSegmentButtonClicked;
    property OnScrollFinish: TksListViewFinishScrollingEvent read FOnFinishScrolling write FOnFinishScrolling;
  end;

procedure Register;

implementation

uses SysUtils, FMX.Platform, FMX.SearchBox, FMX.Objects,
  System.StrUtils, DateUtils, FMX.Forms, Math;

type
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
    FCacheTimer: TTimer;
    FListViews: TObjectList<TksListView>;
    function GetSwitchImage(AState: TksButtonState): TBitmap;
    function GetButtonImage(AWidth, AHeight: single; AText: string; ATintColor: TAlphaColor;
      AState: TksButtonState; AStyleLookup: string): TBitmap;
    procedure OnCacheTimer(Sender: TObject);
  public
    constructor Create(Owner: TksListView);
    destructor Destroy; override;
    function CreateImageCache: Boolean;
    property SwitchImage[AState: TksButtonState]: TBitmap read GetSwitchImage;
    property ButtonImage[AWidth, AHeight: single;
                         AText: string;
                         ATintColor: TAlphaColor;
                         AState: TksButtonState;
                         AStyleLookup: string]: TBitmap read GetButtonImage;
    property ImagesCached: Boolean read FImagesCached;

  end;



var
  AControlBitmapCache: TksControlBitmapCache;
  DefaultScrollBarWidth: integer = 7;

  ATextLayout: TTextLayout;


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
        if FId = C_TITLE then Rect.Width := (FRow.ListView.Width * 0.5) - 32;
        if FId = C_SUBTITLE then Rect.Width := (FRow.ListView.Width * 0.5) - 32;
        if FId = C_DETAIL then Rect.Width := (FRow.ListView.Width * 0.5) - 32;
        if Rect.Width = 0 then
          Rect.Width := ARowBmp.Canvas.TextWidth(FText);
      end;

      if FHeight = 0 then
      begin
        FHeight := CalculateTextHeight(ARowBmp.Canvas);
        if FHeight > FRow.Height  then
        begin
          Rect.Height := FHeight;
          FRow.Height := Round(FHeight);
        end;
      end;
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
  if (FId = C_TITLE) or (FId = C_SUBTITLE) then
  begin
    if FRow.Image.Bitmap.IsEmpty = False then
      Rect.Offset(FRow.Image.Width+8, 0);
    if FRow.ListView.ShowIndicatorColors then
      Rect.Offset(16, 0);
  end;
end;

function TksListItemRowText.CalculateTextHeight(ACanvas: TCanvas): single;
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

  ATextLayout.EndUpdate;

  ATextLayout.Trimming := TTextTrimming.Character;
  ATextLayout.TopLeft := Rect.TopLeft;
  ATextLayout.MaxSize := PointF(Rect.Width, Rect.Height);
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
  {$IFDEF IOS}
  FBitmap.DisposeOf;
  FBorder.DisposeOf;
  {$ELSE}
  FBitmap.Free;
  FBorder.Free;
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
      {$IFDEF IOS}
      ABmp.DisposeOf;
      {$ELSE}
      ABmp.Free;
      {$ENDIF}
    end;
  finally
    {$IFDEF IOS}
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
    {$IFDEF IOS}
    ABitmap.DisposeOf;
    {$ELSE}
    ABitmap.Free;
    {$ENDIF}
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

  if OwnsBitmap = False then
  begin
    Bitmap := TBitmap.Create(1,1);
    Bitmap.BitmapScale := GetScreenScale;
    OwnsBitmap := True;
    //
  end;

  lv := (ListView as TksListView);
  AMargins := lv.ItemSpaces;
  BeginUpdate;
  try

    ABmpWidth := (Round(RowWidth)) - Round((AMargins.Left + AMargins.Right)) * GetScreenScale;
    Bitmap.Height := Round(RowHeight);

    ADetailHeight := FDetail.CalculateTextHeight(Bitmap.Canvas);

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
      Bitmap.Canvas.FillRect(RectF(0, 8, 6, RowHeight(False)-8), 0, 0, [], 1, Bitmap.Canvas.Fill);
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
      FTitle.CalculateRect(Bitmap);
      FTitle.Render(Bitmap.Canvas);
    end;

    if FSubTitle.Text <> '' then
    begin
      FSubTitle.CalculateRect(Bitmap);
      FSubTitle.Render(Bitmap.Canvas);
    end;

    if FDetail.Text <> '' then
    begin
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

procedure TKsListItemRow.Changed;
begin
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
    OwnsBitmap := False;
    Bitmap := ListView.LoadingBitmap;
    FCached := False;
  end;
end;

procedure TksListItemRow.Render(const Canvas: TCanvas;
  const DrawItemIndex: Integer; const DrawStates: TListItemDrawStates;
  const SubPassNo: Integer);
begin
  if OwnsBitmap = False then
    Bitmap := ListView.LoadingBitmap;
  ListView.DoRenderRow(Self);
  inherited;
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
  FPickerItems.DisposeOf;
  {$ELSE}
  FList.Free;
  FFont.Free;
  FAccessory.Free;
  FImage.Free;
  FTitle.Free;
  FSubTitle.Free;
  FDetail.Free;
  FPickerItems.Free;
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
                                  const ATintColor: TAlphaColor = claNull): TksListItemRowButton;
begin
  Result := TksListItemRowButton.Create(Self);
  Result.Align := TListItemAlign.Trailing;
  Result.VertAlign := TListItemAlign.Center;
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
end;


function TKsListItemRow.AddSegmentButtons(AWidth: integer;
                                          ACaptions: array of string;
                                          const AItemIndex: integer = -1): TksListItemRowSegmentButtons;
begin
  Result := AddSegmentButtons(0, AWidth, ACaptions, TListItemAlign.Trailing, AItemIndex);
end;

function TKsListItemRow.AddSegmentButtons(AXPos, AWidth: integer;
                                          ACaptions: array of string;
                                          AAlign: TListItemAlign;
                                          const AItemIndex: integer = -1): TksListItemRowSegmentButtons;
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
  Result.Width := ASize.Width;
  Result.Height := ASize.Height;
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

  Result.Width := AWidth;
  Result.Height := AHeight;

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
  FRows.Clear;
  FListViewItems.Clear;
end;

procedure TksListView.CachePages;
var
  ACurrentPage: integer;
  ICount: integer;
  AStartIndex, AEndIndex: integer;
begin
  ACurrentPage := PageFromRowIndex(FLastRenderedIndex);
  AStartIndex := Max(((ACurrentPage-1) * C_PAGE_SIZE), 0);
  AEndIndex := Min(((ACurrentPage+1) * C_PAGE_SIZE), Items.Count-1);
  for ICount := 0 to Items.Count-1 do
  begin
    if (ICount in [AStartIndex..AEndIndex]) then
      Items[ICount].CacheRow
    else
      Items[ICount].ReleaseRow;
  end;
  Invalidate;

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


  FItems := TKsListItemRows.Create(Self, inherited Items);
  FCombo := TComboBox.Create(nil);
  FCombo.OnClosePopup := ComboClosePopup;

  FDateSelector := TDateEdit.Create(nil);
  FDateSelector.OnClosePicker := ComboClosePopup;

  FScreenScale := GetScreenScale;
  FAppearence := TksListViewAppearence.Create(Self);

  FOnDeleteItem := OnDeleteItem;
  OnDeleteItem := DoOnDeleteItem;

  if AControlBitmapCache = nil then
    AControlBitmapCache := TksControlBitmapCache.Create(Self);
  FCacheTimer := TTimer.Create(Self);
  FCacheTimer.Interval := 100;
  FCacheTimer.Enabled := True;
  AControlBitmapCache.FListViews.Add(Self);

  FItemHeight := 44;
  FHeaderHeight := 44;

  FClickTimer := TTimer.Create(Self);
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
  FScrollDirection := sdDown;
end;

destructor TksListView.Destroy;
begin
  {$IFDEF IOS}
  FAppearence.DisposeOf;
  FClickTimer.DisposeOf;
  FCacheTimer.DisposeOf;
  FScrollTimer.DisposeOf;
  FItems.DisposeOf;
  FCombo.DisposeOf;
  FDateSelector.DisposeOf;
  {$ELSE}
  FAppearence.Free;
  FClickTimer.Free;
  FCacheTimer.Free;
  FScrollTimer.Free;
  FItems.Free;
  FCombo.Free;
  FDateSelector.Free;
  {$ENDIF}
  if AControlBitmapCache <> nil then
    AControlBitmapCache.FListViews.Remove(Self);
  inherited;
end;

procedure TKsListItemRows.Add(ARow: TKsListItemRow);
begin
  FRows.Add(ARow);
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
    {$IFDEF IOS}
    AStrings.DisposeOf;
    {$ELSE}
    AStrings.Free;
    {$ENDIF}
  end;
end;

function TKsListItemRows.AddHeader(AText: string): TKsListItemRow;
begin
  Result := AddRow('', '', None);
  Result.Owner.Purpose := TListItemPurpose.Header;
  Result.Height := FListView.HeaderHeight;
  Result.Owner.Height := FListView.HeaderHeight;
  Result.Title.Text := AText;
  Result.VertAlign := TListItemAlign.Trailing;
end;

function TKsListItemRows.AddRow(AText, ADetail: string; AAccessory: TksAccessoryType;
  const AImageIndex: integer = -1; const AFontSize: integer = 14;
  AFontColor: TAlphaColor = C_DEFAULT_TEXT_COLOR): TKsListItemRow;
begin
  Result := AddRow(AText, '', ADetail, AAccessory, AImageIndex, AFontSize, AFontColor);

end;

function TKsListItemRows.AddRow(AText, ASubTitle, ADetail: string; AAccessory: TksAccessoryType;
  const AImageIndex: integer = -1; const AFontSize: integer = 14;
  AFontColor: TAlphaColor = C_DEFAULT_TEXT_COLOR): TKsListItemRow;
var
  r: TListViewItem;
begin
  r := FListView.AddItem;
  r.Objects.Clear;
  r.Height := FListView.ItemHeight;
  Result := TKsListItemRow.Create(r);
  Add(Result);
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

  Result.ImageIndex := AImageIndex;

  Result.Title.Text := AText;
  Result.SubTitle.Text := ASubTitle;
  Result.Detail.Text := ADetail;

  if ASubTitle <> '' then
  begin
    Result.Title.PlaceOffset := PointF(0, -9);
    Result.SubTitle.PlaceOffset := PointF(0,9);
  end;
  r.Text := '';
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
  Items.Delete(Aindex);
  if Assigned(FOnDeleteItem) then
    FOnDeleteItem(Sender, AIndex);
end;

procedure TksListView.DoRenderRow(ARow: TKsListItemRow);
begin
  FLastRenderedIndex := ARow.Index;
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
      FScrollTimer.Interval := 100;
      FLastScrollPos := Trunc(ScrollViewPos);
      if ScrollViewPos > FLastScrollPos then
        FScrollDirection := sdDown
      else
        FScrollDirection := sdUp;
      Exit;
    end;
  end
  else
  begin

    if FLastScrollPos = Trunc(ScrollViewPos) then
    begin
      FScrolling := False;
      FScrollTimer.Interval := 500;
      CachePages;

      if Assigned(FOnFinishScrolling) then
      begin
        FOnFinishScrolling(Self, AVisibleItems.IndexStart, AVisibleItems.Count);
      end;
    end;
  end;
  FLastScrollPos := Trunc(ScrollViewPos);
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



function TksListView.PageFromRowIndex(AIndex: integer): integer;
begin
  Result := AIndex div C_PAGE_SIZE;
end;

procedure TksListView.Paint;
begin
  FIsShowing := True;
  if not (csDesigning in ComponentState) then
  begin
    if (AControlBitmapCache <> nil) then
      if AControlBitmapCache.ImagesCached = False then
        Exit;
    if (ItemIndex > -1) then
      ShowSelection := Items[ItemIndex].CanSelect;
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
    ARow := Items[ICount];
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
    ARow := Items[ICount];
    ARow.ReleaseAllDownButtons;
  end;
end;

procedure TksListView.Resize;
begin
  inherited;
  {$IFDEF MSWINDOWS}
  if IsShowing then
    RedrawAllRows;
  {$ENDIF}
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
  CachePages;
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
      Result := _Items[ICount].Objects.FindDrawable('ksRow') as TKsListItemRow;;
      {$ELSE}
      Result := _Items[ICount].Objects.FindObject('ksRow') as TKsListItemRow;;
      {$ENDIF}
      Exit;
    end;
  end;
end;

function TksListView.IsShowing: Boolean;
begin
  FIsShowing := False;
  Repaint;
  Application.ProcessMessages;
  Result := FIsShowing;
end;

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
    FLoadingBitmap.Canvas.Fill.Color := claDimgray;
    FLoadingBitmap.Canvas.FillText(RectF(0, 0, 200, FItemHeight), 'PLEASE WAIT...', False, 1, [], TTextAlign.Leading);
    FLoadingBitmap.Canvas.EndScene;
  end;
  Result := FLoadingBitmap;
end;

procedure TksListView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
var

  ARow: TKsListItemRow;
begin
  if y < 0 then
    Exit;
  inherited;
  if (Button = TMouseButton.mbRight) then
    ItemIndex := GetRowFromYPos(y).Index;
  FMouseDownPos := PointF(x-ItemSpaces.Left, y);
  FMouseDownTime := Now;
  ARow := GetRowFromYPos(y);
  if ARow = nil then
    ItemIndex := -1;
end;

procedure TksListView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if y < 0 then
    Exit;
  inherited;
  FCurrentMousepos := PointF(x-ItemSpaces.Left, y);
end;


procedure TksListView.MouseUp(Button: TMouseButton; Shift: TShiftState; x,
  y: single);
var
  AId: string;
  ARow: TKsListItemRow;
  AMouseDownRect: TRectF;
  ARowRect: TRectF;
begin
  if y < 0 then
    Exit;
  inherited;
  AMouseDownRect := RectF(FMouseDownPos.X-8, FMouseDownPos.Y-8, FMouseDownPos.X+8, FMouseDownPos.Y+8);
  x := x - ItemSpaces.Left;
  if PtInRect(AMouseDownRect, PointF(x, y)) then
  begin
    // process a mouse click...
    ARow := GetRowFromYPos(y);
    if ARow = nil then
      Exit;
    ARowRect := GetItemRect(ARow.Index);

    FClickedRowObj := RowObjectAtPoint(ARow, x, y - ARowRect.Top);
    AId := ARow.ID;

    if MilliSecondsBetween(FMouseDownTime, Now) >= 500 then
    begin
      // long tap...
      if Assigned(FOnLongClick) then
        FOnLongClick(Self, x, y, ARow, AId, FClickedRowObj);
    end

    else
    begin
      Application.ProcessMessages;
      // remove row selection?
      ARow.ProcessClick;

      // left click...
      if (Assigned(FOnItemClick)) and (Button = TMouseButton.mbLeft) then
        FOnItemClick(Self, x, y, ARow, AId, FClickedRowObj);
      // right click...
      if (Assigned(FOnItemRightClick)) and (Button = TMouseButton.mbRight) then
        FOnItemRightClick(Self, x, y, ARow, AId, FClickedRowObj);

      if FClickedRowObj <> nil then
      begin
        FClickedRowObj.Click(X - FClickedRowObj.Rect.Left, Y - FClickedRowObj.Rect.Top);
        if (FClickedRowObj is TksListItemRowSwitch) then
        begin
          (FClickedRowObj as TksListItemRowSwitch).Toggle;
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
        if FClickedRowObj <> nil then
          FClickedRowObj.MouseUp;
        ARow.CacheRow;
        Invalidate;
      end;


      if (FKeepSelection = False) and (ItemIndex > -1) then
      begin
        Sleep(100);
        ItemIndex := -1;
        Application.ProcessMessages;
      end;


    end;
    ReleaseAllDownButtons;
  end
  else
  begin
    // mouse up was after scrolling...
  end;

  // remove row selection?
  if (FKeepSelection = False) and (ItemIndex > -1) then
  begin
    ItemIndex := -1;
    Application.ProcessMessages;
  end;
end;

{ TksListItemRowSwitch }


function TksListItemRowSwitch.Render(ACanvas: TCanvas): Boolean;
var
  ABmp: TBitmap;
  AState: TksButtonState;
begin
  Result := inherited Render(ACanvas);
  if AControlBitmapCache.ImagesCached = False then
  begin
    Result := False;
    Exit;
  end;
  ABmp := TBitmap.Create;
  try
    AState := Unpressed;
    if FIsChecked then AState := Pressed;
    ABmp.Assign(AControlBitmapCache.SwitchImage[AState]);
    ACanvas.DrawBitmap(ABmp, RectF(0,0,ABmp.Width,ABmp.Height), Rect, 1);
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
  if FIsChecked <> Value then
  begin
    FIsChecked := Value;
    Changed;
  end;
end;

procedure TksListItemRowSwitch.Toggle;
begin
  IsChecked := not IsChecked;
end;

{ TKsListItemRowAccessory }

procedure TKsListItemRowAccessory.CalculateRect(ARowBmp: TBitmap);
begin
  if (FAccessoryType = TAccessoryType.Checkmark) and
     (TksListView(FRow.ListView).CheckMarkStyle <> ksCmsDefault)  then
  begin
    Width := 20;
    Height := 20;
  end
  else
  begin
    if FImage = nil then
    begin
      Width := FImage.Width;
      Height := FImage.Height;
    end;
  end;
  inherited;
end;

constructor TKsListItemRowAccessory.Create(ARow: TKsListItemRow);
begin
  inherited;
  FAlign := TListItemAlign.Trailing;
  FVertAlignment := TListItemAlign.Center;
  FResources := FRow.GetStyleResources;
  FImage := FResources.AccessoryImages[FAccessoryType].Normal;
  FWidth := FImage.Width;
  FHeight := FImage.Height;
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
        {$IFDEF IOS}
        APath.DisposeOf;
        {$ELSE}
        APath.Free;
        {$ENDIF}
      end;

      ABmp.Canvas.EndScene;
      ADestRect := Rect;
      ACanvas.DrawBitmap(ABmp,
                         RectF(0, 0, ABmp.Width, ABmp.Height),
                         ADestRect,
                         1);

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
    FImage.DrawToCanvas(ACanvas, Rect, 1);
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


procedure TksListItemRowSegmentButtons.Click(x, y: single);
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
  AStyle: string;
  AState: TksButtonState;
begin
  Result := inherited Render(ACanvas);
  if AControlBitmapCache.ImagesCached = False then
    Exit;
  ABtnWidth := Trunc(FWidth / FCaptions.Count);
  ABtnRect := RectF(Rect.Left, Rect.Top, Rect.Left + ABtnWidth, Rect.Bottom);
  for ICount := 0 to FCaptions.Count-1 do
  begin
    if FItemIndex = -1 then
      FItemIndex := 0;
    AHeight := FHeight;

    ABmp := TBitmap.Create;
    try
      AStyle := '';
      if ICount = 0 then AStyle := 'segmentedbuttonleft';
      if ICount = FCaptions.Count-1 then AStyle := 'segmentedbuttonright';
      if AStyle = '' then AStyle := 'segmentedbuttonmiddle';
      AState := Unpressed;
      if FItemIndex = ICount then AState := Pressed;
      ABmp.Assign(AControlBitmapCache.ButtonImage[ABtnWidth,
                                                 AHeight,
                                                 FCaptions[ICount],
                                                 FTintColor,
                                                 AState,
                                                 AStyle]);
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
  if FTintColor <> Value then
  begin
    FTintColor := Value;
    Changed;
  end;
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
  FCacheTimer := TTimer.Create(nil);
  FCacheTimer.Interval := 100;
  FCacheTimer.OnTimer := OnCacheTimer;
  FListViews := TObjectList<TksListView>.Create(False);
end;

destructor TksControlBitmapCache.Destroy;
var
  ICount: integer;
begin
  {$IFDEF IOS}
  FCacheTimer.DisposeOf;
  FSwitchOn.DisposeOf;
  FSwitchOff.DisposeOf;
  for ICount := FCachedButtons.Count-1 downto 0 do
    FCachedButtons.Objects[ICount].DisposeOf;
  FCachedButtons.DisposeOf;
  FListViews.DisposeOf;
  {$ELSE}
  FCacheTimer.Free;
  FSwitchOn.Free;
  FSwitchOff.Free;
  for ICount := FCachedButtons.Count-1 downto 0 do
    FCachedButtons.Objects[ICount].Free;
  FCachedButtons.Free;
  FListViews.Free;
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
end;


function TksControlBitmapCache.GetButtonImage(AWidth, AHeight: single; AText: string;
  ATintColor: TAlphaColor; AState: TksButtonState; AStyleLookup: string): TBitmap;
var
  AId: string;
begin
  Result := nil;
  if FButton.Parent = nil then
    Exit;
  AId := FloatToStr(AWidth)+'_'+
         FloatToStr(AWidth)+'_'+
         AText+' '+
         IntToStr(Ord(AState))+'_'+
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
  FButton.StyledSettings := FButton.StyledSettings - [TStyledSetting.FontColor];

  FButton.TextSettings.FontColorForState.Active := ATintColor;
  FButton.TextSettings.FontColorForState.Normal := ATintColor;
  FButton.TextSettings.FontColorForState.Pressed := claWhite;
  FButton.TintColor := ATintColor;

  FButton.FontColor := ATintColor;
  FButton.StyleLookup := AStyleLookup;

  FButton.StaysPressed := True;
  FButton.GroupName := 'cacheButton';
  FButton.Width := AWidth;
  FButton.Height := AHeight;
  FButton.Text := AText;
  FButton.Font.Size := 11;
  FButton.StyledSettings := [TStyledSetting.Family, TStyledSetting.Style, TStyledSetting.Other];
  FButton.IsPressed := (AState = Pressed);

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

function TksControlBitmapCache.GetSwitchImage(AState: TksButtonState): TBitmap;
begin
  Result := nil;
  case AState of
    Pressed  : Result := FSwitchOn;
    Unpressed: Result := FSwitchOff;
  end;
end;

procedure TksControlBitmapCache.OnCacheTimer(Sender: TObject);
begin
  FCacheTimer.Enabled := False;
  if ImagesCached = False then
    CreateImageCache;
  if ImagesCached then
  begin
    FCacheTimer.OnTimer := nil;
    FCacheTimer.Enabled := False;
    Exit;
  end;
  FCacheTimer.Enabled := True;
end;

{ TksListItemRowButton }

constructor TksListItemRowButton.Create(ARow: TKsListItemRow);
begin
  inherited;
  FTintColor := claNull;
  FState := Unpressed;
end;

procedure TksListItemRowButton.MouseDown;
begin
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
var
  ABmp: TBitmap;
begin
  inherited Render(ACanvas);
  Result := False;
  if AControlBitmapCache.ImagesCached = False then
    Exit;

  ABmp := TBitmap.Create;
  try
    ABmp.Assign(AControlBitmapCache.ButtonImage[Width, Height, FText, FTintColor, FState, FStyleLookup]);
    if ABmp <> nil then
    begin
      if IsBlankBitmap(ABmp) then
        Exit;
      ACanvas.DrawBitmap(ABmp, RectF(0,0,ABmp.Width,ABmp.Height), Rect, 1, True);
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

function TKsListItemRows.AddRow(AText, ASubTitle, ADetail: string;
  AAccessory: TksAccessoryType; AImage: TBitmap; const AFontSize: integer;
  AFontColor: TAlphaColor): TKsListItemRow;
var
  r: TListViewItem;
begin
  r := FListView.AddItem;
  r.Height := FListView.ItemHeight;

  Result := TKsListItemRow.Create(r);
  Add(Result);
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
end;

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
  Result := FRows.Count;
end;

function TKsListItemRows.GetItems(index: integer): TKsListItemRow;
begin
  Result := FRows[index];
end;

procedure TKsListItemRows.ReindexRows;
var
  ICount: integer;
begin
  for ICount := 0 to Count-1 do
    Items[ICount].Index := ICount;
end;

constructor TKsListItemRows.Create(AListView: TksListView; AItems: TksListViewItems);
begin
  inherited Create;
  FRows := TObjectList<TKsListItemRow>.Create(False);
  FListView := AListView;
  FListViewItems := AItems;

end;

procedure TKsListItemRows.Delete(index: integer);
begin
  FRows.Delete(index);
  FListView._Items.Delete(index);
  ReindexRows;
end;

destructor TKsListItemRows.Destroy;
begin
  {$IFDEF IOS}
  FRows.DisposeOf;
  FRowThread.DisposeOf;
  {$ELSE}
  FRows.Free;
  {$ENDIF}
  inherited;
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
  ABmp := TBitmap.Create(Round(Rect.Width*4), Round(Rect.Height*4));
  ARect := RectF(0, 0, ABmp.Width, ABmp.Height);
  ABmp.Clear(claNull);
  ABmp.Canvas.BeginScene;
  try
    ABmp.Canvas.Fill.Color := FBorderColor;
    ABmp.Canvas.FillRect(ARect, FCornerRadius*4, FCornerRadius*4, AllCorners, 1);
    InflateRect(ARect, -(2/GetScreenScale), -(2/GetScreenScale));
    ABmp.Canvas.Fill.Color := FBackgroundColor;
    ABmp.Canvas.FillRect(ARect, FCornerRadius*4, FCornerRadius*4, AllCorners, 1);
    ABarRect := ARect;
    ABarRect.Width := (ABarRect.Width / 100) * FProgressPercent;

    ABmp.Canvas.Fill.Color := FBarColor;
    ABmp.Canvas.FillRect(ABarRect, FCornerRadius*4, FCornerRadius*4, [TCorner.TopLeft, TCorner.BottomLeft], 1);

    ABmp.Canvas.Fill.Color := claBlack;
    ABmp.Canvas.Stroke.Color := claBlack;
    ABmp.Canvas.Font.Size := 12 * 4;
    ABmp.Canvas.FillText(ARect, IntToStr(FProgressPercent)+'%', False, 1, [], TTextAlign.Center, TTextAlign.Center);

    ABmp.Canvas.EndScene;
    ARect := RectF(0, 0, ABmp.Width, ABmp.Height);
    ACanvas.DrawBitmap(ABmp, ARect, Rect, 1, False);
  finally
    ABmp.Free;
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

initialization

  ATextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  {$IFDEF MSWINDOWS}
  DefaultScrollBarWidth := 16;
  {$ENDIF}

finalization

  {$IFDEF IOS}
  AControlBitmapCache.DisposeOf;
  ATextLayout.DisposeOf;
  {$ELSE}
  AControlBitmapCache.Free;
  ATextLayout.Free;
  {$ENDIF}


end.
