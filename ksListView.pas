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
  ******************************************************************************* }

unit ksListView;

interface

uses
  Classes, FMX.Types, FMX.Controls, FMX.ListView, Types, FMX.TextLayout,
  FMX.ListView.Types, FMX.Graphics, Generics.Collections, System.UITypes,
  FMX.ImgList, System.UIConsts;

const
  C_LONG_TAP_DURATION = 5;  // 500 ms

type

  TksListView = class;
  TKsListItemRow = class;
  TksListItemRowObj = class;

  TksListViewRowClickEvent = procedure(Sender: TObject; x, y: single; AItem: TListViewItem; AId: string; ARowObj: TksListItemRowObj) of object;

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
    FRow: TKsListItemRow;
    procedure SetRect(const Value: TRectF);
    procedure SetID(const Value: string);
    procedure Changed;
  protected
    procedure DoChanged(Sender: TObject);
  public
    constructor Create(ARow: TKsListItemRow); virtual;
    procedure Render(ACanvas: TCanvas); virtual; abstract;

    property Rect: TRectF read FRect write SetRect;
    property ID: string read FId write SetID;
  end;


  // ------------------------------------------------------------------------------

  TksListItemRowText = class(TksListItemRowObj)
  private
    FFont: TFont;
    FAlignment: TTextAlign;
    FTextLayout: TTextLayout;
    FTextColor: TAlphaColor;
    FText: string;
    FWordWrap: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetAlignment(const Value: TTextAlign);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetText(const Value: string);
    procedure SetWordWrap(const Value: Boolean);
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    procedure Render(ACanvas: TCanvas); override;
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
    procedure SetBitmap(const Value: TBitmap);
  public
    constructor Create(ARow: TKsListItemRow); override;
    destructor Destroy; override;
    procedure Render(ACanvas: TCanvas); override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
  end;


  // ------------------------------------------------------------------------------

  TKsListItemRow = class(TListItemImage)
  private
    FCached: Boolean;
    FFont: TFont;
    FTextColor: TAlphaColor;
    FIndicatorColor: TAlphaColor;
    FList: TObjectList<TksListItemRowObj>;
    FId: string;
    //FOnDrawRow:
    function TextHeight(AText: string): single;
    function TextWidth(AText: string): single;
    function RowHeight(const AScale: Boolean = True): single;
    function RowWidth(const AScale: Boolean = True): single;
    function GetListView: TCustomListView;
    function GetRowObject(AIndex: integer): TksListItemRowObj;
    function GetRowObjectCount: integer;
    property ListView: TCustomListView read GetListView;
    procedure DoOnListChanged(Sender: TObject; const Item: TksListItemRowObj;
      Action: TCollectionNotification);
    function ScreenWidth: single;
  public
    constructor Create(const AOwner: TListItem);
    destructor Destroy; override;

    procedure CacheRow;
    // bitmap functions...
    function DrawBitmap(ABmp: TBitmap; x, AWidth, AHeight: single): TksListItemRowImage overload;
    function DrawBitmap(ABmpIndex: integer; x, AWidth, AHeight: single): TksListItemRowImage overload;
    function DrawBitmap(ABmp: TBitmap; x, y, AWidth, AHeight: single): TksListItemRowImage overload;
    function DrawBitmapRight(ABmp: TBitmap; AWidth, AHeight, ARightPadding: single): TksListItemRowImage;
    // text functions...
    function TextOut(AText: string; x: single; const AVertAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    function TextOut(AText: string; x, y, AWidth: single; const AVertAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False): TksListItemRowText; overload;
    // right aligned text functions...
    function TextOutRight(AText: string; AXOffset: single; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText; overload;
    function TextOutRight(AText: string; AWidth: single; AXOffset: single; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText; overload;
    function TextOutRight(AText: string; y, AWidth: single; AXOffset: single; const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText; overload;
    // font functions...
    procedure SetFontProperties(AName: string; ASize: integer; AColor: TAlphaColor; AStyle: TFontStyles);
    // properties...
    property Font: TFont read FFont;
    property TextColor: TAlphaColor read FTextColor write FTextColor;
    property RowObject[AIndex: integer]: TksListItemRowObj read GetRowObject;
    property RowObjectCount: integer read GetRowObjectCount;
    property ID: string read FId write FId;
    property Cached: Boolean read FCached write FCached;
    property IndicatorColor: TAlphaColor read FIndicatorColor write FIndicatorColor;
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
    FDefaultRowHeight: integer;
    FAppearence: TksListViewAppearence;
    FOnItemClickEx: TksListViewRowClickEvent;
    FOnItemRightClickEx: TksListViewRowClickEvent;
    FMouseDownPos: TPointF;
    FCurrentMousepos: TPointF;
    FItemHeight: integer;
    FClickTimer: TTimer;
    FLastWidth: integer;
    FMouseDownDuration: integer;
    FOnLongClick: TksListViewRowClickEvent;
    FClickedRowObj: TksListItemRowObj;
    FClickedItem: TListViewItem;
    FSelectOnRightClick: Boolean;
    procedure SetItemHeight(const Value: integer);
    procedure DoClickTimer(Sender: TObject);
    procedure RedrawAllRows;
    function GetCachedRow(index: integer): TKsListItemRow;
    { Private declarations }
  protected
    procedure SetColorStyle(AName: string; AColor: TAlphaColor);
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoItemClick(const AItem: TListViewItem); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddRow(const ASearchIndex: string = ''; const APurpose: TListItemPurpose = TListItemPurpose.None; const AId: string = ''): TKsListItemRow;
    function AddHeader(AText: string): TKsListItemRow;
    function ItemsInView: TksVisibleItems;
    procedure EndUpdate; override;
    property CachedRow[index: integer]: TKsListItemRow read GetCachedRow;

    { Public declarations }
  published
    property Appearence: TksListViewAppearence read FAppearence
      write FAppearence;
    property ItemHeight: integer read FItemHeight write SetItemHeight
      default 44;
    property OnEditModeChange;
    property OnEditModeChanging;
    property EditMode;

    property Transparent default False;
    property AllowSelection;
    property AlternatingColors;
    property ItemIndex;
    property Images;
    property ScrollViewPos;
    property ItemSpaces;
    property SideSpace;

    property OnItemClickEx: TksListViewRowClickEvent read FOnItemClickEx write FOnItemClickEx;
    property OnItemClickRightEx: TksListViewRowClickEvent read FOnItemRightClickEx write FOnItemRightClickEx;

    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
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
    property OnItemsChange;
    property OnScrollViewChange;
    property OnItemClick;

    property OnButtonClick;
    property OnButtonChange;

    property OnDeletingItem;
    property OnDeleteItem;
    property OnDeleteChangeVisible;
    property OnSearchChange;
    property OnFilter;
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
    property PullRefreshWait;
    property OnLongClick: TksListViewRowClickEvent read FOnLongClick write FOnLongClick;
  end;

procedure Register;

implementation

uses SysUtils, FMX.Platform, FMX.Forms, FMX.SearchBox;


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
{$IFDEF IOS}
  if Result = 1 then
    Result := 1.5;
{$ENDIF}
end;

// ------------------------------------------------------------------------------

{ TksListItemRowObj }

procedure TksListItemRowObj.Changed;
begin
  FRow.Cached := False;
end;

constructor TksListItemRowObj.Create(ARow: TKsListItemRow);
begin
  inherited Create;
  FRow := ARow;
end;

procedure TksListItemRowObj.DoChanged(Sender: TObject);
begin
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

// ------------------------------------------------------------------------------

{ TksListItemRowText }

constructor TksListItemRowText.Create(ARow: TKsListItemRow);
begin
  inherited Create(ARow);
  FFont := TFont.Create;
  FTextColor := claBlack;
  FWordWrap := False;
end;

destructor TksListItemRowText.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TksListItemRowText.Render(ACanvas: TCanvas);
begin
  ACanvas.Fill.Color := FTextColor;
  ACanvas.Font.Assign(FFont);
  ACanvas.FillText(FRect, FText, FWordWrap, 1, [], FAlignment);
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
end;

destructor TksListItemRowImage.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TksListItemRowImage.Render(ACanvas: TCanvas);
begin
  ACanvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height),
    FRect, 1, True);
end;

procedure TksListItemRowImage.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  FBitmap.BitmapScale := GetScreenScale;
  Changed;
end;

// ------------------------------------------------------------------------------

{ TksListItemRow }

procedure TKsListItemRow.CacheRow;
var
  ICount: integer;
begin
  if FCached then
    Exit;
  BeginUpdate;
  Bitmap.Width := Round(RowWidth);
  Bitmap.Height := Round(RowHeight);
  Bitmap.Clear(claNull);
  Bitmap.Canvas.BeginScene;
  if FIndicatorColor <> claNull then
  begin
    Bitmap.Canvas.Fill.Color := FIndicatorColor;
    Bitmap.Canvas.FillRect(RectF(0, 8, 6, RowHeight(False)-8), 0, 0, [], 1, Bitmap.Canvas.Fill);
  end;

  for ICount := 0 to FList.Count - 1 do
  begin
    FList[ICount].Render(Bitmap.Canvas);
  end;
  Bitmap.Canvas.EndScene;
  EndUpdate;
  FCached := True;
end;

constructor TKsListItemRow.Create(const AOwner: TListItem);
var
  ABmp: TBitmap;
begin
  inherited Create(AOwner);
{$IFDEF MSWINDOWS}
  ScalingMode := TImageScalingMode.Original;
{$ENDIF}
  FIndicatorColor := claNull;
  OwnsBitmap := True;
  FList := TObjectList<TksListItemRowObj>.Create(True);
  FList.OnNotify := DoOnListChanged;
  ABmp := TBitmap.Create;
  ABmp.BitmapScale := GetScreenScale;
  ABmp.Width := Round(RowWidth);
  ABmp.Height := Round(RowHeight);
  ABmp.Clear(claNull);
  Bitmap := ABmp;
  FTextColor := claBlack;
  FFont := TFont.Create;
  FCached := False;
end;

destructor TKsListItemRow.Destroy;
begin
  FList.Free;
  FFont.Free;
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
  Result := lv.ItemAppearance.ItemHeight;
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

procedure TKsListItemRow.DoOnListChanged(Sender: TObject;
  const Item: TksListItemRowObj; Action: TCollectionNotification);
begin
  FCached := False;
end;

// ------------------------------------------------------------------------------

// bitmap drawing functions...

function TKsListItemRow.DrawBitmap(ABmp: TBitmap; x, AWidth, AHeight: single)
  : TksListItemRowImage;
var
  AYpos: single;
begin
  AYpos := (RowHeight(False) - AHeight) / 2;
  Result := DrawBitmap(ABmp, x, AYpos, AWidth, AHeight);
end;

function TKsListItemRow.DrawBitmap(ABmpIndex: integer;
  x, AWidth, AHeight: single): TksListItemRowImage overload;
var
  ABmp: TBitmap;
  il: TCustomImageList;
  ASize: TSizeF;
begin
  il := ListView.Images;
  if il = nil then
    Exit;
  ASize.cx := 64;
  ASize.cy := 64;
  ABmp := il.Bitmap(ASize, ABmpIndex);
  Result := DrawBitmap(ABmp, x, AWidth, AHeight);
end;

function TKsListItemRow.DrawBitmap(ABmp: TBitmap; x, y, AWidth, AHeight: single)
  : TksListItemRowImage;
begin
  Result := TksListItemRowImage.Create(Self);
  Result.FRect := RectF(x, y, x + AWidth, y + AHeight);
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

procedure TKsListItemRow.SetFontProperties(AName: string; ASize: integer;
  AColor: TAlphaColor; AStyle: TFontStyles);
begin
  if AName <> '' then
    FFont.Family := AName;
  FFont.Size := ASize;
  FTextColor := AColor;
  FFont.Style := AStyle;
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
  Result := TextOut(AText, x, AWidth, AVertAlign, AWordWrap);
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
  AYpos: single;
begin
  AYpos := y;
  case AVertAlign of
    TTextAlign.Center:
      AYpos := ((RowHeight(False) - TextHeight(AText)) / 2) + AYpos;
    TTextAlign.Trailing:
      AYpos := (RowHeight(False) - TextHeight(AText)) + AYpos;
  end;

  Result := TksListItemRowText.Create(Self);
  AHeight := TextHeight(AText);
  Result.FRect := RectF(x, AYpos, x + AWidth, AYpos + AHeight);

  if AWordWrap then
  begin
    AHeight := RowHeight(False);
    Result.FRect := RectF(x, 0, x + AWidth, AHeight);
  end;
  Result.Font.Assign(FFont);
  Result.TextAlignment := TTextAlign.Leading;
  Result.TextColor := FTextColor;
  Result.Text := AText;
  Result.WordWrap := AWordWrap;
  FList.Add(Result);
end;

function TKsListItemRow.TextOutRight(AText: string; AXOffset: single;
  const AVertAlign: TTextAlign = TTextAlign.Center): TksListItemRowText;
var
  AWidth: single;
begin
  AWidth := TextWidth(AText);
  Result := TextOutRight(AText, AWidth, AXOffset, AVertAlign);
end;

function TKsListItemRow.TextOutRight(AText: string; y, AWidth: single;
  AXOffset: single; const AVertAlign: TTextAlign = TTextAlign.Center)
  : TksListItemRowText;
begin
  Result := TextOut(AText, 0, y, AWidth);
  Result.TextAlignment := TTextAlign.Trailing;
  Result.Rect.Offset((ScreenWidth - (Result.Rect.Right + ListView.SideSpace)) +
    AXOffset, 0);
end;

function TKsListItemRow.TextOutRight(AText: string; AWidth: single;
  AXOffset: single; const AVertAlign: TTextAlign = TTextAlign.Center)
  : TksListItemRowText;
begin
  Result := TextOutRight(AText, 0, AWidth, AXOffset, AVertAlign);
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

constructor TksListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScreenScale := GetScreenScale;
  FAppearence := TksListViewAppearence.Create(Self);
  FItemHeight := 44;
  FClickTimer := TTimer.Create(Self);
  FLastWidth := 0;
  FSelectOnRightClick := False;
end;

destructor TksListView.Destroy;
begin
  FAppearence.Free;
  FClickTimer.Free;
  inherited;
end;

function TksListView.AddHeader(AText: string): TKsListItemRow;
begin
  Result := AddRow('', TListItemPurpose.Header, '');
  Result.Font.Style := [];
  Result.TextColor := claSilver;
  Result.Font.Size := 16;
  Result.TextOut(AText, 0, 0, Result.TextWidth(AText), TTextAlign.Trailing);
  Result.CacheRow;
end;

function TksListView.AddRow(const ASearchIndex: string = '';
  const APurpose: TListItemPurpose = TListItemPurpose.None;
  const AId: string = ''): TKsListItemRow;
var
  AItem: TListViewItem;
  AIndex: string;
begin
  AItem := Items.Add;
  AIndex := ASearchIndex;
  if AIndex = '' then
    AIndex := ' ';
  AItem.Text := AIndex;
  AItem.Objects.Clear;
  AItem.Purpose := APurpose;
  Result := TKsListItemRow.Create(AItem);
  Result.Name := 'ksRow';
  Result.ID := AId;
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
var
  ICount: integer;
  ARow: TKsListItemRow;
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

procedure TksListView.ApplyStyle;
var
  StyleObject: TFmxObject;
begin
  SetColorStyle('background', FAppearence.Background);
  SetColorStyle('itembackground', FAppearence.ItemBackground);
  SetColorStyle('alternatingitembackground',
    FAppearence.AlternatingItemBackground);
  inherited;
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
  ARow := nil;
  if FMouseDownDuration >= C_LONG_TAP_DURATION  then
  begin
    FClickTimer.Enabled := False;

    ARow := FClickedItem.Objects.FindObject('ksRow') as TKsListItemRow;
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


procedure TksListView.DoItemClick(const AItem: TListViewItem);
begin
  inherited;
end;

procedure TksListView.MouseUp(Button: TMouseButton; Shift: TShiftState; x,
  y: single);
var
  AId: string;
  ARow: TKsListItemRow;
  ICount: integer;
  AObjRect: TRectF;
  AMouseDownRect: TRect;
  ALongTap: Boolean;
begin
  inherited;
  FClickTimer.Enabled := False;
  ALongTap := FMouseDownDuration >= C_LONG_TAP_DURATION ;
  FMouseDownDuration := 0;

  AMouseDownRect := Rect(Round(FMouseDownPos.X-8), Round(FMouseDownPos.Y-8), Round(FMouseDownPos.X+8), Round(FMouseDownPos.Y+8));
  if not PtInRect(AMouseDownRect, Point(Round(x),Round(y))) then
    Exit;


  if FClickedItem <> nil then
  begin
    AId := '';
    ARow := FClickedItem.Objects.FindObject('ksRow') as TKsListItemRow;
    if ARow <> nil then
    begin
      AId := ARow.ID;
      for ICount := 0 to ARow.RowObjectCount - 1 do
      begin
        AObjRect := ARow.RowObject[ICount].Rect;
        if (FMouseDownPos.x >= (AObjRect.Left - 5)) and
          (FMouseDownPos.x <= (AObjRect.Right + 5)) then
        begin
          FClickedRowObj := ARow.RowObject[ICount];
          // Break;
        end;
      end;
    end;
    if not ALongTap then
    begin
      // normal click.
      if Button = TMouseButton.mbLeft then
      begin
        if Assigned(FOnItemClickEx) then
          FOnItemClickEx(Self, FMouseDownPos.x, FMouseDownPos.y, FClickedItem, AId, FClickedRowObj)
      else
        if Assigned(FOnItemRightClickEx) then
          FOnItemRightClickEx(Self, FMouseDownPos.x, FMouseDownPos.y, FClickedItem, AId, FClickedRowObj);
      end;
    end;
  end;
end;



procedure TksListView.RedrawAllRows;
var
  ICount: integer;
  ARow: TKsListItemRow;
begin
  for ICount := 0 to Items.Count - 1 do
  begin
    ARow := Items[ICount].Objects.FindObject('ksRow') as TKsListItemRow;
    if ARow <> nil then
    begin
      ARow.Cached := False;
      ARow.CacheRow;
    end;
  end;
  Invalidate;
end;

procedure TksListView.Resize;
begin
  inherited;
  RedrawAllRows;
end;

procedure TksListView.EndUpdate;
var
  ICount: integer;
  AItem: TListViewItem;
  ARow: TKsListItemRow;
  ARowObj: TKsListItemRow;
begin
  inherited EndUpdate;
  for ICount := 0 to Items.Count - 1 do
  begin
    AItem := Items[ICount];
    ARow := AItem.Objects.FindObject('ksRow') as TKsListItemRow;
    if ARow <> nil then
      ARow.CacheRow;
  end;
  Invalidate;
end;

function TksListView.GetCachedRow(index: integer): TKsListItemRow;
begin
  Result := Items[index].Objects.FindObject('ksRow') as TKsListItemRow;
end;

function TksListView.ItemsInView: TksVisibleItems;
var
  ICount: integer;
  ARect: TRectF;
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
      ASearchBox.Free;
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
 // AVisibleRect := RectF(0, 0, Width, Height);
  //OffsetRect(AVisibleRect, )
  {for ICount := 0 to Items.Count-1 do
  begin
    GetItemRect(ICount)
  end;  }

  {FItemsInView := TList<TKsListItemRow>.Create;
  try
    Repaint;
    Application.ProcessMessages;
    Result.IndexStart := -1;
    Result.IndexEnd := -1;
    Result.Count := FItemsInView.Count;

    for ICount := 0 to FItemsInView.Count-1 do
    begin
      if ICount = 0 then Result.IndexStart := ICount;
      if ICount = FItemsInView.Count-1 then Result.IndexEnd := ICount;
    end;
  finally
    FItemsInView.Free;
  end;
         }
  {for ICount :=  Low to High do
  begin
    Items[ICount].
  end; }
end;

procedure TksListView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
var
  ICount: integer;
begin
//{$IFDEF MSWINDOWS}
//  FMouseDownPos := PointF(x - 10, y);
//{$ELSE}
  FMouseDownPos := PointF(x, y);
//{$ENDIF}
  FCurrentMousepos := FMouseDownPos;
  FMouseDownDuration := 0;
  for Icount := 0 to Items.Count-1 do
  begin
    if PtInRect(GetItemRect(ICount), PointF(x,y)) then
    begin
      FClickedItem := Items[ICount];
      if (Button = TMouseButton.mbRight) and (FSelectOnRightClick) then
        ItemIndex := Icount;
    end;
  end;
  inherited;
  Application.ProcessMessages;
  FClickTimer.Interval := 100;
  FClickTimer.OnTimer := DoClickTimer;
  FClickTimer.Enabled := True;
end;

procedure TksListView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  //{$IFDEF MSWINDOWS}
  //  FCurrentMousepos := PointF(x - 10, y);
  //{$ELSE}
    FCurrentMousepos := PointF(x, y);
  //{$ENDIF}
end;

end.
