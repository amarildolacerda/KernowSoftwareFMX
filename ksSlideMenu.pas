{*******************************************************************************
*                                                                              *
*  TksSlideMenu - Slide Menu Component                                         *
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

unit ksSlideMenu;

interface

{$IFDEF VER290}
  {$DEFINE XE8_OR_NEWER}
{$ENDIF}

{$IFDEF VER300}
  {$DEFINE XE8_OR_NEWER}
  {$DEFINE XE10_OR_NEWER}
{$ENDIF}


uses System.UITypes, FMX.Controls, FMX.Layouts, FMX.Objects, System.Classes,
  FMX.Types, Generics.Collections, FMX.Graphics, System.UIConsts, FMX.Effects,
  FMX.StdCtrls, System.Types, FMX.ListBox, FMX.Forms, ksListView
  {$IFDEF XE8_OR_NEWER}
  ,FMX.ImgList
  {$ENDIF}
  ;

const
  C_MENU_WIDTH = 250;
  C_TOOLBAR_HEIGHT = 50;
  C_DEFAULT_HEADER_HEIGHT = 30;
  C_DEFAULT_ITEM_HEIGHT = 50;
  C_DEFAULT_FONT_SIZE = 14;
  C_DEFAULT_TOOLBAR_FONT_SIZE = 14;
  C_DEFAULT_SLIDE_SPEED = 0.2;

type
  TSelectMenuItemEvent = procedure(Sender: TObject; AId: string) of object;

  TksMenuPosition = (mpLeft, mpRight);
  TKsMenuStyle = (msOverlap, msReveal);
  TKsMenuTheme = (mtCustom, mtDarkGray, mtDarkBlue);

  TksSlideMenu = class;

  TksSlideMenuItem = class
  strict private
    FText: string;
    FId: string;
    FFont: TFont;
    FImage: TBitmap;
    FHeight: integer;
    FIndex: integer;
    FIsHeader: Boolean;
  public
    constructor Create(AIndex: integer); virtual;
    destructor Destroy; override;
    property Height: integer read FHeight write FHeight;
    property Index: integer read FIndex;
    property Font: TFont read FFont write FFont;
    property Image: TBitmap read FImage write FImage;
    property ID: string read FId write FId;
    property Text: string read FText write FText;
    property IsHeader: Boolean read FIsHeader write FIsHeader;
  end;

  TksSlideMenuItems = class(TObjectList<TksSlideMenuItem>)
  private
    function AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
    function AddHeaderItem(AText: string): TksSlideMenuItem;
  end;

  TksSlideMenuAppearence = class(TPersistent)
  private
    FSlideMenu: TksSlideMenu;
    FHeaderColor: TAlphaColor;
    FItemColor: TAlphaColor;
    FFontColor: TAlphaColor;
    FSelectedColor: TAlphaColor;
    FSelectedFontColor: TAlphaColor;
    FTheme: TKsMenuTheme;
    procedure Changed;
    procedure SetHeaderColor(const Value: TAlphaColor);
    procedure SetItemColor(const Value: TAlphaColor);
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
    procedure SetSelectedFontColor(const Value: TAlphaColor);
    procedure SetTheme(const Value: TKsMenuTheme);
  public
    constructor Create(ASlideMenu: TksSlideMenu); virtual;
  published
    property HeaderColor: TAlphaColor read FHeaderColor write SetHeaderColor default $FF323232;
    property ItemColor: TAlphaColor read FItemColor write SetItemColor default $FF222222;
    property FontColor: TAlphaColor read FFontColor write SetFontColor default claWhite;
    property SelectedItemColor: TAlphaColor read FSelectedColor write SetSelectedColor default claRed;
    property SelectedFontColor: TAlphaColor read FSelectedFontColor write SetSelectedFontColor default claWhite;
    property Theme: TKsMenuTheme read FTheme write SetTheme default mtDarkGray;
  end;


  TksSlideMenuToolbar = class(TPersistent)
  private
    FSlideMenu: TksSlideMenu;
    FText: string;
    FBitmap: TBitmap;
    FVisible: Boolean;
    FHeight: integer;
    FFont: TFont;
    function GetImageRect: TRectF;
    procedure SetFont(const Value: TFont);
    procedure SetBitmap(const Value: TBitmap);

  public
    constructor Create(ASlideMenu: TksSlideMenu); virtual;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
    property Height: integer read FHeight default 44;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Visible: Boolean read FVisible write FVisible default True;
    property Text: string read FText write FText;
    property Font: TFont read FFont write SetFont;
  end;


  TksSlideMenuContainer = class(TLayout)
  private
    FSlideMenu: TksSlideMenu;
    FListView: TksListView;
    FImage: TImage;
    function CalculateListViewHeight: single;
    procedure ItemClick(Sender: TObject; x, y: Single; AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SwitchToImage;
    procedure SwitchToMenu;
    property ListView: TksListView read FListView;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidiOSDevice or pidAndroid)]
  TksSlideMenu = class(TFmxObject)
  strict private
    FAppearence: TksSlideMenuAppearence;
    FMenu: TksSlideMenuContainer;
    FItems: TksSlideMenuItems;
    FShadowLeft: TImage;
    FShadowRight: TImage;
    FBackground: TRectangle;
    FFormImage: TImage;
    FFont: TFont;
    FShowing: Boolean;
    FTopPadding: integer;
    FMenuPosition: TksMenuPosition;
    FMenuStyle: TKsMenuStyle;
    FSlideSpeed: Single;
    FOnSelectMenuItemEvent: TSelectMenuItemEvent;
    FAfterSelectMenuItemEvent: TSelectMenuItemEvent;
    FOnAfterSlideOut: TNotifyEvent;
    FToolBar: TksSlideMenuToolbar;
  private
    FHeaderHeight: integer;
    FItemHeight: integer;
    procedure SetTopPadding(const Value: integer);
    procedure DoBackgroundClick(Sender: TObject);
    procedure ToggleOverlap(ACacheFormImage: Boolean);
    procedure ToggleReveal(ACacheFormImage: Boolean);
    procedure FadeBackground;
    procedure UnfadeBackground;
    procedure GenerateFormImage(AForm: TForm);
    procedure GenerateShadows;
    procedure HidePickers;
    procedure MenuItemSelected(Sender: TObject; x, y: single; AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    {$IFDEF XE8_OR_NEWER}
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddHeader(AText: string);
    {$IFDEF XE8_OR_NEWER}
    function AddMenuItem(AId, AText: string; const AImageIndex: integer = -1): TksSlideMenuItem; overload;
    {$ENDIF}
    function AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem; overload;
    procedure ToggleMenu;
    procedure UpdateMenu;
    procedure ShowForm(AForm: TForm);
    property Showing: Boolean read FShowing;
  published
    property Appearence: TksSlideMenuAppearence read FAppearence write FAppearence;
    property Font: TFont read FFont write FFont;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    {$IFDEF XE8_OR_NEWER}
    property Images: TCustomImageList read GetImages write SetImages;
    {$ENDIF}
    property ItemHeight: integer read FItemHeight write FItemHeight default C_DEFAULT_ITEM_HEIGHT;
    property HeaderHeight: integer read FHeaderHeight write FHeaderHeight default C_DEFAULT_HEADER_HEIGHT;
    property TopPadding: integer read FTopPadding write SetTopPadding default 0;
    property MenuPosition: TksMenuPosition read FMenuPosition write FMenuPosition default mpLeft;
    property MenuStyle: TKsMenuStyle read FMenuStyle write FMenuStyle default msReveal;
    property SlideSpeed: Single read FSlideSpeed write FSlideSpeed;
    property OnSelectMenuItemEvent: TSelectMenuItemEvent read FOnSelectMenuItemEvent write FOnSelectMenuItemEvent;
    property AfterSelectItemEvent: TSelectMenuItemEvent read FAfterSelectMenuItemEvent write FAfterSelectMenuItemEvent;
    property Toolbar: TksSlideMenuToolbar read FToolbar write FToolbar;
    property OnAfterSlideOut: TNotifyEvent read FOnAfterSlideOut write FOnAfterSlideOut;

  end;

  procedure Register;

implementation

uses FMX.Platform, SysUtils, FMX.Ani, FMX.Pickers;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksSlideMenu]);
end;

function GetScreenScale: Single;
var
   Service : IFMXScreenService;
begin
   Service := IFMXScreenService(
      TPlatformServices.Current.GetPlatformService(IFMXScreenService));
   Result := Service .GetScreenScale;
end;

{ TSlideMenu }

procedure TksSlideMenu.AddHeader(AText: string);
var
  AResult: TksSlideMenuItem;
begin
  AResult := FItems.AddHeaderItem(AText);
  AResult.Font.Assign(FFont);
end;

{$IFDEF XE8_OR_NEWER}

function TksSlideMenu.AddMenuItem(AId, AText: string; const AImageIndex: integer = -1): TksSlideMenuItem;
var
  AImage: TBitmap;
  ASize: TSizeF;
begin
  AImage := nil;
  ASize.Width := 64;
  ASize.Height := 64;
  if Images <> nil then
    AImage := Images.Bitmap(ASize, AImageIndex);
  Result := AddMenuItem(AId, AText, AImage);
end;

{$ENDIF}

function TksSlideMenu.AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
begin
  Result := FItems.AddMenuItem(AId, AText, AImage);
  Result.Font.Assign(FFont);
end;

procedure TksSlideMenu.Clear;
begin
  FItems.Clear;
  UpdateMenu;
end;

constructor TksSlideMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FItems := TksSlideMenuItems.Create;
  FShadowLeft := TImage.Create(Self);
  FShadowRight := TImage.Create(Self);
  FAppearence := TksSlideMenuAppearence.Create(Self);
  FMenu := TksSlideMenuContainer.Create(Self);
  FToolbar := TksSlideMenuToolbar.Create(Self);
  FShowing := False;
  FTopPadding := 0;
  FFont.Size := C_DEFAULT_FONT_SIZE;
  FBackground := TRectangle.Create(Self);
  FFormImage := TImage.Create(Self);
  FFormImage.OnClick := DoBackgroundClick;
  FMenuPosition := mpLeft;
  FMenuStyle := msReveal;
  FSlideSpeed := C_DEFAULT_SLIDE_SPEED;
  FHeaderHeight := C_DEFAULT_HEADER_HEIGHT;
  FItemHeight := C_DEFAULT_ITEM_HEIGHT;
  GenerateShadows;
end;

destructor TksSlideMenu.Destroy;
begin
  FFont.Free;
  FItems.Free;
  FAppearence.Free;
  FToolbar.Free;
  if IsChild(FMenu) then FMenu.Free;
  if IsChild(FShadowLeft) then FShadowLeft.Free;
  if IsChild(FShadowRight) then FShadowRight.Free;
  if IsChild(FBackground) then FBackground.Free;
  if IsChild(FFormImage) then FFormImage.Free;
  inherited;
end;


procedure TksSlideMenu.DoBackgroundClick(Sender: TObject);
begin
  ToggleMenu;
end;

procedure TksSlideMenu.FadeBackground;
begin
  FBackground.Fill.Color := claBlack;
  FBackground.Align := TAlignLayout.Contents;
  FBackground.OnClick := DoBackgroundClick;
  FBackground.Opacity := 0;
  TForm(Owner).AddObject(FBackground);
  FBackground.BringToFront;
  TAnimator.AnimateFloat(FBackground, 'Opacity', 0.2, FSlideSpeed);
end;

procedure TksSlideMenu.GenerateFormImage(AForm: TForm);
var
  AScale: single;
  ABmp: TBitmap;
begin
  FFormImage.Visible := false;
  FMenu.Visible := False;
  ABmp := TBitmap.Create;
  try
    AScale := GetScreenScale;
    ABmp.BitmapScale := AScale;
    ABmp.Width := Round(AForm.Width * AScale);
    ABmp.Height := Round(AForm.Height * AScale);
    ABmp.Canvas.BeginScene;
    AForm.PaintTo(ABmp.Canvas);
    ABmp.Canvas.EndScene;
    ABmp.Canvas.BeginScene;
    ABmp.Canvas.Stroke.Color := claBlack;
    ABmp.Canvas.StrokeThickness := 1;
    ABmp.Canvas.DrawLine(PointF(0, 0), PointF(0, ABmp.Height), 1);
    ABmp.Canvas.EndScene;
    FFormImage.Width := Round(AForm.Width);
    FFormImage.Height := Round(AForm.Height);
    FFormImage.Bitmap.Assign(ABmp);
  finally
    ABmp.Free;
  end;
  FFormImage.Visible := True;
  FMenu.Visible := True;
end;

procedure TksSlideMenu.GenerateShadows;
var
  AScale: single;
  AForm: TForm;
  ABmp: TBitmap;
begin
  ABmp := TBitmap.Create;
  try
    AScale := GetScreenScale;
    AForm := (Owner as TForm);
    ABmp.Width := Round(16 * AScale);
    ABmp.Height := Round(AForm.Height * AScale);
    ABmp.Canvas.BeginScene;
    ABmp.Canvas.Fill.Kind := TBrushKind.Gradient;
    ABmp.Canvas.Fill.Gradient.Color := claNull;
    ABmp.Canvas.Fill.Gradient.Color1 := $AA000000;
    ABmp.Canvas.Fill.Gradient.StartPosition.X := 0;
    ABmp.Canvas.Fill.Gradient.StartPosition.Y := 1;
    ABmp.Canvas.Fill.Gradient.StopPosition.X := 1;
    ABmp.Canvas.FillRect(RectF(0, 0, ABmp.Width, ABmp.Height), 0, 0, [], 1);
    ABmp.Canvas.EndScene;
    FShadowLeft.Width := 16;
    FShadowLeft.Height := Round(AForm.Height);
    FShadowLeft.Bitmap.Assign(ABmp);
  finally
    ABmp.Free;
  end;

  ABmp := TBitmap.Create;
  try
    AScale := GetScreenScale;
    AForm := (Owner as TForm);
    ABmp.Width := Round(16 * AScale);
    ABmp.Height := Round(AForm.Height * AScale);
    ABmp.Canvas.BeginScene;
    ABmp.Canvas.Fill.Kind := TBrushKind.Gradient;
    ABmp.Canvas.Fill.Gradient.Color := $AA000000;
    ABmp.Canvas.Fill.Gradient.Color1 := claNull;
    ABmp.Canvas.Fill.Gradient.StartPosition.X := 0;
    ABmp.Canvas.Fill.Gradient.StartPosition.Y := 1;
    ABmp.Canvas.Fill.Gradient.StopPosition.X := 1;
    ABmp.Canvas.FillRect(RectF(0, 0, ABmp.Width, ABmp.Height), 0, 0, [], 1);
    ABmp.Canvas.EndScene;
    FShadowRight.Width := 16;
    FShadowRight.Height := Round(AForm.Height);
    FShadowRight.Bitmap.Assign(ABmp);
  finally
    ABmp.Free;
  end;
end;

{$IFDEF XE8_OR_NEWER}

function TksSlideMenu.GetImages: TCustomImageList;
begin
  Result := FMenu.ListView.Images;
end;

procedure TksSlideMenu.SetImages(const Value: TCustomImageList);
begin
  FMenu.ListView.Images := Value;
end;

{$ENDIF}

function TksSlideMenu.GetItemIndex: Integer;
begin
  Result := FMenu.ListView.ItemIndex;
end;

procedure TksSlideMenu.HidePickers;
var
  PickerService: IFMXPickerService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, PickerService) then
    PickerService.CloseAllPickers;
end;
procedure TksSlideMenu.MenuItemSelected(Sender: TObject; x, y: single;
  AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
begin
  if Assigned(FOnSelectMenuItemEvent) then
    FOnSelectMenuItemEvent(Self, AId);
  GenerateFormImage(Owner as TForm);
  Application.ProcessMessages;
  ToggleMenu;
  if Assigned(FOnAfterSlideOut) then
    FOnAfterSlideOut(Self);
end;



procedure TksSlideMenu.UpdateMenu;
var
  ICount: integer;
  AItem: TksSlideMenuItem;
  ARow: TKsListItemRow;
  ALastIndex: integer;
begin
  ALastIndex := FMenu.FListView.ItemIndex;
  FMenu.ListView.Position.X := 0;
  FMenu.ListView.Position.Y := 0;
  if FToolBar.Visible then
    FMenu.ListView.Position.Y := C_TOOLBAR_HEIGHT;

  FMenu.ListView.Width := C_MENU_WIDTH;


  FMenu.Width := C_MENU_WIDTH;
  FMenu.ListView.Appearence.Background := FAppearence.ItemColor;
  FMenu.ListView.Appearence.ItemBackground := FAppearence.ItemColor;
  FMenu.ListView.Appearence.HeaderColor := FAppearence.HeaderColor;
  FMenu.ListView.Appearence.SeparatorColor := FAppearence.HeaderColor;
  FMenu.ListView.Appearence.SelectedColor := FAppearence.SelectedItemColor;
  FMenu.ListView.HeaderHeight := FHeaderHeight;
  FMenu.ListView.ItemHeight := FItemHeight;
  FMenu.ListView.ItemImageSize := 24;
  FMenu.ListView.BeginUpdate;
  try
    FMenu.ListView.Items.Clear;
    for ICount := 0 to FItems.Count-1 do
    begin
      AItem := FItems[ICount];
      if AItem.IsHeader then
        ARow := FMenu.ListView.Items.AddHeader(AItem.Text)
      else
      begin
        ARow := FMenu.ListView.Items.AddRow(AItem.Text, '', '', More);
        ARow.Image.Bitmap.Assign(AItem.Image);
        ARow.ID := AItem.ID;

      end;
      ARow.Title.TextColor := FAppearence.FontColor;
    end;
  finally
    FMenu.ListView.EndUpdate;
  end;
  if ALastIndex = -1 then
  begin
    FMenu.ListView.SelectFirstItem;
    ALastIndex := FMenu.ListView.ItemIndex;
    if ALastIndex > -1 then
      FMenu.ListView.Items[ALastIndex].BackgroundColor := FAppearence.SelectedItemColor;
  end;
end;

procedure TksSlideMenu.SetItemIndex(const Value: Integer);
begin
  FMenu.ListView.ItemIndex := Value;
end;

procedure TksSlideMenu.SetTopPadding(const Value: integer);
begin
  FTopPadding := Value;
end;

procedure TksSlideMenu.ShowForm(AForm: TForm);
begin
  GenerateFormImage(AForm);
end;

procedure TksSlideMenu.ToggleMenu;
begin
  if FMenu.HitTest = False then
    Exit;

  HidePickers;
  FMenu.HitTest := False;

  FMenu.Height := (Owner as TForm).Height;
  if FMenu.ListView.Items.Count = 0 then
    UpdateMenu;
  Application.ProcessMessages;

  FMenu.SwitchToImage;

  case FMenuStyle of
    msOverlap: ToggleOverlap(not FShowing);
    msReveal: ToggleReveal(not FShowing);
  end;
  FShowing := not FShowing;

  if FShowing then
    FMenu.SwitchToMenu;

  FMenu.HitTest := True;
  if FShowing = False then
  begin
    if Assigned(FOnAfterSlideOut) then
      FOnAfterSlideOut(Self);
  end;
end;

procedure TksSlideMenu.ToggleOverlap(ACacheFormImage: Boolean);
var
  ANewX: Extended;
begin
  if ACacheFormImage then
    GenerateFormImage(Owner as TForm);

  if FShowing then
  begin
    // hide the menu...
    ANewX := 0-C_MENU_WIDTH;
    if FMenuPosition = mpRight then
      ANewX := TForm(Owner).Width;
    FMenu.Width := C_MENU_WIDTH;

    UnfadeBackground;
    TAnimator.AnimateFloatWait(FMenu, 'Position.X', ANewX,  FSlideSpeed);

    TForm(Owner).RemoveObject(FFormImage);
    TForm(Owner).RemoveObject(FMenu);
  end
  else
  begin
    FMenu.Position.Y := FTopPadding;
    FMenu.Position.X := 0-C_MENU_WIDTH;
    ANewX := 0;
    if FMenuPosition = mpRight then
    begin
      FMenu.Position.X := TForm(Owner).Width;//-C_MENU_WIDTH;

      ANewX := TForm(Owner).Width-C_MENU_WIDTH;
    end;
    TForm(Owner).AddObject(FFormImage);
    TForm(Owner).AddObject(FMenu);
    FadeBackground;
    TAnimator.AnimateFloatWait(FMenu, 'Position.X', ANewX,  FSlideSpeed);

  end;
  Application.ProcessMessages;
end;

procedure TksSlideMenu.ToggleReveal(ACacheFormImage: Boolean);
var
  ANewX: Extended;
  AShadow: TImage;
begin
  if ACacheFormImage then
    GenerateFormImage(Owner as TForm);

  AShadow := nil;
  case FMenuPosition of
    mpLeft: AShadow := FShadowLeft;
    mpRight: AShadow := FShadowRight;
  end;

  if FShowing then
  begin
    ANewX := 0;
    TAnimator.AnimateFloatWait(FFormImage, 'Position.X', ANewX, FSlideSpeed);
    TForm(Owner).RemoveObject(FFormImage);
    TForm(Owner).RemoveObject(FMenu);
    FFormImage.RemoveObject(FShadowLeft);
    FFormImage.RemoveObject(FShadowRight);
  end
  else
  begin
    AShadow.Position.X := 0-16;
    FMenu.Position.Y := FTopPadding;
    FMenu.Position.X := 0;
    ANewX := C_MENU_WIDTH;
    if FMenuPosition = mpRight then
    begin
      FMenu.Position.X := TForm(Owner).Width-C_MENU_WIDTH;
      AShadow.Position.X := TForm(Owner).Width;

      ANewX := 0-C_MENU_WIDTH;
    end;
    TForm(Owner).AddObject(FMenu);
    TForm(Owner).AddObject(FFormImage);
    {$IFNDEF ANDROID}
    FFormImage.AddObject(AShadow);
    {$ENDIF}

    TAnimator.AnimateFloatWait(FFormImage, 'Position.X', ANewX,  FSlideSpeed);
  end;
  Application.ProcessMessages;
end;

procedure TksSlideMenu.UnfadeBackground;
begin
  TAnimator.AnimateFloat(FBackground, 'Opacity', 0, FSlideSpeed);
end;

{ TksSlideMenuItem }

constructor TksSlideMenuItem.Create(AIndex: integer);
begin
  inherited Create;
  FImage := TBitmap.Create;
  FFont := TFont.Create;
  FIndex := AIndex;
  FIsHeader := False;
end;

destructor TksSlideMenuItem.Destroy;
begin
  FImage.Free;
  FFont.Free;
  inherited;
end;


{ TksSlideMenuItems }

function TksSlideMenuItems.AddHeaderItem(AText: string): TksSlideMenuItem;
begin
  Result := AddMenuItem('', AText, nil);
  Result.IsHeader := True;
end;

function TksSlideMenuItems.AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
begin
  Result := TksSlideMenuItem.Create(Count);
  if AImage <> nil then
    Result.Image.Assign(AImage);
  Result.Id := AId;
  Result.Text := AText;
  Add(Result);
end;

{ TksSlideMenuAppearence }

procedure TksSlideMenuAppearence.Changed;
begin
  FSlideMenu.UpdateMenu;
end;

constructor TksSlideMenuAppearence.Create(ASlideMenu: TksSlideMenu);
begin
  FSlideMenu := ASlideMenu;
  FHeaderColor := $FF323232;
  FItemColor := $FF222222;
  FFontColor := claWhite;
  FSelectedFontColor := claWhite;
  FSelectedColor := claRed;
  FTheme := mtDarkGray;
end;

procedure TksSlideMenuAppearence.SetFontColor(const Value: TAlphaColor);
begin
  FFontColor := Value;
  FTheme := mtCustom;
  Changed;
end;

procedure TksSlideMenuAppearence.SetHeaderColor(const Value: TAlphaColor);
begin
  FHeaderColor := Value;
  FTheme := mtCustom;
  Changed;
end;

procedure TksSlideMenuAppearence.SetItemColor(const Value: TAlphaColor);
begin
  FItemColor := Value;
  FTheme := mtCustom;
  Changed;
end;

procedure TksSlideMenuAppearence.SetSelectedColor(const Value: TAlphaColor);
begin
  FSelectedColor := Value;
  FTheme := mtCustom;
  Changed;
end;

procedure TksSlideMenuAppearence.SetSelectedFontColor(const Value: TAlphaColor);
begin
  FSelectedFontColor := Value;
  FTheme := mtCustom;
  Changed;
end;

procedure TksSlideMenuAppearence.SetTheme(const Value: TKsMenuTheme);
begin
  if Value = mtDarkGray then
  begin
    FHeaderColor := $FF323232;
    FItemColor := $FF222222;
    FFontColor := claWhite;
    FSelectedFontColor := claWhite;
    FSelectedColor := claRed;
  end;
  if Value = mtDarkBlue then
  begin
    FHeaderColor := $FF001C6F;
    FItemColor := $FF001451;
    FFontColor := claWhite;
    FSelectedFontColor := claWhite;
    FSelectedColor := claRed;
  end;
  FTheme := Value;
  Changed;
end;

{ TksSlideMenuContainer }

function TksSlideMenuContainer.CalculateListViewHeight: single;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to FListView.Items.Count-1 do
  begin
    Result := Result + FListView.Items[ICount].Height;
  end;
end;

constructor TksSlideMenuContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListView := TksListView.Create(Self);
  FImage := TImage.Create(Self);

  FSlideMenu := (AOwner as TksSlideMenu);
  FListView.KeepSelection := True;

  FListView.CanSwipeDelete := False;

  AddObject(FListView);
  AddObject(FImage);
  FListView.ShowSelection := False;
  HitTest := True;
  FImage.HitTest := False;
  FListView.OnItemClick := ItemClick;
  ClipChildren := True;

end;

destructor TksSlideMenuContainer.Destroy;
begin
  if IsChild(FListView) then FListView.Free;
  if IsChild(FImage) then FImage.Free;
  inherited;
end;

procedure TksSlideMenuContainer.ItemClick(Sender: TObject; x, y: Single;
  AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
var
  ICount: integer;
begin
  for ICount := 0 to FListView.Items.Count-1 do
  begin
    if AItem = FListView.Items[ICount] then
      FListView.Items[ICount].BackgroundColor := FSlideMenu.Appearence.SelectedItemColor
    else
      FListView.Items[ICount].BackgroundColor := FSlideMenu.Appearence.ItemColor;
  end;
  Application.ProcessMessages;
  Sleep(200);

  FSlideMenu.MenuItemSelected(Self, x, y, AItem, AId, ARowObj);
end;

procedure TksSlideMenuContainer.Paint;
begin
  inherited;
  Canvas.Clear(FSlideMenu.Appearence.ItemColor);
  if FSlideMenu.Toolbar.Visible then
  begin
    FSlideMenu.Toolbar.DrawToCanvas(Canvas, RectF(0, 0, Width, C_TOOLBAR_HEIGHT));
  end;
end;

procedure TksSlideMenuContainer.SwitchToImage;
var
  ABmp: TBitmap;
begin
  FListView.HitTest := False;
  FImage.Position.X := 0;
  FImage.Position.Y := 0;
  if FSlideMenu.Toolbar.Visible then
    FImage.Position.Y := C_TOOLBAR_HEIGHT;
  FImage.Width := C_MENU_WIDTH;
  FListView.Height := CalculateListViewHeight;
  FImage.Height := CalculateListViewHeight;
  ABmp := TBitmap.Create(Round(FListView.Width*4), Round(FListView.Height*4));
  try
    ABmp.Canvas.BeginScene;
    ABmp.BitmapScale := 2;
    FListView.Visible := True;
    FListView.PaintTo(ABmp.Canvas, RectF(0, 0, ABmp.Width, ABmp.Height));
    FListView.Visible := False;
    ABmp.Canvas.EndScene;
    FImage.Bitmap.Assign(ABmp);
  finally
    ABmp.Free;
  end;
  FListView.Visible := False;
  FImage.Visible := True;
  Application.ProcessMessages;
end;

procedure TksSlideMenuContainer.SwitchToMenu;
var
  AIndex: integer;
begin
  FListView.Height := (FSlideMenu.Owner as TForm).Height;
  FListView.Visible := True;
  AIndex := FSlideMenu.ItemIndex;
  FSlideMenu.UpdateMenu;
  if AIndex > -1 then
    FListView.Items[AIndex].BackgroundColor := FSlideMenu.Appearence.SelectedItemColor;
  FImage.Visible := False;
  Application.ProcessMessages;
  FListView.HitTest := True;
end;

{ TksSlideMenuToolbar }

constructor TksSlideMenuToolbar.Create(ASlideMenu: TksSlideMenu);
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  FFont := TFont.Create;
  FVisible := True;
  FSlideMenu := ASlideMenu;
  FFont.Size := C_DEFAULT_TOOLBAR_FONT_SIZE;
end;

destructor TksSlideMenuToolbar.Destroy;
begin
  FBitmap.Free;
  FFont.Free;
  inherited;
end;

procedure TksSlideMenuToolbar.DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
begin
  ACanvas.Fill.Color := FSlideMenu.Appearence.HeaderColor;
  ACanvas.FillRect(ARect, 0, 0, AllCorners, 1, ACanvas.Fill);
  ACanvas.Stroke.Color := FSlideMenu.Appearence.ItemColor;
  ACanvas.DrawLine(PointF(0, ARect.Bottom), PointF(ARect.Right, ARect.Bottom), 1, ACanvas.Stroke);
  if FBitmap.IsEmpty = False then
    ACanvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), GetImageRect, 1, True);

  ACanvas.Font.Assign(FFont);
  ACanvas.Fill.Color := FSlideMenu.Appearence.FontColor;
  ARect.Left := ARect.Left + (GetImageRect.Right+10);
  ACanvas.FillText(ARect, FText, False, 1, [], TTextAlign.Leading);
end;

function TksSlideMenuToolbar.GetImageRect: TRectF;
begin
  if FBitmap.IsEmpty then
    Result := RectF(0, 0, 0, 0)
  else
    Result := RectF(10, 10, 40, 40);
end;

procedure TksSlideMenuToolbar.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TksSlideMenuToolbar.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

end.


