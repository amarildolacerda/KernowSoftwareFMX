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

{.$DEFINE ADD_SAMPLE_MENU_ITEMS}


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

  C_DEFAULT_SELECTED_COLOR = claWhite;
  C_DEFAULT_FONT_COLOR = claBlack;
  C_DEFAULT_BACKGROUND_COLOR = claWhite;
  C_DEFAULT_HEADER_COLOR = claGainsboro;
  C_DEFAULT_TOOLBAR_COLOR = claWhite;

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
    FToolBarColor: TAlphaColor;
    procedure SetHeaderColor(const Value: TAlphaColor);
    procedure SetItemColor(const Value: TAlphaColor);
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
    procedure SetSelectedFontColor(const Value: TAlphaColor);
    procedure SetTheme(const Value: TKsMenuTheme);
    procedure SetToolBarColor(const Value: TAlphaColor);
  public
    constructor Create(ASlideMenu: TksSlideMenu); virtual;
  published
    property HeaderColor: TAlphaColor read FHeaderColor write SetHeaderColor default $FF323232;
    property ItemColor: TAlphaColor read FItemColor write SetItemColor default $FF222222;
    property FontColor: TAlphaColor read FFontColor write SetFontColor default claWhite;
    property SelectedItemColor: TAlphaColor read FSelectedColor write SetSelectedColor default claRed;
    property SelectedFontColor: TAlphaColor read FSelectedFontColor write SetSelectedFontColor default claWhite;
    property ToolBarColor: TAlphaColor read FToolBarColor write SetToolBarColor default $FF323232;
    property Theme: TKsMenuTheme read FTheme write SetTheme default mtDarkGray;
  end;


  TksSlideMenuToolbar = class(TPersistent)
  private
    FSlideMenu: TksSlideMenu;
    FHeader: TImage;
    FBitmap: TBitmap;
    FText: string;
    FFont: TFont;
    FVisible: Boolean;
    FHeight: integer;
    procedure SetFont(const Value: TFont);
    procedure SetBitmap(const Value: TBitmap);
    function GetBitmap: TBitmap;
    function GetFont: TFont;
    function GetText: string;
    procedure SetText(const Value: string);

  public
    constructor Create(AOwner: TComponent; ASlideMenu: TksSlideMenu); virtual;
    destructor Destroy; override;
    procedure UpdateToolbar;
    property Height: integer read FHeight default 44;
  published
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Visible: Boolean read FVisible write FVisible default True;
    property Text: string read GetText write SetText;
    property Font: TFont read GetFont write SetFont;
  end;


  TksSlideMenuContainer = class(TLayout)
  private
    FSlideMenu: TksSlideMenu;
    FToolBar: TksSlideMenuToolbar;
    FListView: TksListView;
    procedure UpdateSelectedItem;
    procedure CreateListView;
    function CalculateListViewHeight: single;
    procedure ItemClick(Sender: TObject; x, y: Single; AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ListView: TksListView read FListView;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidiOSDevice or pidAndroid)]
  TksSlideMenu = class(TFmxObject)
  strict private
    FAppearence: TksSlideMenuAppearence;
    FMenu: TksSlideMenuContainer;
    FItems: TksSlideMenuItems;
    {$IFNDEF ANDROID}
    FShadowLeft: TImage;
    FShadowRight: TImage;
    {$ENDIF}
    FBackground: TRectangle;
    FFormImage: TImage;
    FMenuImage: TImage;
    FFont: TFont;
    FShowing: Boolean;
    FTopPadding: integer;
    FMenuPosition: TksMenuPosition;
    FMenuStyle: TKsMenuStyle;
    FSlideSpeed: Single;
    FOnSelectMenuItemEvent: TSelectMenuItemEvent;
    FAfterSelectMenuItemEvent: TSelectMenuItemEvent;
    FOnAfterSlideOut: TNotifyEvent;
    {$IFDEF XE8_OR_NEWER}
    FImages: TCustomImageList;
    {$ENDIF}
    FHeaderHeight: integer;
    FItemHeight: integer;
    FToggleButton: TCustomButton;
    FAnimating: Boolean;
    procedure SetTopPadding(const Value: integer);
    procedure DoBackgroundClick(Sender: TObject);
    //procedure FadeBackground;
    //procedure UnfadeBackground;
    procedure GenerateFormImage(AXpos: single);
    procedure RemoveFormImage;
    {$IFNDEF ANDROID}
    procedure GenerateShadows;
    {$ENDIF}
    procedure HidePickers;
    procedure SetToggleButton(const Value: TCustomButton);
    procedure DoToggleButtonClick(Sender: TObject);
    
  private
    FItemIndex: integer;
    procedure MenuItemSelected(Sender: TObject; x, y: single; AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
    procedure SetItemIndex(const Value: integer);
    procedure SwitchMenuToImage;
    procedure SwitchImageToMenu;
    function GetToolbar: TksSlideMenuToolbar;
    function GetToolbarHeight: integer;
    procedure SetToolbar(const Value: TksSlideMenuToolbar);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddHeader(AText: string);
    function AddMenuItem(AId, AText: string; const AImageIndex: integer = -1): TksSlideMenuItem; overload;
    function AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem; overload;
    procedure ToggleMenu;
    procedure UpdateMenu;
    property Showing: Boolean read FShowing;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
  published
    property Appearence: TksSlideMenuAppearence read FAppearence write FAppearence;
    property Font: TFont read FFont write FFont;
    {$IFDEF XE8_OR_NEWER}
    property Images: TCustomImageList read FImages write FImages;
    {$ENDIF}
    property ItemHeight: integer read FItemHeight write FItemHeight default C_DEFAULT_ITEM_HEIGHT;
    property HeaderHeight: integer read FHeaderHeight write FHeaderHeight default C_DEFAULT_HEADER_HEIGHT;
    property TopPadding: integer read FTopPadding write SetTopPadding default 0;
    property MenuPosition: TksMenuPosition read FMenuPosition write FMenuPosition default mpLeft;
    property MenuStyle: TKsMenuStyle read FMenuStyle write FMenuStyle default msReveal;
    property SlideSpeed: Single read FSlideSpeed write FSlideSpeed;
    property OnSelectMenuItemEvent: TSelectMenuItemEvent read FOnSelectMenuItemEvent write FOnSelectMenuItemEvent;
    property AfterSelectItemEvent: TSelectMenuItemEvent read FAfterSelectMenuItemEvent write FAfterSelectMenuItemEvent;
    property Toolbar: TksSlideMenuToolbar read GetToolbar write SetToolbar;
    property ToggleButton: TCustomButton read FToggleButton write SetToggleButton;
    property OnAfterSlideOut: TNotifyEvent read FOnAfterSlideOut write FOnAfterSlideOut;

  end;

  procedure Register;

implementation

uses FMX.Platform, SysUtils, FMX.Ani, FMX.Pickers, Math;


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

function GetColorOrDefault(AColor, ADefaultIfNull: TAlphaColor): TAlphaColor;
begin
  Result := AColor;
  if Result = claNull then
    Result := ADefaultIfNull;
end;

{ TSlideMenu }

procedure TksSlideMenu.AddHeader(AText: string);
var
  AResult: TksSlideMenuItem;
begin
  AResult := FItems.AddHeaderItem(AText);
  AResult.Font.Assign(FFont);
end;


function TksSlideMenu.AddMenuItem(AId, AText: string; AImage: TBitmap): TksSlideMenuItem;
begin
  Result := FItems.AddMenuItem(AId, AText, AImage);
  Result.Font.Assign(FFont);
  UpdateMenu;
end;

function TksSlideMenu.AddMenuItem(AId, AText: string; const AImageIndex: integer = -1): TksSlideMenuItem;
var
  AImage: TBitmap;
  ASize: TSizeF;
begin
  AImage := nil;
  ASize.Width := 64;
  ASize.Height := 64;
  {$IFDEF XE8_OR_NEWER}
  if Images <> nil then
    AImage := Images.Bitmap(ASize, AImageIndex);
  {$ENDIF}
  Result := AddMenuItem(AId, AText, AImage);
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
  {$IFNDEF ANDROID}
  FShadowLeft := TImage.Create(Self);
  FShadowRight := TImage.Create(Self);
  {$ENDIF}
  FAppearence := TksSlideMenuAppearence.Create(Self);
  FMenu := TksSlideMenuContainer.Create(Self);
  FFormImage := TImage.Create(Self);
  FBackground := TRectangle.Create(Self);

  FShowing := False;
  FTopPadding := 0;
  FFont.Size := C_DEFAULT_FONT_SIZE;
  FFormImage.OnClick := DoBackgroundClick;
  FMenuPosition := mpLeft;
  FMenuStyle := msReveal;
  FSlideSpeed := C_DEFAULT_SLIDE_SPEED;
  FHeaderHeight := C_DEFAULT_HEADER_HEIGHT;
  FItemHeight := C_DEFAULT_ITEM_HEIGHT;
  FItemIndex := -1;
  {$IFNDEF ANDROID}
  GenerateShadows;
  {$ENDIF}
  FAnimating := False;
end;

destructor TksSlideMenu.Destroy;
begin
  FFont.Free;
  FAppearence.Free;
  FItems.Free;
  if IsChild(FMenu) then FMenu.Free;

  {$IFNDEF ANDROID}
  if IsChild(FShadowLeft) then FShadowLeft.Free;
  if IsChild(FShadowRight) then FShadowRight.Free;
  {$ENDIF}
  if IsChild(FBackground) then FBackground.Free;
  if IsChild(FFormImage) then FFormImage.Free;
  inherited;
end;


procedure TksSlideMenu.DoBackgroundClick(Sender: TObject);
begin
  ToggleMenu;
end;

procedure TksSlideMenu.DoToggleButtonClick(Sender: TObject);
begin
  ToggleMenu;
end;

procedure TksSlideMenu.GenerateFormImage(AXpos: single);
var
  AScale: single;
  ABmp: TBitmap;
  AForm: TForm;
begin
  AForm := (Owner as TForm);
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
    {$IFDEF IOS}
    ABmp.DisposeOf;
    {$ELSE}
    ABmp.Free;
    {$ENDIF}
  end;

  FFormImage.Position.Y := 0;
  FFormImage.Position.X := AXpos;

  {$IFNDEF ANDROID}
  FShadowLeft.Position.Y := 0;
  FShadowLeft.Position.X := 0-FShadowLeft.Width;
  FFormImage.AddObject(FShadowLeft);

  FShadowRight.Position.Y := 0;
  FShadowRight.Position.X := FFormImage.Width;
  FFormImage.AddObject(FShadowRight);

  {$ENDIF}

  AForm.AddObject(FFormImage);
  FFormImage.Visible := True;
  FMenu.Visible := True;
  Application.ProcessMessages;
end;

procedure TksSlideMenu.RemoveFormImage;
var
  AForm: TForm;
begin
  AForm := (Owner as TForm);
  if AForm <> nil then
    AForm.RemoveObject(FFormImage);

end;

{$IFNDEF ANDROID}

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
    {$IFDEF IOS}
    ABmp.DisposeOf;
    {$ELSE}
    ABmp.Free;
    {$ENDIF}
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
    {$IFDEF IOS}
    ABmp.DisposeOf;
    {$ELSE}
    ABmp.Free;
    {$ENDIF}
  end;
end;

{$ENDIF}

function TksSlideMenu.GetToolbar: TksSlideMenuToolbar;
begin
  Result := FMenu.FToolBar;
end;

function TksSlideMenu.GetToolbarHeight: integer;
begin
  Result := 0;
  if Toolbar.Visible then
    Result := C_TOOLBAR_HEIGHT;
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
  FItemIndex := AItem.Index;
  if Assigned(FOnSelectMenuItemEvent) then
    FOnSelectMenuItemEvent(Self, AId);
  Application.ProcessMessages;
  GenerateFormImage(FFormImage.Position.X);
  Application.ProcessMessages;
  ToggleMenu;
end;



procedure TksSlideMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FToggleButton) and (Operation = TOperation.opRemove) then
    FToggleButton := nil;
end;

procedure TksSlideMenu.UpdateMenu;
var
  ICount: integer;
  AItem: TksSlideMenuItem;
  ARow: TKsListItemRow;
  lv: TksListView;
  ASelectedColor: TAlphaColor;
  AFontColor: TAlphaColor;
begin
  if FMenu.ListView = nil then
    Exit;
  Application.ProcessMessages;
  lv := FMenu.FListView;

  lv.Position.X := 0;
  lv.Position.Y := 0;
  lv.Repaint;

  lv.Position.Y := GetToolbarHeight;;

  lv.Width := C_MENU_WIDTH;


  FMenu.Width := C_MENU_WIDTH;
  AFontColor := GetColorOrDefault(FAppearence.FontColor, C_DEFAULT_FONT_COLOR);
  ASelectedColor := GetColorOrDefault(FAppearence.SelectedItemColor, C_DEFAULT_SELECTED_COLOR);

  lv.Appearence.Background := GetColorOrDefault(FAppearence.ItemColor, C_DEFAULT_BACKGROUND_COLOR);
  lv.Appearence.ItemBackground := GetColorOrDefault(FAppearence.ItemColor, C_DEFAULT_BACKGROUND_COLOR);
  lv.Appearence.HeaderColor := GetColorOrDefault(FAppearence.HeaderColor, C_DEFAULT_HEADER_COLOR);
  lv.Appearence.SeparatorColor := GetColorOrDefault(FAppearence.HeaderColor, C_DEFAULT_HEADER_COLOR);
  lv.Appearence.SelectedColor := ASelectedColor;





  lv.HeaderHeight := FHeaderHeight;
  lv.ItemHeight := FItemHeight;
  lv.ItemImageSize := 24;
  lv.BeginUpdate;
  try
    lv.Items.Clear;

    {$IFDEF ADD_SAMPLE_MENU_ITEMS}
    if FItems.Count = 0 then
    begin
      // add some sample items...
      AddMenuItem('', 'Menu Item 1');
      AddMenuItem('', 'Menu Item 2');
      AddHeader('SAMPLE HEADER 1');
      AddMenuItem('', 'Menu Item 3');
      AddMenuItem('', 'Menu Item 4');
      AddMenuItem('', 'Menu Item 5');
      AddHeader('SAMPLE HEADER 2');
      AddMenuItem('', 'Menu Item 6');
      AddMenuItem('', 'Menu Item 7');
      AddMenuItem('', 'Menu Item 8');
      AddMenuItem('', 'Menu Item 9');
      AddMenuItem('', 'Menu Item 10');
    end;
    {$ENDIF}

    for ICount := 0 to FItems.Count-1 do
    begin
      AItem := FItems[ICount];
      if AItem.IsHeader then
        ARow := lv.Items.AddHeader(AItem.Text)
      else
      begin
        ARow := lv.Items.AddRow(AItem.Text, '', '', More);
        ARow.Image.Bitmap.Assign(AItem.Image);
        ARow.ID := AItem.ID;

      end;
      ARow.Title.TextColor := AFontColor;
    end;
  finally
    lv.EndUpdate;
  end;
  if FItemIndex = -1 then
  begin
    lv.SelectFirstItem;
    FItemIndex := lv.ItemIndex;
    lv.ScrollTo(FItemIndex);
  end;
  if (FItemIndex > -1) and (FItemIndex <= lv.Items.Count-1) then
    lv.Items[FItemIndex].BackgroundColor := ASelectedColor;
end;

procedure TksSlideMenu.SetItemIndex(const Value: integer);
begin
  FItemIndex := Value;
end;

procedure TksSlideMenu.SetToggleButton(const Value: TCustomButton);
begin
  FToggleButton := Value;
  FToggleButton.OnClick := DoToggleButtonClick;
end;

procedure TksSlideMenu.SetToolbar(const Value: TksSlideMenuToolbar);
begin
  FMenu.FToolBar := Value;
end;

procedure TksSlideMenu.SetTopPadding(const Value: integer);
begin
  FTopPadding := Value;
end;

procedure TksSlideMenu.SwitchMenuToImage;
var
  ABmp: TBitmap;
  AForm: TForm;
  ABottom: single;
begin
  AForm := (Owner as TForm);
  ABmp := TBitmap.Create(Round(C_MENU_WIDTH * GetScreenScale), Round(AForm.ClientHeight * GetScreenScale));
  try
    FMenuImage := TImage.Create(AForm);
    FMenuImage.Width := C_MENU_WIDTH;
    FMenuImage.Height := AForm.ClientHeight;
    case FMenuPosition of
      mpLeft: FMenuImage.Position.X := 0;
      mpRight: FMenuImage.Position.X := AForm.Width - C_MENU_WIDTH;
    end;
    ABmp.Canvas.BeginScene;
    ABmp.BitmapScale := GetScreenScale;
    FMenu.PaintTo(ABmp.Canvas, RectF(0, 0, ABmp.Width, ABmp.Height));
    ABmp.Canvas.EndScene;

    ABmp.Canvas.BeginScene;

    ABottom := (GetToolbarHeight + FMenu.CalculateListViewHeight)*GetScreenScale;
    ABmp.Canvas.Fill.Color := FAppearence.ItemColor;
    ABmp.Canvas.FillRect(RectF(0, ABottom, C_MENU_WIDTH*GetScreenScale, ABmp.Height), 0, 0, AllCorners, 1, ABmp.Canvas.Fill);
    ABmp.Canvas.EndScene;
    FMenuImage.Bitmap := ABmp;
  finally
    ABmp.Free;
  end;
end;

procedure TksSlideMenu.SwitchImageToMenu;
var
  AForm: TForm;
begin
  AForm := (Owner as TForm);
  AForm.InsertObject(AForm.ChildrenCount-1, FMenu);
  AForm.RemoveObject(FMenuImage);
  FMenu.HitTest := True;
end;

procedure TksSlideMenu.ToggleMenu;
var
  AStartXPos: single;
  ANewXPos: single;
  AForm: TForm;
begin
  if FAnimating then
    Exit;
  FAnimating := True;
  try
    if (FShowing = False) then
      FMenu.FToolBar.UpdateToolbar;

    FMenu.HitTest := False;

    AForm := (Owner as TForm);

    HidePickers;

    if FShowing = False then
    begin
      AStartXPos := 0;
      ANewXPos := 0;
      case FMenuPosition of
        mpLeft:  ANewXPos := C_MENU_WIDTH;
        mpRight: ANewXPos := 0-C_MENU_WIDTH;
      end;


    end
    else
    begin
      AStartXPos := FFormImage.Position.X;
      ANewXPos := 0;
    end;

    GenerateFormImage(AStartXPos);

    FMenu.Height := (Owner as TForm).ClientHeight;

    // add the menu just behind the screen image...

    case FMenuPosition of
      mpLeft : FMenu.Position.X := 0;
      mpRight: FMenu.Position.X := AForm.Width - C_MENU_WIDTH;
    end;
    AForm.InsertObject(0, FMenu);

    if FMenu.FListView.Items.Count = 0 then
      UpdateMenu;

    SwitchMenuToImage;
    AForm.RemoveObject(FMenu);
    //FMenu.Position.X := 0;
    AForm.InsertObject(AForm.ChildrenCount-1, FMenuImage);

    Application.ProcessMessages;

    FFormImage.HitTest := False;
    TAnimator.AnimateFloatWait(FFormImage, 'Position.X', ANewXPos, FSlideSpeed);
    FFormImage.HitTest := True;

    FShowing := not FShowing;

    if FShowing = False then
    begin
      AForm.RemoveObject(FMenu);
      RemoveFormImage;
    end
    else
      SwitchImageToMenu;
    AForm.RemoveObject(FMenuImage);
    Application.ProcessMessages;
  finally
    FAnimating := False;
  end;
  if FShowing = False then
  begin
    if Assigned(FOnAfterSlideOut) then
      FOnAfterSlideOut(Self);
  end;
end;

{procedure TksSlideMenu.FadeBackground;
begin
  FBackground.Fill.Color := claBlack;
  FBackground.Align := TAlignLayout.Contents;
  FBackground.OnClick := DoBackgroundClick;
  FBackground.Opacity := 0;
  TForm(Owner).AddObject(FBackground);
  FBackground.BringToFront;
  TAnimator.AnimateFloat(FBackground, 'Opacity', 0.2, FSlideSpeed);
end;

procedure TksSlideMenu.UnfadeBackground;
begin
  TAnimator.AnimateFloat(FBackground, 'Opacity', 0, FSlideSpeed);
  TForm(Owner).RemoveObject(FBackground);
end; }

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
  {$IFDEF IOS}
  FImage.DisposeOf;
  FFont.DisposeOf;
  {$ELSE}
  FImage.Free;
  FFont.Free;
  {$ENDIF}
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

constructor TksSlideMenuAppearence.Create(ASlideMenu: TksSlideMenu);
begin
  FSlideMenu := ASlideMenu;
  FHeaderColor := $FF323232;
  FItemColor := $FF222222;
  FToolBarColor := $FF323232;
  FFontColor := claWhite;
  FSelectedFontColor := claWhite;
  FSelectedColor := claRed;
  FTheme := mtDarkGray;
end;

procedure TksSlideMenuAppearence.SetFontColor(const Value: TAlphaColor);
begin
  FFontColor := Value;
  FTheme := mtCustom;

end;

procedure TksSlideMenuAppearence.SetHeaderColor(const Value: TAlphaColor);
begin
  FHeaderColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetItemColor(const Value: TAlphaColor);
begin
  FItemColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetSelectedColor(const Value: TAlphaColor);
begin
  FSelectedColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetSelectedFontColor(const Value: TAlphaColor);
begin
  FSelectedFontColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetTheme(const Value: TKsMenuTheme);
begin
  if Value = mtDarkGray then
  begin
    FHeaderColor := $FF323232;
    FToolBarColor := $FF323232;
    FItemColor := $FF222222;
    FFontColor := claWhite;
    FSelectedFontColor := claWhite;
    FSelectedColor := claRed;
  end;
  if Value = mtDarkBlue then
  begin
    FHeaderColor := $FF001C6F;
    FToolBarColor := $FF001C6F;
    FItemColor := $FF001451;
    FFontColor := claWhite;
    FSelectedFontColor := claWhite;
    FSelectedColor := claRed;
  end;
  FTheme := Value;
end;

procedure TksSlideMenuAppearence.SetToolBarColor(const Value: TAlphaColor);
begin
  FToolBarColor := Value;
  FTheme := mtCustom;
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
  FSlideMenu := (AOwner as TksSlideMenu);
  FToolBar := TksSlideMenuToolbar.Create(Self, FSlideMenu);
  CreateListView;
  ClipChildren := True;
  HitTest := True;
end;

procedure TksSlideMenuContainer.CreateListView;
begin
  FListView := TksListView.Create(Self);
  FListView.KeepSelection := True;
  FListView.CanSwipeDelete := False;
  FListView.ShowSelection := False;
  FListView.OnItemClick := ItemClick;
  FListView.Align := TAlignLayout.Client;
  AddObject(FListView);

end;

destructor TksSlideMenuContainer.Destroy;
begin
  {$IFDEF IOS}
  FToolBar.DisposeOf;
  if IsChild(FListView) then FListView.DisposeOf;
  {$ELSE}
  FListView.Free;
  if IsChild(FListView) then FListView.Free;
  {$ENDIF}
  inherited;
end;

procedure TksSlideMenuContainer.ItemClick(Sender: TObject; x, y: Single;
  AItem: TKsListItemRow; AId: string; ARowObj: TksListItemRowObj);
begin
  if HitTest = False then
    Exit;
  HitTest := False;
  UpdateSelectedItem;
  Sleep(200);

  FSlideMenu.MenuItemSelected(Self, x, y, AItem, AId, ARowObj);
end;

procedure TksSlideMenuContainer.UpdateSelectedItem;
var
  ICount: integer;
begin
  for ICount := 0 to FListView.Items.Count-1 do
  begin
    if FListView.ItemIndex = ICount then
      FListView.Items[ICount].BackgroundColor := GetColorOrDefault(FSlideMenu.Appearence.SelectedItemColor, C_DEFAULT_SELECTED_COLOR)
    else
      FListView.Items[ICount].BackgroundColor := FSlideMenu.Appearence.ItemColor;
  end;
  Application.ProcessMessages;
end;

{ TksSlideMenuToolbar }

constructor TksSlideMenuToolbar.Create(AOwner: TComponent; ASlideMenu: TksSlideMenu);
begin
  inherited Create;
  FSlideMenu := ASlideMenu;
  FBitmap := TBitmap.Create;
  FFont := TFont.Create;

  FHeader := TImage.Create(AOwner);
  FHeader.Position.X := 0;
  FHeader.Position.Y := 0;
  FHeader.Width := C_MENU_WIDTH;
  FHeader.Height := C_TOOLBAR_HEIGHT;
  FHeader.Align := TAlignLayout.Top;
  FHeader.HitTest := False;
  FVisible := True;
  (AOwner as TFmxObject).AddObject(FHeader);
end;

destructor TksSlideMenuToolbar.Destroy;
begin
  {$IFDEF IOS}
  FBitmap.DisposeOf;
  FFont.DisposeOf;
  FHeader.DisposeOf;
  {$ELSE}
  FBitmap.Free;
  FFont.Free;
  FHeader.Free;
  {$ENDIF}
  inherited;
end;

function TksSlideMenuToolbar.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

function TksSlideMenuToolbar.GetFont: TFont;
begin
  Result := FFont;
end;

function TksSlideMenuToolbar.GetText: string;
begin
  Result := FText;
end;

procedure TksSlideMenuToolbar.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TksSlideMenuToolbar.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TksSlideMenuToolbar.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TksSlideMenuToolbar.UpdateToolbar;
var
  ABmp: TBitmap;
  AXPos: integer;
begin
  if not Visible then
  begin
    FHeader.Height := 0;
    Exit;
  end;
  ABmp := TBitmap.Create(Round(C_MENU_WIDTH*GetScreenScale), Round(C_TOOLBAR_HEIGHT*GetScreenScale));
  try
    ABmp.BitmapScale := GetScreenScale;
    AXPos := 10;
    ABmp.Clear(FSlideMenu.Appearence.ToolBarColor);
    ABmp.Canvas.BeginScene;
    if FBitmap.IsEmpty = False then
    begin
      ABmp.Canvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height), RectF(10, 10, 40, 40), 1);
      AXPos := 50;
    end;
    ABmp.Canvas.Fill.Color := FSlideMenu.Appearence.FFontColor;

    ABmp.Canvas.Font.Assign(FFont);
    ABmp.Canvas.FillText(RectF(AXPos, 0, C_MENU_WIDTH, C_TOOLBAR_HEIGHT), FText, False, 1, [], TTextAlign.Leading);
    ABmp.Canvas.EndScene;

    FHeader.Bitmap := ABmp;
  finally
    ABmp.Free;
  end;
end;

end.


