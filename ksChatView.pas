{*******************************************************************************
*                                                                              *
*  TksChatView - TksTableView wrapper for chat bubbles applications            *
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

unit ksChatView;

interface

{$I ksComponents.inc}

uses System.UITypes, FMX.Controls, FMX.Layouts, FMX.Objects, System.Classes,
  FMX.Types, Generics.Collections, FMX.Graphics, System.UIConsts, FMX.Effects,
  FMX.StdCtrls, System.Types, FMX.Forms, ksTableView, FMX.Edit,
  FMX.VirtualKeyboard, System.Messaging, ksTypes;


type
  TksChatView = class;

  TksChatBubbleInfo = record
    Text: string;
    Color: TAlphaColor;
    TextColor: TAlphaColor;
  end;

  TksChatViewPostText = procedure(Sender: TObject; var ABubble: TksChatBubbleInfo) of object;

  TksChatViewEdit = class(TToolBar)
  private
    [weak]FChatView: TksChatView;
    FEdit: TEdit;
    FSendButton: TButton;
    procedure EnterEdit(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]
  TksChatView = class(TksControl)
  private
    FTableView: TksTableView;
    FEdit: TksChatViewEdit;
    FMyImage: TBitmap;
    FSpacer: TLayout;
    FBeforePostText: TksChatViewPostText;
    procedure SetEditVisible(const Value: Boolean);
    function GetEditVisible: Boolean;
    procedure SetMyImage(const Value: TBitmap);
    procedure VirtualKeyboardChangeHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    function GetButtonText: string;
    procedure SetButtonText(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddChatBubble(AText: string;
                            APosition: TksTableViewChatBubblePosition;
                            AColor, ATextColor: TAlphaColor;
                            const AUserImage: TBitmap);
    procedure Clear;
  published
    property Align;
    property ButtonText: string read GetButtonText write SetButtonText;
    property MyImage: TBitmap read FMyImage write SetMyImage;
    property EditVisible: Boolean read GetEditVisible write SetEditVisible default True;
    property Position;
    property Size;
    // events...
    property BeforePostText: TksChatViewPostText read FBeforePostText write FBeforePostText;
  end;

  {$R *.dcr}

  procedure Register;

implementation

uses SysUtils, FMX.Platform, FMX.Ani;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksChatView]);
end;

{ TksChatView }

procedure TksChatView.AddChatBubble(AText: string; APosition: TksTableViewChatBubblePosition; AColor, ATextColor: TAlphaColor; const AUserImage: TBitmap);
var
  AInfo: TksChatBubbleInfo;
begin
  AInfo.Text := AText;
  AInfo.Color := AColor;
  AInfo.TextColor := ATextColor;
  if Assigned(FBeforePostText) then
    FBeforePostText(Self, AInfo);
  FTableView.Items.AddChatBubble(AInfo.Text, APosition, AInfo.Color, AInfo.TextColor, AUserImage);
  FTableView.ScrollToItem(FTableView.Items.LastItem, True);
end;

procedure TksChatView.Clear;
begin
  FTableView.ClearItems;
end;

procedure TksChatView.VirtualKeyboardChangeHandler(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  if TVKStateChangeMessage(Msg).KeyboardVisible then
  begin
    FSpacer.Height := TVKStateChangeMessage(Msg).KeyboardBounds.Height;
    FTableView.ScrollToItem(FTableView.Items.LastItem, True);
  end
  else
    FSpacer.Height := 0;
end;

constructor TksChatView.Create(AOwner: TComponent);
var
  VKToolbar: IFMXVirtualKeyboardToolbarService;
begin
  inherited;
  FTableView := TksTableView.Create(Self);
  FEdit := TksChatViewEdit.Create(Self);
  FSpacer := TLayout.Create(Self);
  FMyImage := TBitmap.Create;

  TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, VirtualKeyboardChangeHandler);
  // hide the done button
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, IInterface(VKToolbar)) then
    VKToolbar.SetToolbarEnabled(False);

  FTableView.Stored := False;
  FEdit.Stored := False;

  FTableView.Appearence.SeparatorColor := claNull;
  FTableView.SelectionOptions.ShowSelection := False;

  FSpacer.Align := TAlignLayout.MostBottom;
  FSpacer.Height := 0;
  Size.Width := 200;
  Size.Height := 300;

  FTableView.Align := TAlignLayout.Client;
  FEdit.Align := TAlignLayout.Bottom;
  AddObject(FTableView);
  AddObject(FEdit);
  AddObject(FSpacer);
end;

destructor TksChatView.Destroy;
begin
  FTableView.DisposeOf;
  FEdit.DisposeOf;

  FSpacer.DisposeOf;
  FreeAndNil(FMyImage);
  inherited;
end;

function TksChatView.GetButtonText: string;
begin
  Result := FEdit.FSendButton.Text;
end;

function TksChatView.GetEditVisible: Boolean;
begin
  Result := FEdit.Visible;
end;

procedure TksChatView.SetButtonText(const Value: string);
begin
  FEdit.FSendButton.Text := Value;
end;

procedure TksChatView.SetEditVisible(const Value: Boolean);
begin
  FEdit.Visible := Value;
end;

procedure TksChatView.SetMyImage(const Value: TBitmap);
begin
  FMyImage.Assign(Value);
end;

{ TksChatViewEdit }

constructor TksChatViewEdit.Create(AOwner: TComponent);
begin
  inherited;
  FChatView := TksChatView(AOwner);
  FEdit := TEdit.Create(Self);
  FSendButton := TButton.Create(Self);
  FSendButton.CanFocus := False;
  Padding.Rect := RectF(6, 6, 6, 6);
  FSendButton.Align := TAlignLayout.Right;
  FEdit.Align := TAlignLayout.Client;
  FSendButton.Margins.Left := 8;
  FSendButton.StyleLookup := 'listitembutton';
  FSendButton.Text := 'SEND';
  AddObject(FSendButton);
  AddObject(FEdit);
  FSendButton.OnClick := SendButtonClick;
  FEdit.OnEnter := EnterEdit;
  FEdit.OnKeyDown := EditKeyDown;
end;

destructor TksChatViewEdit.Destroy;
begin
  FEdit.DisposeOf;
  FSendButton.DisposeOf;
  inherited;
end;

procedure TksChatViewEdit.EditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then
    SendButtonClick(Self);
end;

procedure TksChatViewEdit.EnterEdit(Sender: TObject);
begin
  FChatView.FTableView.ScrollToItem(FChatView.FTableView.Items.LastItem, True);
end;

procedure TksChatViewEdit.SendButtonClick(Sender: TObject);
begin
  FChatView.AddChatBubble(FEdit.Text, ksCbpLeft, claDodgerblue, claWhite, FChatView.MyImage);
  FEdit.Text := '';
end;

end.


