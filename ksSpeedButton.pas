{*******************************************************************************
*                                                                              *
*  TksSpeedButton - TSpeedButton with iOS style badge                          *
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

unit ksSpeedButton;

interface

{$I ksComponents.inc}

uses Classes, FMX.StdCtrls, FMX.Graphics, ksControlBadge, ksTypes, FMX.Objects;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]

  TksSpeedButtonIcon = (Custom, AlarmClock, BarChart, Barcode, Bell, BookCover, BookCoverMinus, BookCoverPlus, BookMark, BookOpen,
                        Calendar, Camera, Car, Clock, CloudDownload, CloudUpload, Cross, Document, Download, Earth, Email,
                        Fax, FileList, FileMinus, FilePlus, Files, FileStar, FileTick, Flag, Folder, FolderMinus,
                        FolderPlus, FolderStar, Home, Inbox, Incoming, ListBullets, ListCheckBoxes, ListImages, ListNumbered, ListTicked,
                        Location, More, Note, Outgoing,
                        PaperClip, Photo, PieChart, Pin, Presentation, Search, Settings, Share, ShoppingCart, Spanner, Speaker,
                        Star, Tablet, Tag, Telephone, Telephone2, TelephoneBook, Tick, Timer, Trash, Upload,
                        User, UserEdit, UserGroup, Users, UserSearch,
                        VideoCamera, VideoPlayer, Viewer,
                        Wifi, Window, Write);



  TksSpeedButton = class(TksBaseSpeedButton)
  private
    FBadge: TksControlBadge;
    FIcon: TksSpeedButtonIcon;
    FImage: Timage;
    procedure SetBadge(Value: TksBadgeProperties);
    function GetBadge: TksBadgeProperties;
    procedure SetIcon(const Value: TksSpeedButtonIcon);
  protected
    procedure Resize; override;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Icon: TksSpeedButtonIcon read FIcon write SetIcon;
    property Badge: TksBadgeProperties read GetBadge write SetBadge;
  end;


  procedure Register;

implementation

uses Math, FMX.types, System.TypInfo, System.Types, ksCommon, System.UIConsts;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksSpeedButton]);
end;

{ TksSpeedButton }

constructor TksSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBadge := TksControlBadge.Create(Self);
  FIcon := Custom;
  FImage := TImage.Create(Self);
  FImage.HitTest := False;
  FImage.Locked := True;
  FImage.Stored := False;
  //StyledSettings := [];
  FImage.Align := TAlignLayout.Center;
  AddObjecT(FImage);
  AddObject(FBadge);
end;

destructor TksSpeedButton.Destroy;
begin
  {$IFDEF NEXTGEN}
  FBadge.DisposeOf;
  FImage.DisposeOf;
  {$ELSE}
  FBadge.Free;
  FImage.Free;
  {$ENDIF}
  inherited;
end;


function TksSpeedButton.GetBadge: TksBadgeProperties;
begin
  Result := FBadge.Properties;
end;

procedure TksSpeedButton.Resize;
begin
  inherited;
  FBadge.Position.X := (Width - FBadge.Width) - (Width * 0.1);
end;

function TksSpeedButton.GetDefaultStyleLookupName: string;
begin
  Result := GenerateStyleName(TSpeedButton.ClassName);
end;

procedure TksSpeedButton.SetIcon(const Value: TksSpeedButtonIcon);
var
  AStream: TResourceStream;
  AEnumName: String;
  ABmp: TBitmap;
begin
  if Value <> TksSpeedButtonIcon.Custom  then
  begin
    AEnumName := GetENumName(TypeInfo(TksSpeedButtonIcon), Ord(Value));
    AStream := TResourceStream.Create(HInstance, AEnumName, RT_RCDATA);
    try
      FImage.Bitmap := nil;
      ABmp := TBitmap.Create;
      ABmp.LoadFromStream(AStream);
      ABmp.Resize(Round(24 * GetScreenScale), Round(24 * GetScreenScale));
      FImage.Width := ABmp.Width;
      FImage.Height := ABmp.Height;
      {$IFDEF IOS}
      ReplaceOpaqueColor(ABmp, claDodgerblue);
      {$ENDIF}

      FImage.Bitmap := ABmp;
      ABmp.Free;
    finally
      AStream.Free;
    end;
  end;
  FIcon := Value;

end;

procedure TksSpeedButton.SetBadge(Value: TksBadgeProperties);
begin
  FBadge.Properties.Assign(Value);
end;


initialization

  Classes.RegisterClass(TksSpeedButton);

end.
