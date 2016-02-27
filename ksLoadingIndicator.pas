{*******************************************************************************
*                                                                              *
*  TksLoadingIndicator - Loading indicator control                             *
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

unit ksLoadingIndicator;

interface

{$I ksComponents.inc}

uses
  System.SysUtils, System.Classes, ksTypes, FMX.Objects, FMX.StdCtrls,
  FMX.Types;

type
  TksLoadingIndicatorObject = class(TRectangle)
  private
    FLabel: TLabel;
    FArc: TArc;
    FTimer: TTimer;
    function GetText: string;
    procedure SetText(const Value: string);
    procedure DoTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text: string read GetText write SetText;
    procedure StartAnimation;
    procedure StopAnimation;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]
  TksLoadingIndicator = class(TksComponent)
  private
    FIndicator: TksLoadingIndicatorObject;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowLoading;
    procedure HideLoading;
  end;

procedure Register;

implementation


uses FMX.Graphics, FMX.Forms, System.UIConsts;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksLoadingIndicator]);
end;


{ TksLoadingIndicatorObject }

constructor TksLoadingIndicatorObject.Create(AOwner: TComponent);
begin
  inherited;
  FArc := TArc.Create(Self);
  FArc.Width := 40;
  FArc.Height := 40;
  FArc.Position.X := 30;
  FArc.Position.Y := 15;
  FArc.StartAngle := 90;
  FArc.EndAngle := 180;
  FArc.Stroke.Color := claLightgray;
  FArc.Stroke.Thickness := 3;
  FArc.Stroke.Kind := TBrushKind.Gradient;
  FArc.Stroke.Gradient.Color := claWhite;
  FArc.Stroke.Gradient.Color1 := claBlack;
  Width := 100;
  Height := 100;
  Align := TAlignLayout.Center;
  Visible := False;
  XRadius := 10;
  YRadius := 10;
  Fill.Color := claBlack;
  HitTest := False;
  Opacity := 1;
  FLabel := TLabel.Create(Self);
  FLabel.StyledSettings := [];
  FLabel.Font.Size := 14;
  FLabel.Text := 'LOADING...';
  FLabel.Height := 30;
  FLabel.Align := TAlignLayout.Bottom;
  FLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FLabel.TextSettings.FontColor := claWhite;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 25;
  FTimer.OnTimer := DoTimer;
  FTimer.Enabled := True;
  AddObject(FLabel);
  AddObject(FArc);
  Stored := False;
end;

destructor TksLoadingIndicatorObject.Destroy;
begin
  FTimer.DisposeOf;
  inherited;
end;

procedure TksLoadingIndicatorObject.DoTimer(Sender: TObject);
begin
  FArc.RotationAngle := FArc.RotationAngle+10;
end;

function TksLoadingIndicatorObject.GetText: string;
begin
  Result := FLabel.Text;
end;

procedure TksLoadingIndicatorObject.SetText(const Value: string);
begin
  FLabel.Text := Value;
end;

procedure TksLoadingIndicatorObject.StartAnimation;
begin
  FTimer.Enabled := True;
end;

procedure TksLoadingIndicatorObject.StopAnimation;
begin
  FTimer.Enabled := False;
end;

{ TksLoadingIndicator }

constructor TksLoadingIndicator.Create(AOwner: TComponent);
begin
  inherited;
  FIndicator := TksLoadingIndicatorObject.Create(nil);
end;

procedure TksLoadingIndicator.HideLoading;
var
  AOwner: TCustomForm;
begin
  AOwner := (Root.GetObject as TCustomForm);
  FIndicator.StopAnimation;
  AOwner.RemoveObject(FIndicator);

end;

procedure TksLoadingIndicator.ShowLoading;
var
  AOwner: TCustomForm;
begin
  AOwner := (Root.GetObject as TCustomForm);
  AOwner.AddObject(FIndicator);
  FIndicator.BringToFront;
  FIndicator.Visible := True;
  FIndicator.StartAnimation;
end;

end.
