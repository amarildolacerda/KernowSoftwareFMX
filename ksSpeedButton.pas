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

uses Classes, FMX.StdCtrls, FMX.Graphics, ksControlBadge;

{$IFDEF VER290}
{$DEFINE XE8_OR_NEWER}
{$ENDIF}
{$IFDEF VER300}
{$DEFINE XE8_OR_NEWER}
{$DEFINE XE10_OR_NEWER}
{$ENDIF}

type

  TksSpeedButton = class(TSpeedButton)
  private
    FBadge: TksControlBadge;
    procedure SetBadge(Value: TksBadgeProperties);
    function GetBadge: TksBadgeProperties;
  protected
    procedure Resize; override;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Badge: TksBadgeProperties read GetBadge write SetBadge;
  end;


  procedure Register;

implementation

uses Math;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksSpeedButton]);
end;

{ TksSpeedButton }

constructor TksSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBadge := TksControlBadge.Create(Self);
  StyledSettings := [];
  AddObject(FBadge);
end;

destructor TksSpeedButton.Destroy;
begin
  {$IFDEF NEXTGEN}
  FBadge.DisposeOf;
  {$ELSE}
  FBadge.Free;
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

procedure TksSpeedButton.SetBadge(Value: TksBadgeProperties);
begin
  FBadge.Properties.Assign(Value);
end;


initialization

  Classes.RegisterClass(TksSpeedButton);

end.
