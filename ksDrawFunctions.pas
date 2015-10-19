{*******************************************************************************
*                                                                              *
*  ksDrawFunctions - control drawing functions for ksListView                  *
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

Once installed, simply uncomment the conditional define below

}

//{$DEFINE USE_TMS_HTML_ENGINE}

unit ksDrawFunctions;

interface

uses Classes, FMX.Graphics, FMX.Types, Types, System.UIConsts, System.UITypes,
  FMX.ListView.Types, Generics.Collections, FMX.Platform;


type
  TksButtonStyle = (ksButtonDefault, ksButtonSegmentLeft, ksButtonSegmentMiddle, ksButtonSegmentRight);

  function GetScreenScale: single;

  function IsBlankBitmap(ABmp: TBitmap; const ABlankColor: TAlphaColor = claNull): Boolean;
  function GetColorOrDefault(AColor, ADefaultIfNull: TAlphaColor): TAlphaColor;

  procedure DrawSwitch(ACanvas: TCanvas; ARect: TRectF; AChecked, AEnabled: Boolean; ASelectedColor: TAlphaColor);
  procedure DrawButton(ACanvas: TCanvas; ARect: TRectF; AText: string; ASelected: Boolean; AColor: TAlphaColor; AStyle: TksButtonStyle);

  {procedure DrawAccessory(ACanvas: TCanvas; ARect: TRectF; AColor: TAlphaColor; AAccessory: TAccessoryType);
  procedure DrawCheckMarkAccessory(ACanvas: TCanvas; ARect: TRectF; AColor: TAlphaColor);
  procedure DrawMoreAccessory(ACanvas: TCanvas; ARect: TRectF; AColor: TAlphaColor);
  }
  function TextWidth(AText: string; AFont: TFont): single;
  function TextHeight(AText: string; AFont: TFont; AWordWrap: Boolean; const AWidth: single = 0): single;

  function TextSizeHtml(AText: string; AFont: TFont; const AWidth: single = 0): TPointF;


  procedure RenderText(ACanvas: TCanvas;
                       x, y,
                       AWidth, AHeight: single;
                       AText: string;
                       AFont: TFont;
                       ATextColor: TAlphaColor;
                       AWordWrap: Boolean;
                       AHorzAlign: TTextAlign;
                       AVertAlign: TTextAlign;
                       ATrimming: TTextTrimming);


  procedure RenderHhmlText(ACanvas: TCanvas;
                           x, y,
                           AWidth, AHeight: single;
                           AText: string;
                           AFont: TFont;
                           ATextColor: TAlphaColor;
                           AWordWrap: Boolean;
                           AHorzAlign: TTextAlign;
                           AVertAlign: TTextAlign;
                           ATrimming: TTextTrimming);





implementation

uses SysUtils, FMX.TextLayout, Math, FMX.Objects, FMX.Styles, FMX.Styles.Objects,

  FMX.Forms {$IFDEF USE_TMS_HTML_ENGINE} , FMX.TMSHTMLEngine {$ENDIF};

var
  _ScreenScale: single;
  ATextLayout: TTextLayout;

function GetColorOrDefault(AColor, ADefaultIfNull: TAlphaColor): TAlphaColor;
begin
  Result := AColor;
  if Result = claNull then
    Result := ADefaultIfNull;
end;

function GetScreenScale: single;
var
  Service: IFMXScreenService;
begin
  if _ScreenScale > 0 then
  begin
    Result := _ScreenScale;
    Exit;
  end;
  Service := IFMXScreenService(TPlatformServices.Current.GetPlatformService
    (IFMXScreenService));
  Result := Service.GetScreenScale;
  _ScreenScale := Result;
end;

function TextSizeHtml(AText: string; AFont: TFont; const AWidth: single = 0): TPointF;
{$IFDEF USE_TMS_HTML_ENGINE}
var
  AnchorVal,
  StripVal,
  FocusAnchor: string;
  XSize,
  YSize: single;
  HyperLinks,
  MouseLink: Integer;
  HoverRect:TRectF;
  ABmp: TBitmap;
  {$ENDIF}
begin
  Result := PointF(0, 0);
  {$IFDEF USE_TMS_HTML_ENGINE}

  XSize := AWidth;

  if XSize <= 0 then
    XSize := MaxSingle;

  ABmp := TBitmap.Create(10, 10);
  try
    ABmp.BitmapScale := GetScreenScale;
    ABmp.Canvas.Assign(AFont);
    {$IFDEF USE_TMS_HTML_ENGINE}
    HTMLDrawEx(ABmp.Canvas, AText, RectF(0, 0, XSize, MaxSingle), 0,0, 0, 0, 0, False,
               False, False, False, False, False, False, 1, claNull,
               claNull, claNull, claNull, AnchorVal, StripVal, FocusAnchor, XSize, YSize, HyperLinks, MouseLink,
               HoverRect, 1, nil, 1);
    Result := PointF(XSize, YSize);
    {$ELSE}
    Result := PointF(0, 0);
    {$ENDIF}
  finally
    FreeAndNil(ABmp);
  end;
  {$ENDIF}
end;

function TextWidth(AText: string; AFont: TFont): single;
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
    ATextLayout.Font.Assign(AFont);
    ATextLayout.HorizontalAlign := TTextAlign.Leading;
    ATextLayout.EndUpdate;
    Result := ATextLayout.Width;
end;


function TextHeight(AText: string; AFont: TFont; AWordWrap: Boolean; const AWidth: single = 0): single;
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
  ATextLayout.Font.Assign(AFont);
  ATextLayout.HorizontalAlign := TTextAlign.Leading;
  ATextLayout.VerticalAlign := TTextAlign.Leading;
  ATextLayout.EndUpdate;
  Result := ATextLayout.TextHeight;
end;

procedure RenderText(ACanvas: TCanvas;
                     x, y,
                     AWidth, AHeight: single;
                     AText: string;
                     AFont: TFont;
                     ATextColor: TAlphaColor;
                     AWordWrap: Boolean;
                     AHorzAlign: TTextAlign;
                     AVertAlign: TTextAlign;
                     ATrimming: TTextTrimming);
begin
  ATextLayout.BeginUpdate;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := AWordWrap;
  ATextLayout.Font.Assign(AFont);
  ATextLayout.Color := ATextColor;
  ATextLayout.HorizontalAlign := AHorzAlign;
  ATextLayout.VerticalAlign := AVertAlign;
  ATextLayout.Trimming := ATrimming;
  ATextLayout.TopLeft := PointF(x, y);
  ATextLayout.MaxSize := PointF(AWidth, AHeight);
  ATextLayout.EndUpdate;
  ATextLayout.RenderLayout(ACanvas);
end;

procedure RenderHhmlText(ACanvas: TCanvas;
                           x, y,
                           AWidth, AHeight: single;
                           AText: string;
                           AFont: TFont;
                           ATextColor: TAlphaColor;
                           AWordWrap: Boolean;
                           AHorzAlign: TTextAlign;
                           AVertAlign: TTextAlign;
                           ATrimming: TTextTrimming);
{$IFDEF USE_TMS_HTML_ENGINE}
var
  AnchorVal,
  StripVal,
  FocusAnchor: string;
  XSize,
  YSize: single;
  HyperLinks,
  MouseLink: Integer;
  HoverRect:TRectF;
  {$ENDIF}
begin
  {$IFDEF USE_TMS_HTML_ENGINE}
  ACanvas.Fill.Color := ATextColor;
  ACanvas.Font.Assign(AFont);
  HTMLDrawEx(ACanvas, AText, RectF(x,y , x+AWidth, y+AHeight), 0,0, 0, 0, 0, False,
             False, True, False, False, False, AWordWrap, 1, claNull,
             claNull, claNull, claNull, AnchorVal, StripVal, FocusAnchor, XSize, YSize, HyperLinks, MouseLink,
             HoverRect, 1, nil, 1);
  {$ELSE}
  AFont.Size := 10;
  RenderText(ACanvas, x, y, AWidth, AHeight, 'Requires TMS FMX', AFont, ATextColor, AWordWrap, AHorzAlign, AVertAlign, ATrimming);
  {$ENDIF}
end;

function IsBlankBitmap(ABmp: TBitmap; const ABlankColor: TAlphaColor = claNull): Boolean;
var
  ABlank: TBitmap;
begin
  ABlank := TBitmap.Create(ABmp.Width, ABmp.Height);
  try
    ABlank.Clear(ABlankColor);
    Result := ABmp.EqualsBitmap(ABlank);
  finally
    FreeAndNil(ABlank);
  end;
end;



procedure DrawSwitch(ACanvas: TCanvas; ARect: TRectF; AChecked, AEnabled: Boolean; ASelectedColor: TAlphaColor);
var
  ABmp: TBitmap;
  r: TRectF;
  ASwitchRect: TRectF;
begin
  ABmp := TBitmap.Create(Round(ARect.Width * GetScreenScale), Round(ARect.Height * GetScreenScale));
  try
    ABmp.Clear(claNull);
    ABmp.BitmapScale := GetScreenScale;

    ABmp.Canvas.BeginScene;
    ABmp.Canvas.StrokeThickness := GetScreenScale;

    r := RectF(0, 0, ABmp.Height, ABmp.Height);
    if not AChecked then
      ASwitchRect := r;
    ABmp.Canvas.Stroke.Color := claSilver;
    ABmp.Canvas.Fill.Color := claWhite;
    if AChecked then
      Abmp.Canvas.Fill.Color := ASelectedColor;

    if AEnabled = False then
      ABmp.Canvas.Fill.Color := claGainsboro;


    ABmp.Canvas.FillEllipse(r, 1, ABmp.Canvas.Fill);
    ABmp.Canvas.DrawEllipse(r, 1, ABmp.Canvas.Stroke);
    OffsetRect(r, ABmp.Width-r.Height, 0);
    if AChecked then
      ASwitchRect := r;

    ABmp.Canvas.FillEllipse(r, 1, ABmp.Canvas.Fill);
    ABmp.Canvas.DrawEllipse(r, 1, ABmp.Canvas.Stroke);

    //ABmp.Canvas.Fill.Color := claWhite;
    ABmp.Canvas.FillRect(RectF(0  + (r.Width/2), 0, ABmp.Width - (r.Width/2), ABmp.Height), 0, 0,  AllCorners, 1, ABmp.Canvas.Fill);

    r := RectF(ABmp.Height/2, 0, ABmp.Width-(ABmp.Height/2), ABmp.Height);


    ABmp.Canvas.FillRect(r, 0, 0, AllCorners, 1, ABmp.Canvas.Fill);
    ABmp.Canvas.StrokeThickness := 3;
    r.Bottom := r.Bottom -1;
    r.Left := r.Left - (GetScreenScale*4);
    r.Right := r.Right + (GetScreenScale*4);
    ABmp.Canvas.DrawRectSides(r, 0, 0, AllCorners, 1, [TSide.Top, TSide.Bottom], ABmp.Canvas.Stroke);
    ABmp.Canvas.StrokeThickness := 2;

    ABmp.Canvas.Fill.Color := claWhite;
    if AEnabled = False then
    begin
      ABmp.Canvas.Fill.Color := $FFEEEEEE;
    end;
    if AChecked then
    begin
      InflateRect(ASwitchRect, -2, -2);
      ABmp.Canvas.FillEllipse(ASwitchRect, 1, ABmp.Canvas.Fill);
    end
    else
    begin
      InflateRect(ASwitchRect, -1, -1);
      ABmp.Canvas.Stroke.Color := claSilver;
      ABmp.Canvas.DrawEllipse(ASwitchRect, 1, ABmp.Canvas.Stroke);
    end;
    ABmp.Canvas.EndScene;
    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ARect, 1, False);
  finally
    FreeAndNil(ABmp);
  end;
end;

procedure DrawButton(ACanvas: TCanvas; ARect: TRectF; AText: string; ASelected: Boolean; AColor: TAlphaColor; AStyle: TksButtonStyle);
var
  ABmp: TBitmap;
  r: TRectF;
  ARadius: single;
  AFill, AOutline, AFontColor: TAlphaColor;
  AScale: single;
begin
  AScale := 2;
  ARadius := 5*AScale;
  ABmp := TBitmap.Create(Round(ARect.Width * AScale), Round(ARect.Height * AScale));
  try
    if AColor = claNull then
      AColor := claDodgerblue;

    ABmp.Clear(claNull);
    ABmp.BitmapScale := AScale;
    r := RectF(0, 0, ABmp.Width, ABmp.Height);
    ABmp.Canvas.BeginScene;
    ABmp.Canvas.StrokeThickness := AScale;
    ABmp.Canvas.Stroke.Color := claSilver;
    ABmp.Canvas.Font.Size := (13 * AScale);

    if ASelected then
    begin
      AFill := AColor;
      AOutline := AColor;
      AFontColor := claWhite;
    end
    else
    begin
      AFill := claWhite;
      AOutline := AColor;
      AFontColor := AColor;
    end;
    ABmp.Canvas.Blending := True;
    ABmp.Canvas.Fill.Color := AFill;
    ABmp.Canvas.Stroke.Color := AOutline;
    if AStyle = ksButtonSegmentLeft then
    begin
      ABmp.Canvas.FillRect(r, ARadius, ARadius, [TCorner.TopLeft, TCorner.BottomLeft], 1, ABmp.Canvas.Fill);
      ABmp.Canvas.DrawRect(r, ARadius, ARadius, [TCorner.TopLeft, TCorner.BottomLeft], 1, ABmp.Canvas.Stroke);
    end
    else
    if AStyle = ksButtonSegmentRight then
    begin
      ABmp.Canvas.FillRect(r, ARadius, ARadius, [TCorner.TopRight, TCorner.BottomRight], 1, ABmp.Canvas.Fill);
      ABmp.Canvas.DrawRect(r, ARadius, ARadius, [TCorner.TopRight, TCorner.BottomRight], 1, ABmp.Canvas.Stroke);
    end
    else
    begin
      ABmp.Canvas.FillRect(r, 0, 0, AllCorners, 1, ABmp.Canvas.Fill);
      ABmp.Canvas.DrawRect(r, 0, 0, AllCorners, 1, ABmp.Canvas.Stroke);
    end;

    ABmp.Canvas.Fill.Color := AFontColor;
    ABmp.Canvas.FillText(r, AText, False, 1, [], TTextAlign.Center);

    ABmp.Canvas.EndScene;

    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ARect, 1, True);
  finally
    FreeAndNil(ABmp);
  end;
end;

procedure DrawCheckMarkAccessory(ACanvas: TCanvas; ARect: TRectF; AColor: TAlphaColor);
var
  ABmp: TBitmap;
  r: TRectF;
begin
  ABmp := TBitmap.Create(Round(ARect.Width * GetScreenScale), Round(ARect.Height * GetScreenScale));
  try
    ABmp.Clear(claNull);
    ABmp.BitmapScale := GetScreenScale;

    ABmp.Canvas.BeginScene;
    ABmp.Canvas.StrokeThickness := 2*GetScreenScale;
    ABmp.Canvas.Stroke.Color := AColor;
    r := RectF(0, 0, ABmp.Height, ABmp.Height);

    ABmp.Canvas.DrawLine(PointF(4, ABmp.Height/2), PointF(ABmp.Width/3, ABmp.Height-4), 1);
    ABmp.Canvas.DrawLine(PointF(ABmp.Width/3, ABmp.Height-4), PointF(ABmp.Width-4, 0),  1);

    ABmp.Canvas.EndScene;

    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ARect, 1, False);
  finally
    FreeAndNil(ABmp);
  end;
end;

procedure DrawMoreAccessory(ACanvas: TCanvas; ARect: TRectF; AColor: TAlphaColor);
var
  ABmp: TBitmap;
  APath: TPathData;
  APadding: single;
begin
  ABmp := TBitmap.Create(Round(ARect.Width * GetScreenScale), Round(ARect.Height * GetScreenScale));
  try
    ABmp.Clear(claNull);
    ABmp.BitmapScale := GetScreenScale;

    ABmp.Canvas.BeginScene;
    ABmp.Canvas.StrokeThickness := GetScreenScale*2;
    ABmp.Canvas.Stroke.Color := AColor;
    ABmp.Canvas.StrokeJoin := TStrokeJoin.Miter;

    APadding := GetScreenScale;

    APath := TPathData.Create;
    try
      APath.MoveTo(PointF(ABmp.Width / 2, ABmp.Height-APadding));
      APath.LineTo(PointF(ABmp.Width-APadding, (ABmp.Height/2)));
      APath.LineTo(PointF(ABmp.Width / 2, APadding));
      APath.MoveTo(PointF(ABmp.Width-APadding, (ABmp.Height/2)));
      APath.ClosePath;

      ABmp.Canvas.DrawPath(APath, 1);
    finally
      FreeAndNil(APath);
    end;

    ABmp.Canvas.EndScene;



    ACanvas.DrawBitmap(ABmp, RectF(0, 0, ABmp.Width, ABmp.Height), ARect, 1, False);
  finally
    FreeAndNil(ABmp);
  end;
end;

initialization

  _ScreenScale := 0;
  ATextLayout := TTextLayoutManager.DefaultTextLayout.Create;

finalization

  FreeAndNil(ATextLayout);

end.

