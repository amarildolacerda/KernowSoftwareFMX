{*******************************************************************************
*                                                                              *
*  TksNetHttpClient - Enhanced NetHTTP component with ASync methods            *
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

unit ksNetHttpClient;

interface

{$I ksComponents.inc}

uses
  System.SysUtils, System.Classes, System.Net.HttpClientComponent,
  System.Net.HttpClient, System.Net.UrlClient, FMX.Graphics;

type
  TAsyncGetEvent = procedure(Sender: TObject; AResponse: IHTTPResponse) of object;
  TAsyncGetBitmapEvent = procedure(Sender: TObject; ABitmap: TBitmap) of object;
  TksHttpThreadType = (ksTtNormal, ksTtBitmap);

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]
  TksNetHttpClient = class(TNetHTTPClient)
  private
    FOnAsyncGet: TAsyncGetEvent;
    FOnAsyncGetBitmap: TAsyncGetBitmapEvent;
    procedure DoOnAsyncGet(AResponse: IHTTPResponse);
    procedure DoOnAsyncGetBitmap(ABitmap: TBitmap);
    { Private declarations }
  protected
    { Protected declarations }
  public
    procedure GetAsync(const AURL: string;
                       const AHeaders: TNetHeaders = nil;
                       const AOnReceiveData: TAsyncGetEvent = nil);

    procedure GetAsyncBitmap(const AURL: string;
                             const AHeaders: TNetHeaders = nil;
                             const AOnReceiveBitmap: TAsyncGetBitmapEvent = nil);
    { Public declarations }
  published
    property OnAsyncGet: TAsyncGetEvent read FOnAsyncGet write FOnAsyncGet;
    property OnAsyncGetBitmap: TAsyncGetBitmapEvent read FOnAsyncGetBitmap write FOnAsyncGetBitmap;
    { Published declarations }
  end;

procedure Register;

implementation

type
  TksNetHttpClientThread = class(TThread)
  private
    FOwner: TksNetHttpClient;
    FHttpClient: TksNetHttpClient;
    FHeaders: TNetHeaders;
    FUrl: string;
    FResponse: IHTTPResponse;
    FAsyncGet: TAsyncGetEvent;
    FAsyncGetBitmap: TAsyncGetBitmapEvent;
    FType: TksHttpThreadType;
    FData: TObject;
    procedure DataReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TksNetHttpClient);
    destructor Destroy; override;
    procedure Get(const AURL: string; const AHeaders: TNetHeaders; const AOnGet: TAsyncGetEvent);
    procedure GetBitmap(const AURL: string; const AHeaders: TNetHeaders; const AOnGet: TAsyncGetBitmapEvent);
    property OnAsyncGet: TAsyncGetEvent read FAsyncGet write FAsyncGet;
    property Data: TObject read FData write FData;
  end;

procedure Register;
begin
  RegisterComponents('Kernow Software Misc', [TksNetHttpClient]);
end;

{ TksNetHttpClient }

procedure TksNetHttpClient.DoOnAsyncGet(AResponse: IHTTPResponse);
begin
  if Assigned(FOnAsyncGet) then
    FOnAsyncGet(Self, AResponse);
end;

procedure TksNetHttpClient.DoOnAsyncGetBitmap(ABitmap: TBitmap);
begin
  if Assigned(FOnAsyncGetBitmap) then
    FOnAsyncGetBitmap(Self, ABitmap);
end;

procedure TksNetHttpClient.GetAsync(const AURL: string;
                                    const AHeaders: TNetHeaders = nil;
                                    const AOnReceiveData: TAsyncGetEvent = nil);
var
  AThread: TksNetHttpClientThread;
begin
  AThread := TksNetHttpClientThread.Create(Self);
  AThread.Get(AUrl, AHeaders, AOnReceiveData);
end;



procedure TksNetHttpClient.GetAsyncBitmap(const AURL: string;
  const AHeaders: TNetHeaders = nil; const AOnReceiveBitmap: TAsyncGetBitmapEvent = nil);
var
  AThread: TksNetHttpClientThread;
begin
  AThread := TksNetHttpClientThread.Create(Self);
  AThread.GetBitmap(AUrl, AHeaders, AOnReceiveBitmap);
end;

{ TksNetHttpClientThread }

constructor TksNetHttpClientThread.Create(AOwner: TksNetHttpClient);
begin
  inherited Create(True);
  FHttpClient := TksNetHttpClient.Create(nil);
  FType := ksTtNormal;
  FOwner := AOwner;
  FreeOnTerminate := True;
end;

procedure TksNetHttpClientThread.DataReceived;
var
  ABmp: TBitmap;
begin
  ABmp := nil;
  if FType = ksTtNormal then
  begin
    if Assigned(FAsyncGet) then
    begin
      FAsyncGet(Self, FResponse);
    end
    else
    begin
      if FOwner <> nil then
        FOwner.DoOnAsyncGet(FResponse);
    end;
  end;
  if FType = ksTtBitmap then
  begin
    ABmp := TBitmap.CreateFromStream(FResponse.ContentStream);
    try
      if Assigned(FAsyncGetBitmap) then
      begin
        FAsyncGetBitmap(Self, ABmp);
      end
      else
      begin
        if FOwner <> nil then
          FOwner.DoOnAsyncGetBitmap(ABmp);
      end;

    finally
      FreeAndNil(ABmp);
    end;
  end;

end;

destructor TksNetHttpClientThread.Destroy;
begin
  {$IFDEF NEXTGEN}
  FHttpClient.DisposeOf;
  {$ELSE}
  FHttpClient.Free;
  {$ENDIF}
  inherited;
end;

procedure TksNetHttpClientThread.Execute;
begin
  FResponse := FHttpClient.Get(FUrl, nil, FHeaders);
  Synchronize(DataReceived);
end;

procedure TksNetHttpClientThread.Get(const AURL: string;
                                     const AHeaders: TNetHeaders;
                                     const AOnGet: TAsyncGetEvent);
begin
  FUrl := AUrl;
  FType := ksTtNormal;
  FData := nil;
  FHeaders := AHeaders;
  FAsyncGet := AOnGet;
  Start;
end;

procedure TksNetHttpClientThread.GetBitmap(const AURL: string; const AHeaders: TNetHeaders; const AOnGet: TAsyncGetBitmapEvent);
begin
  FUrl := AUrl;
  FType := ksTtBitmap;
  FHeaders := AHeaders;
  FAsyncGetBitmap := AOnGet;
  Start;
end;

end.
