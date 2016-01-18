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

uses
  System.SysUtils, System.Classes, System.Net.HttpClientComponent,
  System.Net.HttpClient, System.Net.UrlClient;

type
  TAsyncGetEvent = procedure(Sender: TObject; AResponse: IHTTPResponse) of object;

  TksNetHttpClient = class(TNetHTTPClient)
  private
    FOnAsyncGet: TAsyncGetEvent;
    procedure DoOnAsyncGet(AResponse: IHTTPResponse);
    { Private declarations }
  protected
    { Protected declarations }
  public
    //constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
    procedure GetAsync(const AURL: string;
                       const AHeaders: TNetHeaders = nil;
                       const AOnReceiveData: TAsyncGetEvent = nil);

    { Public declarations }
  published
    property OnAsyncGet: TAsyncGetEvent read FOnAsyncGet write FOnAsyncGet;
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
    procedure DataReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TksNetHttpClient);
    destructor Destroy; override;
    procedure Get(const AURL: string; const AHeaders: TNetHeaders; const AOnGet: TAsyncGetEvent);
    property OnAsyncGet: TAsyncGetEvent read FAsyncGet write FAsyncGet;
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

procedure TksNetHttpClient.GetAsync(const AURL: string;
                                    const AHeaders: TNetHeaders = nil;
                                    const AOnReceiveData: TAsyncGetEvent = nil);
var
  AThread: TksNetHttpClientThread;
begin
  AThread := TksNetHttpClientThread.Create(Self);
  AThread.Get(AUrl, AHeaders, AOnReceiveData);
end;



{ TksNetHttpClientThread }

constructor TksNetHttpClientThread.Create(AOwner: TksNetHttpClient);
begin
  inherited Create(True);
  FHttpClient := TksNetHttpClient.Create(nil);
  FOwner := AOwner;
  FreeOnTerminate := True;
end;

procedure TksNetHttpClientThread.DataReceived;
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
  Sleep(3000);
  Synchronize(DataReceived);
end;

procedure TksNetHttpClientThread.Get(const AURL: string;
                                     const AHeaders: TNetHeaders;
                                     const AOnGet: TAsyncGetEvent);
begin
  FUrl := AUrl;
  FHeaders := AHeaders;
  FAsyncGet := AOnGet;
  Start;
end;

end.
