unit untSourceCode;

interface

{$IFDEF VER290}
  {$DEFINE XE8_OR_NEWER}
{$ENDIF}

{$IFDEF VER300}
  {$DEFINE XE8_OR_NEWER}
  {$DEFINE XE10_OR_NEWER}
{$ENDIF}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Objects;

type
  TfrmSourceCode = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    btnRightMenu: TButton;
    btnLeftMenu: TButton;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Rectangle1: TRectangle;
    procedure btnLeftMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnRightMenuClick(Sender: TObject);
  private
    FCode: string;
    FBitmap: TBitmap;
    { Private declarations }
  protected
    procedure DoShow; override;
  public
    { Public declarations }
  end;

  procedure DisplaySourceCode(ACode: TStrings; AImage: TBitmap);

implementation

uses FMX.Platform, System.Rtti;

{$R *.fmx}

function GetScreenScale: single;
var
  Service: IFMXScreenService;
begin
  Service := IFMXScreenService(TPlatformServices.Current.GetPlatformService
    (IFMXScreenService));
  Result := Service.GetScreenScale;
end;

procedure DisplaySourceCode(ACode: TStrings; AImage: TBitmap);
var
  frmSourceCode: TfrmSourceCode;
begin
  frmSourceCode := TfrmSourceCode.Create(nil);
  frmSourceCode.FCode := ACode.Text;
  frmSourceCode.FBitmap := AImage;
  frmSourceCode.Show;
end;

procedure TfrmSourceCode.btnLeftMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSourceCode.btnRightMenuClick(Sender: TObject);
var
  ClipBoard: IFMXClipboardService;
  Value: TValue;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(Clipboard)) then
    begin
      Value := TValue.From<string>(FCode);
      Clipboard.SetClipboard(Value);
      ShowMessage('Code copied to clipboard');
    end;
end;

procedure TfrmSourceCode.DoShow;
begin
  inherited;
  Image1.Position.X := 0;
  Image1.Position.Y := 0;
  Image1.Width := FBitmap.Width;
  Image1.Height := FBitmap.Height;
  Image1.Bitmap := FBitmap;
end;

procedure TfrmSourceCode.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

end.
