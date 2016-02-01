unit ksComponentEditors;

interface

uses Classes, DesignEditors, DesignIntf;

const
  C_VERB_ABOUT = 'About';
  C_ADD_TAB = '&Add Tab';

type


 TksBaseComponentEditor = class(TComponentEditor)
  private
    FVerbs: TStringList;
  protected
    procedure BuildVerbs(AVerbs: TStrings); virtual;
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    destructor Destroy; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TksTabControlComponentEditor = class(TksBaseComponentEditor)
  private
  protected
    procedure BuildVerbs(AVerbs: TStrings); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
  end;




  procedure Register;

implementation

uses SysUtils, FMX.Types, FMX.Dialogs, ksTabControl, ksTypes;

procedure Register;
begin
  RegisterComponentEditor(TksComponent, TksBaseComponentEditor);
  RegisterComponentEditor(TksControl, TksBaseComponentEditor);

  // tab control...
  RegisterComponentEditor(TksTabControl, TksTabControlComponentEditor);
  RegisterComponentEditor(TksTabItem, TksTabControlComponentEditor);
end;

{ TksTabControlComponentEditor }

procedure TksTabControlComponentEditor.BuildVerbs(AVerbs: TStrings);
begin
  inherited;
  AVerbs.Add(C_ADD_TAB);
end;

procedure TksTabControlComponentEditor.ExecuteVerb(Index: Integer);
var
  ItemParent: TFmxObject;
  ATabControl: TksTabControl;
begin
  inherited;
  if FVerbs[Index] = C_ADD_TAB then
  begin
    ATabControl := nil;
    if Component is TksTabControl then
      ATabControl := (Component as TksTabControl);
    if Component is TksTabItem then
      ATabControl := TksTabControl(TksTabItem(Component).Parent);
    if ATabControl <> nil then
    begin
      ItemParent := TFmxObject((ATabControl as IItemsContainer).GetObject);
      Designer.CreateChild(TksTabItem, ItemParent);
    end;
  end;
end;

{ TksBaseComponentEditor }

procedure TksBaseComponentEditor.BuildVerbs(AVerbs: TStrings);
begin
  //AVerbs.Add(C_VERB_ABOUT);
end;

constructor TksBaseComponentEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited;
  FVerbs := TStringList.Create;
  BuildVerbs(FVerbs);
  if FVerbs.Count > 0 then
    FVerbs.Add('-');
  FVerbs.Add(C_VERB_ABOUT);
  FVerbs.Add('-');
end;

destructor TksBaseComponentEditor.Destroy;
begin
  FVerbs.Free;
  inherited;
end;

procedure TksBaseComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if FVerbs[Index] = C_VERB_ABOUT then
    ShowMessage('ksComponents - Components for FireMonkey'+#13+#13+
                'Twitter: @kscomponents'+#13+#13+
                'Copyright © 2016 Graham Murt');
end;

function TksBaseComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := FVerbs[Index];
end;

function TksBaseComponentEditor.GetVerbCount: Integer;
begin
  Result := FVerbs.Count;
end;

end.
