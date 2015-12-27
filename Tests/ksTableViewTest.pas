unit ksTableViewTest;

interface
uses
  DUnitX.TestFramework,
  ksTableView;

type

  [TestFixture]
  TksTableViewTest = class(TObject)
  private
    FTableView: TksTableView;
    procedure Add100Items;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // tests...
    [Test] procedure Test1000Items;
    [Test] procedure TestFilteredItems;
    [Test] procedure TestClearItems;
    [Test] procedure TestScrollToLastItem;
  end;

implementation

uses SysUtils;

procedure TksTableViewTest.Add100Items;
var
  ICount: integer;
begin
  // arrange
  // act
  FTableView.BeginUpdate;
  try
    for ICount := 1 to 100 do
      FTableView.Items.AddItem('Item '+IntToStr(ICount), 'a sub title', 'the detail', atNone);
  finally
    FTableView.EndUpdate;
  end;
end;

procedure TksTableViewTest.Setup;
begin
  FTableView := TksTableView.Create(nil);
end;

procedure TksTableViewTest.TearDown;
begin
  FTableView.Free;
end;

procedure TksTableViewTest.Test1000Items;
var
  ICount: integer;
begin
  // arrange
  // act
  FTableView.BeginUpdate;
  try
    for ICount := 1 to 1000 do
      FTableView.Items.AddItem('Item '+IntToStr(ICount), 'a sub title', 'the detail', atNone);
  finally
    FTableView.EndUpdate;
  end;
  // assert
  Assert.AreEqual(1000, FTableView.Items.Count);
end;


procedure TksTableViewTest.TestClearItems;
begin
  // arrange
  Add100Items;
  // act
  FTableView.ClearItems;
  // assert
  Assert.AreEqual(0, FTableView.Items.Count);
end;

procedure TksTableViewTest.TestFilteredItems;
begin
  // arrange
  FTableView.SearchVisible := True;
  Add100Items;
  // act
  FTableView.SearchText := 'Item 6';
  // assert
  Assert.AreEqual(11, FTableView.FilteredItems.Count);
end;

procedure TksTableViewTest.TestScrollToLastItem;
var
  AResult: Extended;
begin
  // arrange
  Add100Items;
  // act
  FTableView.ScrollToItem(99);
  // assert
  AResult := FTableView.ScrollViewPos;
  Assert.AreEqual(4356.0, AResult);
end;

initialization
  TDUnitX.RegisterTestFixture(TksTableViewTest);
end.
