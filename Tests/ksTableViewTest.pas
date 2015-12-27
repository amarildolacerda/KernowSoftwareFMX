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
    [Test] procedure TestAccessories;
    [Test] procedure TestGetItemFromPos;
    [Test] procedure BringSelectedIntoView;
    [Test] procedure TestGetCheckedCount;
    [Test] procedure TestTotalItemHeight;
    [Test] procedure TestTopItem;
    [Test] procedure TestTopItemFiltered;
  end;

implementation

uses SysUtils;

//----------------------------------------------------------------------------------------------------

procedure TksTableViewTest.Setup;
begin
  FTableView := TksTableView.Create(nil);
end;

procedure TksTableViewTest.TearDown;
begin
  FTableView.Free;
end;

//----------------------------------------------------------------------------------------------------

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

procedure TksTableViewTest.TestGetCheckedCount;
var
  ICount: integer;
begin
  FTableView.CheckMarkOptions.CheckMarks := TksTableViewCheckMarks.cmMultiSelect;
  Add100Items;
  for ICount := 1 to 100 do
  begin
    if ICount mod 2 = 0 then
      FTableView.Items[ICount-1].Checked := True;
  end;
  Assert.AreEqual(50, FTableView.Items.GetCheckedCount);
end;


procedure TksTableViewTest.TestGetItemFromPos;
var
  AItem: TksTableViewItem;
begin
  Add100Items;
  AItem := FTableView.GetItemFromPos(10, 600);
  Assert.AreEqual(13, AItem.Index);
end;

procedure TksTableViewTest.BringSelectedIntoView;
begin
  Add100Items;
  FTableView.SelectionOptions.KeepSelectedInView := False;
  FTableView.SelectionOptions.KeepSelection := True;
  FTableView.ItemIndex := 15;
  FTableView.BringSelectedIntoView;
  Assert.AreEqual(404, Integer(Round(FTableView.ScrollViewPos)));
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




procedure TksTableViewTest.TestAccessories;
var
  ICount: TksAccessoryType;
begin
  for ICount := Low(TksAccessoryType) to High(TksAccessoryType) do
    FTableView.Items.AddItem('Item', ICount);
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

procedure TksTableViewTest.TestTopItem;
begin
  Add100Items;
  Assert.AreEqual(0, FTableView.TopItem.Index);
end;

procedure TksTableViewTest.TestTopItemFiltered;
begin
  Add100Items;
  FTableView.SearchText := 'Item 4';
  Assert.AreEqual(3, FTableView.TopItem.AbsoluteIndex);
end;

procedure TksTableViewTest.TestTotalItemHeight;
begin
  Add100Items;
  Assert.AreEqual(4400, Integer(Round(FTableView.TotalItemHeight)));
end;

initialization
  TDUnitX.RegisterTestFixture(TksTableViewTest);
end.
