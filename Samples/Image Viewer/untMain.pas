unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  ksImageViewer, FMX.Controls.Presentation, FMX.StdCtrls, ksTypes, FMX.Objects,
  FMX.ListBox, FMX.Gestures;

type
  TfrmMain = class(TForm)
    ksImageViewer1: TksImageViewer;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

end.
