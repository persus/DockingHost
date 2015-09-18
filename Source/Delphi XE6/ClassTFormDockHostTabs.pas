{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
  Note: Host for dockable forms. It is of the shape where it is floating and
        other dockable forms are docked in a PageControl. This form is dockable.
===============================================================================}

unit ClassTFormDockHostTabs;

interface

{$REGION 'uses'}
uses
  System.Classes,
  System.Types,

  WinAPI.Windows,
  WinAPI.Messages,

  VCL.ActnList,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.Forms,
  VCL.Graphics,

  ClassTFormDockableBase,
  ClassTFormDockable;
{$ENDREGION}

type

  TFormDockHostTabs = class(TFormDockableBase)
  protected
    function GetStandardCloseAction: TCloseAction; override;
    function GetIconIndex(const DockClient: TWinControl): Integer; virtual;
    procedure SetImageIndexes;

  public
    function GetStandardDockHost: TWinControl; override;
    procedure UpdateCaption(const Exclude: TControl); override;

  published
    PageControl: TPageControl;

    procedure ControlDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer); override;
    procedure ControlDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
                State: TDragState; var Accept: Boolean);
    procedure ControlGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
                MousePos: TPoint; var CanDock: Boolean);
    procedure PageControlDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect;
                Active: Boolean);
  end;

implementation

{$R *.dfm}

{$REGION 'uses'}
uses
  ClassTFormDockHost,
  ClassTFormDockHostJoin;
{$ENDREGION}

{$REGION 'TFormDockHostTabs'}

function TFormDockHostTabs.GetIconIndex(const DockClient: TWinControl): Integer;
begin
  Result := -1;
//  if Assigned(DockClient) then Result := <You may set an IconIndex here>
end;

function TFormDockHostTabs.GetStandardCloseAction: TCloseAction;
begin
  Result := caFree;
end;

function TFormDockHostTabs.GetStandardDockHost: TWinControl;
begin
  Result := PageControl;
end;

procedure TFormDockHostTabs.ControlDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  inherited;
  Self.SetImageIndexes;
end;

procedure TFormDockHostTabs.ControlDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
begin
  Accept := Self.DockingIsAllowed(Source.Control) and (Source.Control.HostDockSite <> PageControl);
end;

procedure TFormDockHostTabs.ControlGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := Self.DockingIsAllowed(DockClient) and (DockClient.HostDockSite <> PageControl);
end;

procedure TFormDockHostTabs.PageControlDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect;
            Active: Boolean);
var
  CaptionX: Integer;
  CaptionY: Integer;
  TabCaption: string;
begin
  if TabIndex = 1
  then begin
    Control.Canvas.Font.Color := clWhite;
    Control.Canvas.Brush.Color := clMaroon;
  end
  else begin
    Control.Canvas.Font.Color := clWindowText;
    Control.Canvas.Brush.Color := clBtnFace;
  end;

  TabCaption := PageControl.Pages[TabIndex].Caption;
  CaptionX := Rect.Left + ((Rect.Right - Rect.Left - Control.Canvas.TextWidth(TabCaption)) div 2);
  CaptionY := Rect.Top + ((Rect.Bottom - Rect.Top - Control.Canvas.TextHeight('Gg')) div 2);

  Control.Canvas.FillRect(Rect);
  Control.Canvas.TextOut(CaptionX, CaptionY, TabCaption);
end;

procedure TFormDockHostTabs.SetImageIndexes;
var
  iIndex: Integer;
  TabSheet: TTabSheet;
  DockClient: TWinControl;
begin
//  if not Assigned(PageControl.Images) then PageControl.Images := You may assign an ImageList here
  if not Assigned(PageControl.Images) then Exit;

  for iIndex := 0 to PageControl.DockClientCount - 1
  do begin
    DockClient := PageControl.DockClients[iIndex] as TWinControl;
    TabSheet := DockClient.Parent as TTabSheet;
    TabSheet.ImageIndex := Self.GetIconIndex(DockClient);
  end;
end;

procedure TFormDockHostTabs.UpdateCaption(const Exclude: TControl);
begin
  FDockingHelper.UpdateCaption(Exclude);
end;

{$ENDREGION}

end.
