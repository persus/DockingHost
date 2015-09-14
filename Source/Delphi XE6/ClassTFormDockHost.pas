{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
  Note: This is the MainForm and the standard-host for doackable forms.
===============================================================================}

unit ClassTFormDockHost;

interface

{$REGION 'uses'}
uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Actions,

  WinAPI.Windows,

  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.ActnList,
  VCL.Forms,
  VCL.Tabs,
  VCL.DockTabSet,

  ClassTFormDockableBase,
  ClassTDockingHelper;
{$ENDREGION}

type

  TFormDockHost = class(TForm)
  {$REGION 'private'}
  private
    FLeftDockPanelWidth: Integer;
    FRightDockPanelWidth: Integer;
    FBottomDockPanelHeight: Integer;

    function GetFrameHost: TWinControl;
    function GetActionFromSender(const Sender: TObject): TAction;
    function GetPanelFromSender(const Sender: TObject): TPanel;
    function GetPanelFromSplitter(const Splitter: TSplitter): TPanel;
    function GetSplitterFromPanel(const Panel: TPanel): TSplitter;
    function GetSplitterFromSender(const Sender: TObject): TSplitter;
    function GetTabFromSender(const Sender: TObject): TDockTabSet;

    procedure HandleDockingSource(const Form: TFormDockableBase; const DockType: TAlign);
    procedure HandleUndockingSource(const Form: TFormDockableBase);

    procedure ShowDockableFormDockedInJoinDockHost(const DockableForm: TFormDockableBase);
    procedure ShowDockableFormDockedInTabsDockHost(const DockableForm: TFormDockableBase);
  {$ENDREGION}

  {$REGION 'protected'}
  protected
    FDockingHelper: TDockingHelper;

    function GetLeftDockPanelWidth: Integer; virtual;
    procedure SetLeftDockPanelWidth(const Value: Integer); virtual;
    function GetRightDockPanelWidth: Integer; virtual;
    procedure SetRightDockPanelWidth(const Value: Integer); virtual;
    function GetBottomDockPanelHeight: Integer; virtual;
    procedure SetBottomDockPanelHeight(const Value: Integer); virtual;
  {$ENDREGION}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CheckForProperDockCombinations(const HostDockSite: TWinControl; const Source: TControl): Boolean;
    procedure DoDockOnPanel(const Panel: TPanel);
    procedure DoUndockFromPanel(const Sender: TObject);
    function UndockingIsAllowed(const Client: TControl; const NewTarget: TWinControl): Boolean;

    procedure HideDockPanel(const Panel: TPanel; const CloseAction: TCloseAction = caHide);
    procedure ShowDockPanel(const Panel: TPanel; const Client: TControl);
    procedure HideDockTabSet(const DockTabSet: TDockTabSet);
    procedure ShowDockTabSet(const DockTabSet: TDockTabSet);
    procedure HideDockableForm(const DockableForm: TFormDockableBase);
    procedure ShowDockableForm(const DockableForm: TFormDockableBase);

    procedure TriggerDockableForm(Sender: TObject);

  published
    pnl_DockLeft: TPanel;
    spl_DockLeft: TSplitter;
    dts_DockTabSetLeft: TDockTabSet;
    pnl_DockRight: TPanel;
    spl_DockRight: TSplitter;
    dts_DockTabSheetRight: TDockTabSet;
    pnl_Main: TPanel;
    dts_DockTabSetBottom: TDockTabSet;
    pnl_DockBottom: TPanel;
    spl_DockBottom: TSplitter;

    procedure DockPanelDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure DockPanelDockOverBottom(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
                State: TDragState; var Accept: Boolean);
    procedure DockPanelDockOverLeft(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
                State: TDragState; var Accept: Boolean);
    procedure DockPanelDockOverRight(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
                State: TDragState; var Accept: Boolean);
    procedure DockPanelGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
                MousePos: TPoint; var CanDock: Boolean);
    procedure DockPanelUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean); virtual;

    procedure DockSplitterMoved(Sender: TObject);

    procedure DockTabSetDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure DockTabSetGetSiteInfoBottom(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
                MousePos: TPoint; var CanDock: Boolean);
    procedure DockTabSetGetSiteInfoLeft(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
                MousePos: TPoint; var CanDock: Boolean);
    procedure DockTabSetGetSiteInfoRight(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
                MousePos: TPoint; var CanDock: Boolean);
    procedure DockTabSetTabAdded(Sender: TObject);
    procedure DockTabSetTabRemoved(Sender: TObject);
    procedure DockTabSetUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);

    property FrameHost: TWinControl read GetFrameHost;
    property LeftDockPanelWidth: Integer read GetLeftDockPanelWidth write SetLeftDockPanelWidth;
    property RightDockPanelWidth: Integer read GetRightDockPanelWidth write SetRightDockPanelWidth;
    property BottomDockPanelHeight: Integer read GetBottomDockPanelHeight write SetBottomDockPanelHeight;
  end;

implementation

{$R *.DFM}

{$REGION 'uses'}
uses
  DockExceptions,
  ClassTFormDockHostTabs,
  ClassTFormDockHostJoin;
{$ENDREGION}

{$REGION 'TFormDockHost'}

constructor TFormDockHost.Create(AOwner: TComponent);
begin
  inherited;
  FDockingHelper := TDockingHelper.Create(nil);

  FLeftDockPanelWidth := Self.Width div ZoneWidth;
  FRightDockPanelWidth := Self.Width div ZoneWidth;
  FBottomDockPanelHeight := Self.Width div ZoneWidth;

  spl_DockBottom.Tag := NativeInt(pnl_DockBottom);
  pnl_DockBottom.Tag := NativeInt(spl_DockBottom);
  pnl_DockBottom.Height := 0;
  dts_DockTabSetBottom.Height := 0;
  HideDockTabSet(dts_DockTabSetBottom);

  spl_DockLeft.Tag := NativeInt(pnl_DockLeft);
  pnl_DockLeft.Tag := NativeInt(spl_DockLeft);
  pnl_DockLeft.Width := 0;
  dts_DockTabSetLeft.Width := 0;
  HideDockTabSet(dts_DockTabSetBottom);

  spl_DockRight.Tag := NativeInt(pnl_DockRight);
  pnl_DockRight.Tag := NativeInt(spl_DockRight);
  pnl_DockRight.Width := 0;
  dts_DockTabSheetRight.Width := 0;
  HideDockTabSet(dts_DockTabSetBottom);
end;

destructor TFormDockHost.Destroy;
begin
  System.SysUtils.FreeAndNil(FDockingHelper);
  inherited;
end;

function TFormDockHost.CheckForProperDockCombinations(const HostDockSite: TWinControl; const Source: TControl): Boolean;
begin
  Assert(Assigned(HostDockSite));
  Assert(Assigned(Source));

  if (Source is TFormDockHostTabs) and (HostDockSite.DockClientCount > 0)
  then Result := False
  else Result := not Assigned(FDockingHelper.GetDockHostTabsIfDockedOnControl(HostDockSite));
end;

function TFormDockHost.GetFrameHost: TWinControl;
begin
  Result := pnl_Main;
end;

function TFormDockHost.GetLeftDockPanelWidth: Integer;
begin
  Result := FLeftDockPanelWidth;
end;

procedure TFormDockHost.DockPanelDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  // OnDockDrop gets called after the client has actually docked, so we
  // check for DockClientCount >= 1 before making the dock panel visible.

  Self.DoDockOnPanel(Self.GetPanelFromSender(Sender));
  Self.HandleDockingSource(Source.Control as TFormDockableBase, Source.DropAlign);
  FDockingHelper.MarkClientAsDocked(Source);
end;

procedure TFormDockHost.DockPanelDockOverBottom(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
             State: TDragState; var Accept: Boolean);
var
  Rect: TRect;
  Panel: TPanel;
begin
  Panel := Self.GetPanelFromSender(Sender);
  Accept := FDockingHelper.DockingIsAllowed(Source.Control)
        and FDockingHelper.AnchorsAreSubSet((Source.Control as TFormDockableBase).AllowedAnchors, [akBottom])
        and Self.CheckForProperDockCombinations(Panel, Source.Control);

  if Accept then
  begin
    Rect.TopLeft := Panel.ClientToScreen(Point(0, -Self.ClientHeight div 4));
    Rect.BottomRight := Panel.ClientToScreen(Point(Panel.Width, Panel.Height));
    Source.DockRect := Rect;
  end;
end;

procedure TFormDockHost.DockPanelDockOverLeft(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
var
  Rect: TRect;
  Panel: TPanel;
begin
  Panel := Self.GetPanelFromSender(Sender);
  Accept := FDockingHelper.DockingIsAllowed(Source.Control)
        and FDockingHelper.AnchorsAreSubSet((Source.Control as TFormDockableBase).AllowedAnchors, [akLeft])
        and Self.CheckForProperDockCombinations(Panel, Source.Control);

  if Accept then
  begin
    Rect.TopLeft := Panel.ClientToScreen(Point(0, 0));
    Rect.BottomRight := Panel.ClientToScreen(Point(Self.ClientWidth div 4, Panel.Height));
    Source.DockRect := Rect;
  end;
end;

procedure TFormDockHost.DockPanelDockOverRight(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
var
  Rect: TRect;
  Panel: TPanel;
begin
  Panel := Self.GetPanelFromSender(Sender);
  Accept := FDockingHelper.DockingIsAllowed(Source.Control)
        and FDockingHelper.AnchorsAreSubSet((Source.Control as TFormDockableBase).AllowedAnchors, [akRight])
        and Self.CheckForProperDockCombinations(Panel, Source.Control);

  if Accept then
  begin
    Rect.TopLeft := Panel.ClientToScreen(Point(-Self.ClientWidth div 4, 0));
    Rect.BottomRight := Panel.ClientToScreen(Point(0, Panel.Height));
    Source.DockRect := Rect;
  end;
end;

procedure TFormDockHost.DockPanelGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := FDockingHelper.DockingIsAllowed(DockClient)
         and Self.CheckForProperDockCombinations(Sender as TWinControl, DockClient);
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TFormDockHost.DockPanelUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  // OnUnDock gets called before the client is undocked, in order to optionally
  // disallow the undock. DockClientCount is never 0 when called from this event.

  Allow := FDockingHelper.TransdockingIsAllowed(Client) or Self.UndockingIsAllowed(Client, NewTarget);
  if Allow
  then begin
    Self.DoUndockFromPanel(Sender);
    Self.HandleUndockingSource(Client as TFormDockableBase);
  end
  else begin
    FDockingHelper.DoFloatDueToNotAllowedDockingTarget(Client as TFormDockableBase);
  end;
end;

procedure TFormDockHost.DockTabSetDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  if Source.Control.Visible then (Sender as TDockTabSet).ShowDockClient(Source.Control);
  Self.HandleDockingSource(Source.Control as TFormDockableBase, Source.DropAlign);
  FDockingHelper.MarkClientAsDocked(Source);
end;

procedure TFormDockHost.DockTabSetGetSiteInfoBottom(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
var
  SendingTab: TDockTabSet;
begin
  SendingTab := Self.GetTabFromSender(Sender);
  InfluenceRect.Right := SendingTab.DestinationDockSite.ClientWidth;

  CanDock := FDockingHelper.DockingIsAllowed(DockClient)
         and FDockingHelper.AnchorsAreSubSet((DockClient as TFormDockableBase).AllowedAnchors, [akBottom])
         and (SendingTab.Tabs.Count > 0);
end;

procedure TFormDockHost.DockTabSetGetSiteInfoLeft(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
var
  SendingTab: TDockTabSet;
begin
  SendingTab := Self.GetTabFromSender(Sender);
  InfluenceRect.Right := SendingTab.DestinationDockSite.ClientWidth;

  CanDock := FDockingHelper.DockingIsAllowed(DockClient)
         and FDockingHelper.AnchorsAreSubSet((DockClient as TFormDockableBase).AllowedAnchors, [akLeft])
         and (SendingTab.Tabs.Count > 0);
end;

procedure TFormDockHost.DockTabSetGetSiteInfoRight(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
var
  SendingTab: TDockTabSet;
begin
  SendingTab := Self.GetTabFromSender(Sender);
  InfluenceRect.Right := SendingTab.DestinationDockSite.ClientWidth;

  CanDock := FDockingHelper.DockingIsAllowed(DockClient)
         and FDockingHelper.AnchorsAreSubSet((DockClient as TFormDockableBase).AllowedAnchors, [akRight])
         and (SendingTab.Tabs.Count > 0);
end;

procedure TFormDockHost.DockTabSetTabAdded(Sender: TObject);
const
  DockTabSize = 23;
var
  SendingTab : TDockTabSet;
begin

  SendingTab := Self.GetTabFromSender(Sender);
  if SendingTab.Tabs.Count = 1
  then begin

    ShowDockTabSet(SendingTab);

    if SendingTab.Align in [alBottom, alTop]
    then begin
      SendingTab.Height := DockTabSize;
      if SendingTab.Align = alTop
      then SendingTab.Top := 0
      else SendingTab.Top := ClientHeight;
    end
    else begin
      SendingTab.Width := DockTabSize;
      if SendingTab.Align = alLeft
      then SendingTab.Left := 0
      else SendingTab.Left := ClientWidth;
    end;

  end;

end;

procedure TFormDockHost.DockTabSetTabRemoved(Sender: TObject);
var
  SendingTab : TDockTabSet;
begin
  SendingTab := Self.GetTabFromSender(Sender);
  if SendingTab.Tabs.Count = 0 then HideDockTabSet(SendingTab);
end;

procedure TFormDockHost.DockTabSetUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  Allow := FDockingHelper.TransdockingIsAllowed(Client) or Self.UndockingIsAllowed(Client, NewTarget);
  if Allow
  then Self.HandleUndockingSource(Client as TFormDockableBase)
  else FDockingHelper.DoFloatDueToNotAllowedDockingTarget(Client as TFormDockableBase);
end;

procedure TFormDockHost.DoDockOnPanel(const Panel: TPanel);
begin
  if Assigned(Panel) and ((Panel = pnl_DockLeft) or (Panel = pnl_DockRight) or (Panel = pnl_DockBottom))
  then begin
    if Panel.DockClientCount >= 1 then Self.ShowDockPanel(Panel, nil);
    if Panel.Align = alBottom then dts_DockTabSetBottom.Top := dts_DockTabSetBottom.Parent.Height;
    Panel.DockManager.ResetBounds(True);
  end
  else begin
    raise EUnknownDockingPanelOnMainForm.Create('TFormDockHost.DoDockOnPanel: ' + EUnknownDockingPanelOnMainForm.ClassName);
  end;
end;

procedure TFormDockHost.DoUndockFromPanel(const Sender: TObject);
var
  Panel: TPanel;
begin
  Panel := Self.GetPanelFromSender(Sender);
  if Panel.DockClientCount = 1 then Self.HideDockPanel(Panel);
end;

function TFormDockHost.GetActionFromSender(const Sender: TObject): TAction;
begin
  Assert(Assigned(Sender));
  Assert(Sender is TAction);
  Result := Sender as TAction;
end;

function TFormDockHost.GetBottomDockPanelHeight: Integer;
begin
  Result := FBottomDockPanelHeight;
end;

function TFormDockHost.GetPanelFromSender(const Sender: TObject): TPanel;
begin
  Assert(Assigned(Sender));
  Assert(Sender is TPanel);
  Result := Sender as TPanel;
end;

function TFormDockHost.GetPanelFromSplitter(const Splitter: TSplitter): TPanel;
begin
  Assert(Assigned(Splitter));
  Assert(Splitter.Tag > 0);
  Result := TPanel(Pointer(Splitter.Tag));
end;

function TFormDockHost.GetRightDockPanelWidth: Integer;
begin
  Result := FRightDockPanelWidth;
end;

function TFormDockHost.GetSplitterFromPanel(const Panel: TPanel): TSplitter;
begin
  Assert(Assigned(Panel));
  Assert(Panel.Tag > 0);
  Result := TSplitter(Pointer(Panel.Tag));
end;

function TFormDockHost.GetSplitterFromSender(const Sender: TObject): TSplitter;
begin
  Assert(Assigned(Sender));
  Assert(Sender is TSplitter);
  Result := Sender as TSplitter;
end;

function TFormDockHost.GetTabFromSender(const Sender: TObject): TDockTabSet;
begin
  Assert(Assigned(Sender));
  Assert(Sender is TDockTabSet);
  Result := Sender as TDockTabSet;
end;

procedure TFormDockHost.HandleDockingSource(const Form: TFormDockableBase; const DockType: TAlign);
begin
  Assert(Assigned(Form));
  Form.DockType := DockType;
end;

procedure TFormDockHost.HandleUndockingSource(const Form: TFormDockableBase);
begin
  Assert(Assigned(Form));
  Form.DockType := alNone;
end;

procedure TFormDockHost.HideDockableForm(const DockableForm: TFormDockableBase);
begin
  // If window is tab-docked and only one tab is visible so close the host
  // form otherwise close the window itself

  if (DockableForm.HostDockSite is TPageControl) and ((DockableForm.HostDockSite as TPageControl).VisibleDockClientCount = 1)
  then (DockableForm.HostDockSite.Owner as TForm).Close
  else DockableForm.Close;
end;

procedure TFormDockHost.HideDockPanel(const Panel: TPanel; const CloseAction: TCloseAction = caHide);
var
  Splitter: TSplitter;
begin
  // Don't try to hide a panel which has visible dock clients

  if Panel.VisibleDockClientCount <= 1
  then begin
    Splitter := GetSplitterFromPanel(Panel);
    Splitter.Visible := False;
    if (Panel.Align = alLeft) or (Panel.Align = alRight)
    then Panel.Width := 0
    else Panel.Height := 0;
  end;
end;

procedure TFormDockHost.HideDockTabSet(const DockTabSet: TDockTabSet);
begin
  DockTabSet.Visible := False;
end;

procedure TFormDockHost.SetBottomDockPanelHeight(const Value: Integer);
begin
  FBottomDockPanelHeight := Value;
end;

procedure TFormDockHost.SetLeftDockPanelWidth(const Value: Integer);
begin
  FLeftDockPanelWidth := Value;
end;

procedure TFormDockHost.SetRightDockPanelWidth(const Value: Integer);
begin
  FRightDockPanelWidth := Value;
end;

procedure TFormDockHost.ShowDockableForm(const DockableForm: TFormDockableBase);
begin

  // In case window is tab-docked, it is docked to the PageControl (owned by TFormDockHostTabs)
  if (DockableForm.HostDockSite is TPageControl)
  then Self.ShowDockableFormDockedInTabsDockHost(DockableForm)

  // If window is conjoin-docked, host aor form may not be visible so show both.
  else if (DockableForm.HostDockSite is TFormDockHostJoin) and (not DockableForm.HostDockSite.Visible)
  then Self.ShowDockableFormDockedInJoinDockHost(DockableForm)

  // If form is docked to one of the "hidden" docking panels, resize the panel and re-show the docked form.
  else if (DockableForm.HostDockSite is TPanel)
      and ((DockableForm.HostDockSite.Height = 0) or (DockableForm.HostDockSite.Width = 0))
  then Self.ShowDockPanel(DockableForm.HostDockSite as TPanel, DockableForm)

  // If the window isn't docked at all, simply show it.
  else DockableForm.Show;

end;

procedure TFormDockHost.ShowDockableFormDockedInJoinDockHost(const DockableForm: TFormDockableBase);
var
  FormDockHostJoin: TFormDockHostJoin;
begin
  // If window is conjoin-docked, host or form may not be visible so show both.

  FormDockHostJoin := DockableForm.HostDockSite as TFormDockHostJoin;
  DockableForm.HostDockSite.Show;
  FormDockHostJoin.UpdateCaption(nil);
  DockableForm.Show;
end;

procedure TFormDockHost.ShowDockableFormDockedInTabsDockHost(const DockableForm: TFormDockableBase);
var
  FormDockHostTabs: TFormDockHostTabs;
begin
  // If window is tab-docked, it is docked to the PageControl (owned by TFormDockHostTabs)
  // so show the host form. The docked form may not be visible so show it, too.

  FormDockHostTabs := DockableForm.HostDockSite.Owner as TFormDockHostTabs;
  if (FormDockHostTabs.HostDockSite is TPanel) and ((FormDockHostTabs.Height = 0) or (FormDockHostTabs.Width = 0))
  then Self.ShowDockPanel(FormDockHostTabs.HostDockSite as TPanel, FormDockHostTabs);

  FormDockHostTabs.Show;
  DockableForm.Show;
  FormDockHostTabs.UpdateCaption(nil);
end;

procedure TFormDockHost.ShowDockPanel(const Panel: TPanel; const Client: TControl);
var
  Splitter: TSplitter;
begin
  // Since docking to a non-visible docksite isn't allowed, instead of setting
  // Visible for the panels we set the width to zero. The default InfluenceRect
  // for a control extends a few pixels beyond it's boundaries, so it is possible
  // to dock to zero width controls.

  Splitter := Self.GetSplitterFromPanel(Panel);
  Splitter.Visible := True;

  if Panel.Align = alLeft
  then begin
    Panel.Width := Self.LeftDockPanelWidth;
    Splitter.Left := Panel.Width + Splitter.Width;
  end
  else if Panel.Align = alRight
  then begin
    Panel.Width := Self.RightDockPanelWidth;
    Splitter.Left := ClientWidth - (Panel.Width + Splitter.Width);
  end
  else begin
    Panel.Height := Self.BottomDockPanelHeight;
    Splitter.Top := ClientHeight - Panel.Height - Splitter.Width;
  end;

  if Assigned(Client) then Client.Show;
end;

procedure TFormDockHost.ShowDockTabSet(const DockTabSet: TDockTabSet);
begin
  DockTabSet.Visible := True;
end;

procedure TFormDockHost.DockSplitterMoved(Sender: TObject);
var
  Splitter: TSplitter;
  Panel: TPanel;
begin
  Splitter := Self.GetSplitterFromSender(Sender);
  Panel := Self.GetPanelFromSplitter(Splitter);

  case Panel.Align of
    alLeft: Self.LeftDockPanelWidth := Panel.Width;
    alRight: Self.RightDockPanelWidth := Panel.Width;
    alBottom: Self.BottomDockPanelHeight := Panel.Height;
  end;
end;

procedure TFormDockHost.TriggerDockableForm(Sender: TObject);
var
  DockableForm: TFormDockableBase;
  Action: TAction;
begin
  Action := Self.GetActionFromSender(Sender);

  DockableForm := TFormDockableBase(Pointer(Action.Tag));
  if Assigned(DockableForm)
  then begin
    if Action.Checked
    then Self.ShowDockableForm(DockableForm)
    else Self.HideDockableForm(DockableForm);
  end;
end;

function TFormDockHost.UndockingIsAllowed(const Client: TControl; const NewTarget: TWinControl): Boolean;
begin
  Result := FDockingHelper.DockingIsAllowed(Client)
        and FDockingHelper.UndockingIsAllowed(Client as TFormDockableBase, NewTarget);
end;

{$ENDREGION}

end.
