{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
  Note: Regular predeccessor for all docable forms.

        While the successors are all inherited on its own, their DFMs are not.
        This has some reasons. One is that the successors may define by themselves
        how to react while docking is in progress.

        For all inherited forms do not inherit the DFMs. The following rules must
        been followed.

        In the inherited forms the dockingevents have to be activated. These are
        useually this ones.
        - OnDockDrop
        - OnDockOver
        - OnGetSiteInfo
        - OnUndock

        The event OnStartDock should not be implemented or overriden. It is in
        control of the blue rectangle that is shown instead of the form itself
        while dragging. The blue rectangle is neccessary due to a performance
        issue of the VCL while dragging a docakbe form. I can't tell why this is so.
===============================================================================}

unit ClassTFormDockable;

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
  VCL.DockTabSet,
  VCL.ExtCtrls,
  VCL.Forms,

  ClassTFormDockableBase;
{$ENDREGION}

type

  TFormDockable = class(TFormDockableBase)
  {$REGION 'private'}
  private
    procedure CMDockClient(var Mssg: TCMDockClient); message CM_DOCKCLIENT;
    procedure WMNCLButtonDown(var Mssg: TMessage); message WM_NCLBUTTONDOWN;

    procedure DockClientToMainJoinDockHost(const Mssg: TCMDockClient; const DockType: TAlign);
    procedure DockClientToMainTabsDockHost(const Mssg: TCMDockClient; const DockType: TAlign);
    procedure DockClientToFloatingDockHost(const Host: TFormDockableBase; const Mssg: TCMDockClient; const DockType: TAlign);
  {$ENDREGION}

  {$REGION 'protected'}
  protected
    function CreateDockHostTabsAndDockIt(const DockHost: TWinControl): TForm;

    function DetermineDockHost(const DockSite: TAnchorKind; var DockType: TAlign;
               const Minimized: Boolean): TWinControl; overload;
    function DetermineDockHost(const DockSite: TAnchorKind; const WinControl: TWinControl;
               const ClassOfTWinControl: TWinControlClass): TWinControl; overload;
    function DetermineDockHostByClass(const DockSite: TAnchorKind; const WinControl: TWinControl;
               const ClassOfTWinControl: TWinControlClass): TWinControl;
    function DetermineAllowedDockHost(const DockHostBase: TWinControl; var DockType: TAlign): TWinControl;
  {$ENDREGION}

  public
    procedure Do_Dock(const DockSite: TAnchorKind; const DockType: TAlign; const Minimized: Boolean); overload; override;
    procedure Do_Dock(const DockHost: TWinControl; const DockType: TAlign); overload; override;

    function GetStandardDockHost: TWinControl; override;
    procedure UpdateCaption(const Exclude: TControl); override;

  published
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  end;

implementation

{$R *.dfm}

{$REGION 'uses'}
uses
  ClassTFormDockHost,
  ClassTFormDockHostTabs,
  ClassTFormDockHostJoin,
  ClassTTransparentDragDockObject;
{$ENDREGION}

{$REGION 'TFormDockable'}

procedure TFormDockable.CMDockClient(var Mssg: TCMDockClient);
var
  Rect: TRect;
  DockType: TAlign;

begin
  // Overriding this message allows the dock form to create host forms depending
  // on the mouse position when docking occurs. If we don't override this message,
  // the form will use VCL's default DockManager.
  // The only time ManualDock can be safely called during a drag operation is we
  // override processing of CM_DOCKCLIENT.

  if Mssg.DockSource.Control is TFormDockableBase
  then begin

    // Find out how to dock (Using a TAlign as the result of ComputeDockingRect)
    DockType := FDockingHelper.ComputeDockingRect(Rect, Mssg.MousePos.X, Mssg.MousePos.Y);
    // Mark the dock-process as succes (needed for later undocking permission)
    FDockingHelper.MarkClientAsDocked(Mssg.DockSource);

    // If we are over a dockable form docked to a panel in the main window,
    // manually dock the dragged form to the panel with the correct orientation.
    if HostDockSite is TPanel
    then begin
      if DockType = alClient
      then DockClientToMainTabsDockHost(Mssg, DockType)
      else DockClientToMainJoinDockHost(Mssg, DockType);
    end

    // If we are over a dockable form floating outside the main window,
    // manually dock the dragged form to the panel with the correct orientation.
    else if not Assigned(HostDockSite)
    then begin
      if DockType = alClient
      then DockClientToFloatingDockHost(TFormDockHostTabs.Create(Application), Mssg, DockType)
      else DockClientToFloatingDockHost(TFormDockHostJoin.Create(Application), Mssg, DockType);
    end

    else begin
      if DockType = alClient
      then DockClientToFloatingDockHost(HostDockSite.Parent as TFormDockHostTabs, Mssg, DockType)
      else DockClientToFloatingDockHost(HostDockSite as TFormDockHostJoin, Mssg, DockType);
    end;

  end;

end;

function TFormDockable.DetermineDockHost(const DockSite: TAnchorKind; var DockType: TAlign; const Minimized: Boolean): TWinControl;
begin
  Result := nil;
  if (Assigned(Application.MainForm)) and (Application.MainForm is TFormDockHost)
  then begin

    if Minimized
    then Result := Self.DetermineDockHost(DockSite, Application.MainForm, TDockTabSet)
    else Result := Self.DetermineDockHost(DockSite, Application.MainForm, TPanel);

    if Assigned(Result) then Result := Self.DetermineAllowedDockHost(Result, DockType);

  end;
end;

function TFormDockable.CreateDockHostTabsAndDockIt(const DockHost: TWinControl): TForm;
begin
  Result := TFormDockHostTabs.Create(Application);
  (Result as TFormDockHostTabs).Do_Dock(DockHost, alClient);
  (Application.MainForm as TFormDockHost).ShowDockableForm(Result as TFormDockHostTabs);
  (Application.MainForm as TFormDockHost).DoDockOnPanel(DockHost as TPanel);
end;

function TFormDockable.DetermineAllowedDockHost(const DockHostBase: TWinControl; var DockType: TAlign): TWinControl;
var
  MyForm: TForm;
begin
  Result := DockHostBase;
  Assert(Assigned(DockHostBase));

  MyForm := FDockingHelper.GetDockHostTabsIfDockedOnControl(DockHostBase);
  if Assigned(MyForm) and (MyForm is TFormDockHostTabs)
  then begin
    Result := (MyForm as TFormDockHostTabs).GetStandardDockHost;
    DockType := alClient;
  end

  else if DockType = alClient
  then begin

    if DockHostBase.DockClientCount > 0
    then begin
      DockType := alLeft;
    end
    else begin
      MyForm := Self.CreateDockHostTabsAndDockIt(DockHostBase);
      Result := (MyForm as TFormDockHostTabs).GetStandardDockHost;
    end;

  end;

end;

function TFormDockable.DetermineDockHost(const DockSite: TAnchorKind; const WinControl: TWinControl;
           const ClassOfTWinControl: TWinControlClass): TWinControl;
begin
  Result := Self.DetermineDockHostByClass(DockSite, Application.MainForm, ClassOfTWinControl);
  if not Assigned(Result)
  then Result := Self.DetermineDockHostByClass(DockSite, (Application.MainForm as TFormDockHost).FrameHost, ClassOfTWinControl);
end;

function TFormDockable.DetermineDockHostByClass(const DockSite: TAnchorKind; const WinControl: TWinControl;
           const ClassOfTWinControl: TWinControlClass): TWinControl;
var
  Control: TControl;
  iIndex: Integer;
begin
  Result := nil;
  Assert(Assigned(WinControl));

  for iIndex := 0 to WinControl.ControlCount - 1
  do begin
    Control := WinControl.Controls[iIndex];
    if (Control is ClassOfTWinControl) and ((Control as TWinControl).DockSite) and (Self.AnchorMatchesAlign(DockSite, Control.Align))
    then Exit(Control as TWinControl);
  end;
end;

procedure TFormDockable.DockClientToFloatingDockHost(const Host: TFormDockableBase;
                                                     const Mssg: TCMDockClient;
                                                     const DockType: TAlign);
begin
  // If DockType <> alClient, create the specific DockHost and manually dock both forms to it.
  Assert(Assigned(Host));
  Host.BoundsRect := Self.BoundsRect;
  Self.ManualDock(Host.GetStandardDockHost, nil, alNone);
  Mssg.DockSource.Control.ManualDock(Host.GetStandardDockHost, nil, DockType);
  Host.Visible := True;
end;

procedure TFormDockable.DockClientToMainJoinDockHost(const Mssg: TCMDockClient; const DockType: TAlign);
begin
  // The JoinDockHost ist managed automaticly by the panels on the MainForm
  Mssg.DockSource.Control.ManualDock(HostDockSite, nil, DockType)
end;

procedure TFormDockable.DockClientToMainTabsDockHost(const Mssg: TCMDockClient; const DockType: TAlign);
var
  Host: TFormDockHostTabs;
begin
  // Create a TabDockHost and manually dock both forms to the PageControl owned by the TabDockHost.
  Host := TFormDockHostTabs.Create(Application);
  Host.BoundsRect := Self.BoundsRect;
  Host.ManualDock(HostDockSite, nil, DockType);
  Self.ManualDock(Host.GetStandardDockHost, nil, alClient);
  Mssg.DockSource.Control.ManualDock(Host.GetStandardDockHost, nil, alClient);
  Host.Visible := True;
end;

{-------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

procedure TFormDockable.Do_Dock(const DockSite: TAnchorKind; const DockType: TAlign; const Minimized: Boolean);
var
  DockHost: TWinControl;
  MyDockType: TAlign;
begin
  // Try to determine the application's main form. Then dock to it.
  MyDockType := DockType;

  DockHost := Self.DetermineDockHost(DockSite, MyDockType, Minimized);
  if Assigned(DockHost)
  then begin
    Self.Do_Dock(DockHost, MyDockType);
    (Application.MainForm as TFormDockHost).ShowDockableForm(Self);
    if DockHost is TPageControl then (DockHost as TPageControl).ActivePage := Self.Parent as TTabSheet;
  end;
end;

procedure TFormDockable.Do_Dock(const DockHost: TWinControl; const DockType: TAlign);
begin
  Assert(Assigned(DockHost));
  Self.ManualDock(DockHost, nil, DockType);
  Self.DockType := DockType;
end;

procedure TFormDockable.FormDockOver(Sender: TObject;
                                     Source: TDragDockObject;
                                     X, Y: Integer;
                                     State: TDragState;
                                     var Accept: Boolean);
var
  Rect: TRect;
begin
  // Check if source is valid and if so, draw dock preview depending on where
  // the cursor is relative to our client area
  Accept := Self.DockSite and Self.DockingIsAllowed(Source.Control);
  if Accept and (FDockingHelper.ComputeDockingRect(Rect, TPoint.Create(X, Y)) <> alNone)
  then Source.DockRect := Rect;
end;

function TFormDockable.GetStandardDockHost: TWinControl;
begin
  Result := Self;
end;

procedure TFormDockable.UpdateCaption(const Exclude: TControl);
begin
  // Must be overriden
end;

procedure TFormDockable.WMNCLButtonDown(var Mssg: TMessage);
begin
  //  This event happen when mouse click in caption save initial window position
  inherited;
  FUndockedLeft := Left;
  FUndockedTop := Top;
end;

{$ENDREGION}

end.
