{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
  Note: Base class for dockable forms.

        It is implementing just some methods and properties all dockable forms
        should have.

        It is not meant to be used directly. It is also not able to dock or to
        serve as dock-hos ont its own. It won't be accepted as docakbel form
        either.

        Usualy you will inherit from TFormDockable, but not from this class.
        There is only one reason to inherit from this class directly: You want
        to be part of the docking system but you don't want to be dockable and
        don't want to serve as dock-host.
===============================================================================}

unit ClassTFormDockableBase;

interface

{$REGION 'uses'}
uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,

  WinAPI.Windows,
  WinAPI.Messages,

  VCL.ActnList,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.Forms,

  IntfDockable,
  ClassTDockingHelper,
  ClassTTransparentDragDockObject;
{$ENDREGION}

type

  TFormDockableBase = class(TForm, IDockable)
  {$REGION 'protected'}
  protected
    FAllowedAnchors: TAnchors;
    FStandardCloseAction: TCloseAction;
    FControlingAction: TAction;
    FDockingHelper: TDockingHelper;
    FUndockedLeft: Integer;
    FUndockedTop: Integer;
    FDockType: TAlign;
    FDragDockObject: TTransparentDragDockObject;

    function AnchorMatchesAlign(const Anchor: TAnchorKind; const Align: TAlign): Boolean;

    procedure CloseDockClient(Sender: TObject; var Action: TCloseAction);
    procedure CloseDockHost(Sender: TObject; var Action: TCloseAction);

    function GetAllowedAnchors: TAnchors;
    procedure SetAllowedAnchors(const Value: TAnchors);
    function GetControlingAction: TAction; virtual;
    procedure SetControlingAction(const Value: TAction); virtual;
    function GetStandardCloseAction: TCloseAction; virtual;
    procedure SetStandardCloseAction(const Value: TCloseAction); virtual;

    function UndockingIsAllowed(const Client: TControl; const NewTarget: TWinControl): Boolean;
  {$ENDREGION}

  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AOwner: TComponent;
                       const AAllowedAnchors: TAnchors;
                       const AStandardCloseAction: TCloseAction;
                       const AControlingAction: TAction); reintroduce; overload;
    destructor Destroy; override;

    procedure DisableDockingAbility;
    procedure EnableDockingAbility;

    procedure Do_Dock(const DockSite: TAnchorKind; const DockType: TAlign; const Minimized: Boolean); overload; virtual;
    procedure Do_Dock(const DockHost: TWinControl; const DockType: TAlign); overload; virtual;
    procedure Do_Float;
    procedure RequestClosingDockedControl(const AControl: TControl; const Action: TCloseAction);

    function GetHostDockSite: TWinControl;
    function GetStandardDockHost: TWinControl; virtual; abstract;
    function Get_BoundsRect: TRect;
    procedure Set_BoundsRect(const Rect: TRect);

    function DockingIsAllowed(const DockClient: TControl): Boolean;
    procedure UpdateCaption(const Exclude: TControl); virtual; abstract;

  published
    procedure ControlDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer); virtual;
    procedure ControlUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean); virtual;

    procedure FormClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure FormShow(Sender: TObject); virtual;
    procedure FormStartDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer); virtual;

    property DockType: TAlign read FDockType write FDockType;
    property AllowedAnchors: TAnchors read GetAllowedAnchors write SetAllowedAnchors;
    property ControlingAction: TAction read GetControlingAction write SetControlingAction;
    property StandardCloseAction: TCloseAction read GetStandardCloseAction write SetStandardCloseAction;

    property DragDockObject: TTransparentDragDockObject read FDragDockObject;
    property UndockedLeft: Integer read FUndockedLeft;
    property UndockedTop: Integer read FUndockedTop;
  end;

implementation

{$R *.dfm}

{$REGION 'uses'}
uses
  ClassTFormDockHost,
  ClassTFormDockHostTabs,
  ClassTFormDockHostJoin;
{$ENDREGION}

{$REGION 'TFormDockableBase'}

constructor TFormDockableBase.Create(AOwner: TComponent);
begin
  inherited;

  FAllowedAnchors := [akLeft, akTop, akRight, akBottom];
  FStandardCloseAction := caFree;
  FControlingAction := nil;
  FDragDockObject := nil;
  FDockingHelper := TDockingHelper.Create(Self);

  Self.UpdateCaption(nil);
end;

constructor TFormDockableBase.Create(const AOwner: TComponent;
                                     const AAllowedAnchors: TAnchors;
                                     const AStandardCloseAction: TCloseAction;
                                     const AControlingAction: TAction);
begin
  Self.Create(AOwner);
  Self.AllowedAnchors := AAllowedAnchors;
  Self.StandardCloseAction := AStandardCloseAction;
  Self.ControlingAction := AControlingAction;
end;

destructor TFormDockableBase.Destroy;
begin
  System.SysUtils.FreeAndNil(FDockingHelper);
  inherited;
end;

function TFormDockableBase.AnchorMatchesAlign(const Anchor: TAnchorKind; const Align: TAlign): Boolean;
begin
  Result := ((Anchor = akLeft) and (Align = alLeft))
         or ((Anchor = akTop) and (Align = alTop))
         or ((Anchor = akRight) and (Align = alRight))
         or ((Anchor = akBottom) and (Align = alBottom));
end;

procedure TFormDockableBase.CloseDockClient(Sender: TObject; var Action: TCloseAction);
begin

  if Self.HostDockSite is TFormDockableBase
  then (Self.HostDockSite as TFormDockableBase).RequestClosingDockedControl(Self, Action)

  // If docked to a panel, tell the panel to hide itself. If there are other
  // visible dock clients on the panel, HideDockPanel won't allow it to be hidden

  else if Self.HostDockSite is TPanel
  then (Application.MainForm as TFormDockHost).HideDockPanel(Self.HostDockSite as TPanel, Action);

  if Assigned(Self.ControlingAction) then Self.ControlingAction.Checked := False;
end;

procedure TFormDockableBase.CloseDockHost(Sender: TObject; var Action: TCloseAction);
var
  StandardDockHost: TWinControl;
begin
  StandardDockHost := Self.GetStandardDockHost;
  if StandardDockHost.DockClientCount = 1
  then begin
    FDockingHelper.ReleaseLastDockedClient;
    Action := Self.StandardCloseAction;
  end
  else begin
    FDockingHelper.HandleAttachedActions(StandardDockHost, False);
    Action := caHide;
    Self.CloseDockClient(Sender, Action);
  end;
end;

procedure TFormDockableBase.ControlDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
var
  FormDockable: TFormDockableBase;
begin
  FormDockable := Source.Control as TFormDockableBase;
  FormDockable.DisableDockingAbility;
  FormDockable.DockType := Source.DropAlign;

  Self.UpdateCaption(nil);
  Self.AllowedAnchors := FDockingHelper.IntersectAnchors(Self.AllowedAnchors, FormDockable.AllowedAnchors);
  FDockingHelper.MarkClientAsDocked(Source);
end;

procedure TFormDockableBase.ControlUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
var
  StandardDockHost: TWinControl;
begin
  // Only two dock clients means the host must be destroyed and the remaining
  // window undocked to its old position and size.

  Allow := FDockingHelper.TransdockingIsAllowed(Client) or Self.UndockingIsAllowed(Client, NewTarget);
  if Allow
  then begin
    StandardDockHost := Self.GetStandardDockHost;
    if (StandardDockHost.DockClientCount = 2) and (NewTarget <> Self) then PostMessage(Self.Handle, WM_CLOSE, 0, 0);

    (Client as TFormDockableBase).EnableDockingAbility;
    Self.UpdateCaption(Client);
    FDockingHelper.ResetAnchors;
  end
  else begin
    FDockingHelper.DoFloatDueToNotAllowedDockingTarget(Client as TFormDockableBase);
  end;

end;

procedure TFormDockableBase.DisableDockingAbility;
begin
  Self.DockSite := False;
end;

function TFormDockableBase.DockingIsAllowed(const DockClient: TControl): Boolean;
begin
  Result := FDockingHelper.DockingIsAllowed(DockClient)
        and FDockingHelper.AnchorsAreSubSet((DockClient as TFormDockableBase).AllowedAnchors)
        and (not (DockClient is TFormDockHostJoin))
        and (not (DockClient is TFormDockHostTabs));
end;

procedure TFormDockableBase.Do_Dock(const DockSite: TAnchorKind; const DockType: TAlign; const Minimized: Boolean);
begin
  // We do not support docking for this early stage of inheritance.
end;

procedure TFormDockableBase.Do_Dock(const DockHost: TWinControl; const DockType: TAlign);
begin
  // // We do not support docking for this early stage of inheritance.
end;

procedure TFormDockableBase.Do_Float;
begin
  // Float the control with its original size.
  Self.ManualFloat(TRect.Create(FUndockedLeft, FUndockedTop, FUndockedLeft + UndockWidth, FUndockedTop + UndockHeight));
  Self.EnableDockingAbility;
  Self.DockType := alNone;
end;

procedure TFormDockableBase.EnableDockingAbility;
begin
  Self.DockSite := True;
end;

procedure TFormDockableBase.FormClose(Sender: TObject; var Action: TCloseAction);
var
  StandardDockHost: TWinControl;
begin
  StandardDockHost := Self.GetStandardDockHost;
  if StandardDockHost.DockClientCount > 0
  then begin
    Self.CloseDockHost(Sender, Action);
  end
  else begin
    Action := Self.StandardCloseAction;
    Self.CloseDockClient(Sender, Action);
  end;
end;

{-------------------------------------------------------------------------------
  Note:
-------------------------------------------------------------------------------}

procedure TFormDockableBase.FormEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  try
    if not Assigned((Sender as TWinControl).Parent) then Self.Set_BoundsRect(Self.Get_BoundsRect);
  finally
    FDragDockObject := nil;
  end;
end;

procedure TFormDockableBase.FormShow(Sender: TObject);
begin
  if Assigned(Self.ControlingAction) then Self.ControlingAction.Checked := True;
  FDockingHelper.HandleAttachedActions(Self.GetStandardDockHost, True);
  Self.UpdateCaption(nil);
end;

procedure TFormDockableBase.FormStartDock(Sender: TObject; var DragObject: TDragDockObject);
begin
  FDragDockObject := TTransparentDragDockObject.Create(Self);
  DragObject := FDragDockObject;
end;

function TFormDockableBase.GetAllowedAnchors: TAnchors;
begin
  Result := FAllowedAnchors;
end;

function TFormDockableBase.GetControlingAction: TAction;
begin
  Result := FControlingAction;
end;

function TFormDockableBase.GetHostDockSite: TWinControl;
begin
  Result := Self.HostDockSite;
end;

function TFormDockableBase.GetStandardCloseAction: TCloseAction;
begin
  Result := FStandardCloseAction;
end;

function TFormDockableBase.Get_BoundsRect: TRect;
begin
  if Assigned(FDragDockObject)
  then Result := FDragDockObject.BoundsRect
  else Result := Self.BoundsRect;
end;

procedure TFormDockableBase.RequestClosingDockedControl(const AControl: TControl; const Action: TCloseAction);
var
  StandardDockHost: TWinControl;
  DockClientCountOffset: Integer;
begin
  Self.UpdateCaption(AControl);

  // It is funny that when the Action is caHide, the DockClientCount counts
  // minus one. But if the Action is caFree the DockClientCount seems to count
  // minus one later an we do get the wrong count of docked clients.
  // So we need an offset here.

  DockClientCountOffset := 0;
  if Action = caFree then DockClientCountOffset := 1;

  StandardDockHost := Self.GetStandardDockHost;
  if StandardDockHost.DockClientCount - DockClientCountOffset = 1
  then FDockingHelper.ReleaseLastDockedClient
  else if StandardDockHost.VisibleDockClientCount <= 1
  then Self.Hide;
end;

procedure TFormDockableBase.SetAllowedAnchors(const Value: TAnchors);
begin
  FAllowedAnchors := Value;
end;

procedure TFormDockableBase.SetControlingAction(const Value: TAction);
begin
  if FControlingAction <> Value
  then begin

    FControlingAction := Value;
    if Assigned(FControlingAction)
    then begin
      FControlingAction.Tag := NativeInt(Pointer(Self));
      Self.StandardCloseAction := caHide;
    end;

  end;
end;

procedure TFormDockableBase.SetStandardCloseAction(const Value: TCloseAction);
begin
  FStandardCloseAction := Value;
end;

procedure TFormDockableBase.Set_BoundsRect(const Rect: TRect);
begin
  Self.BoundsRect := Rect;
end;

function TFormDockableBase.UndockingIsAllowed(const Client: TControl; const NewTarget: TWinControl): Boolean;
begin
  Result := Self.DockingIsAllowed(Client) and FDockingHelper.UndockingIsAllowed(Client as TFormDockableBase, NewTarget);
end;

{$ENDREGION}

end.
