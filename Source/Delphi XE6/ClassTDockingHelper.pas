{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
===============================================================================}

unit ClassTDockingHelper;

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
  ClassTTransparentDragDockObject;
{$ENDREGION}

const

  ZoneWidth = 4;

type

  TDockingHelper = class
  {$REGION 'protected'}
  protected
    FHost: IDockable;
    FZoneWidth: Integer;
    FSubZoneWidth: Integer;

    function DetermineDockingRectLeft(var DockRect: TRect; MousePos: TPoint): TAlign;
    function DetermineDockingRectTop(var DockRect: TRect; MousePos: TPoint): TAlign;
    function DetermineDockingRectRight(var DockRect: TRect; MousePos: TPoint): TAlign;
    function DetermineDockingRectBottom(var DockRect: TRect; MousePos: TPoint): TAlign;
    function DetermineDockingRectCenter(var DockRect: TRect; MousePos: TPoint): TAlign;
    procedure FloatLastDockedClient;
    procedure ReDockLastDockedClient(const NewHostDockSite: TWinControl);
  {$ENDREGION}

  public
    constructor Create(const AOwner: IDockable);

    function AnchorsAreSubSet(const AnchorsSuperSet, AnchorsSubSet: TAnchors): Boolean; overload;
    function AnchorsAreSubSet(const Anchors: TAnchors): Boolean; overload;
    function AnchorsAreSuperSet(const Anchors: TAnchors): Boolean;
    function IntersectAnchors(const Anchors1, Anchors2: TAnchors): TAnchors;
    procedure ResetAnchors;

    function ComputeDockingRect(var DockRect: TRect; const X, Y: Integer): TAlign; overload;
    function ComputeDockingRect(var DockRect: TRect; const MousePos: TPoint): TAlign; overload;
    function DockingIsAllowed(const DockClient: TControl): Boolean;
    function TransdockingIsAllowed(const DockClient: TControl): Boolean; overload;
    function TransdockingIsAllowed(const DragDockObject: TTransparentDragDockObject): Boolean; overload;
    function UndockingIsAllowed(const Source: IDockable; const Target: TWinControl): Boolean;

    procedure DoFloatDueToNotAllowedDockingTarget(const DockedForm: IDockable);
    function GetDockHostTabsIfDockedOnControl(const HostDockSite: TWinControl): TForm;
    procedure HandleAttachedActions(const HostDockSite: TWinControl; const Visible: Boolean);
    procedure MarkClientAsDocked(const DragDockObject: TDragDockObject);
    procedure ReleaseLastDockedClient;
    procedure UpdateCaption(const Exclude: TControl);
  end;

implementation

{$REGION 'uses'}
uses

  ClassTFormDockable,
  ClassTFormDockableBase,
  ClassTFormDockHostTabs;
{$ENDREGION}

{$REGION 'implementation'}

constructor TDockingHelper.Create(const AOwner: IDockable);
begin
  FHost := AOwner;
  FZoneWidth := ZoneWidth;
  FSubZoneWidth := FZoneWidth - 1;
end;

function TDockingHelper.AnchorsAreSubSet(const AnchorsSuperSet, AnchorsSubSet: TAnchors): Boolean;
begin
  Result := True;
  if (akLeft in AnchorsSubSet) and (not (akLeft in AnchorsSuperSet)) then Exit(False);
  if (akTop in AnchorsSubSet) and (not (akTop in AnchorsSuperSet)) then Exit(False);
  if (akRight in AnchorsSubSet) and (not (akRight in AnchorsSuperSet)) then Exit(False);
  if (akBottom in AnchorsSubSet) and (not (akBottom in AnchorsSuperSet)) then Exit(False);
end;

function TDockingHelper.AnchorsAreSubSet(const Anchors: TAnchors): Boolean;
begin
  Result := Self.AnchorsAreSubSet((FHost as TFormDockableBase).AllowedAnchors, Anchors);
end;

function TDockingHelper.AnchorsAreSuperSet(const Anchors: TAnchors): Boolean;
begin
  Result := Self.AnchorsAreSubSet(Anchors, (FHost as TFormDockableBase).AllowedAnchors);
end;

function TDockingHelper.ComputeDockingRect(var DockRect: TRect; const X, Y: Integer): TAlign;
begin
  Result := ComputeDockingRect(DockRect, TPoint.Create(X, Y));
end;

function TDockingHelper.ComputeDockingRect(var DockRect: TRect; const MousePos: TPoint): TAlign;
var
  DockHost: TControl;
begin
  Assert(FHost is TControl);
  DockHost := FHost as TControl;

  Result := DetermineDockingRectLeft(DockRect, MousePos);
  if Result = alNone then Result := DetermineDockingRectTop(DockRect, MousePos);
  if Result = alNone then Result := DetermineDockingRectRight(DockRect, MousePos);
  if Result = alNone then Result := DetermineDockingRectBottom(DockRect, MousePos);
  if Result = alNone then Result := DetermineDockingRectCenter(DockRect, MousePos);
  if Result <> alNone
  then begin
    DockRect.TopLeft := DockHost.ClientToScreen(DockRect.TopLeft);
    DockRect.BottomRight := DockHost.ClientToScreen(DockRect.BottomRight);
  end;
end;

function TDockingHelper.DetermineDockingRectBottom(var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockHost: TControl;
begin
  Assert(FHost is TControl);
  DockHost := FHost as TControl;
  Result := alNone;

  DockRect.TopLeft := TPoint.Create(DockHost.ClientWidth div FZoneWidth,
                                    DockHost.ClientHeight div FZoneWidth * FSubZoneWidth);
  DockRect.BottomRight := TPoint.Create(DockHost.ClientWidth div FZoneWidth * FSubZoneWidth,
                                        DockHost.ClientHeight);

  if WinAPI.Windows.PtInRect(DockRect, MousePos)
  then begin
    Result := alBottom;
    DockRect.Left := 0;
    DockRect.Right := DockHost.ClientWidth;
    DockRect.Top := DockHost.ClientHeight div 2;
  end;
end;

function TDockingHelper.DetermineDockingRectCenter(var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockHost: TControl;
begin
  Assert(FHost is TControl);
  DockHost := FHost as TControl;
  Result := alNone;

  DockRect.TopLeft := TPoint.Create(DockHost.ClientWidth div FZoneWidth,
                                    DockHost.ClientHeight div FZoneWidth);
  DockRect.BottomRight := TPoint.Create(DockHost.ClientWidth div FZoneWidth * FSubZoneWidth,
                                        DockHost.ClientHeight div FZoneWidth * FSubZoneWidth);

  if WinAPI.Windows.PtInRect(DockRect, MousePos) then Result := alClient;
end;

function TDockingHelper.DetermineDockingRectLeft(var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockHost: TControl;
begin
  Assert(FHost is TControl);
  DockHost := FHost as TControl;
  Result := alNone;

  DockRect.TopLeft := TPoint.Create(0, 0);
  DockRect.BottomRight := TPoint.Create(DockHost.ClientWidth div FZoneWidth, DockHost.ClientHeight);

  if WinAPI.Windows.PtInRect(DockRect, MousePos)
  then begin
    Result := alLeft;
    DockRect.Right := DockHost.ClientWidth div 2;
  end;
end;

function TDockingHelper.DetermineDockingRectRight(var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockHost: TControl;
begin
  Assert(FHost is TControl);
  DockHost := FHost as TControl;
  Result := alNone;

  DockRect.TopLeft := TPoint.Create(DockHost.ClientWidth div FZoneWidth * FSubZoneWidth, 0);
  DockRect.BottomRight := TPoint.Create(DockHost.ClientWidth, DockHost.ClientHeight);

  if WinAPI.Windows.PtInRect(DockRect, MousePos)
  then begin
    Result := alRight;
    DockRect.Left := DockHost.ClientWidth div 2;
  end;
end;

function TDockingHelper.DetermineDockingRectTop(var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockHost: TControl;
begin
  Assert(FHost is TControl);
  DockHost := FHost as TControl;
  Result := alNone;

  DockRect.TopLeft := TPoint.Create(DockHost.ClientWidth div FZoneWidth, 0);
  DockRect.BottomRight := TPoint.Create(DockHost.ClientWidth div FZoneWidth * FSubZoneWidth,
                                        DockHost.ClientHeight div FZoneWidth);

  if WinAPI.Windows.PtInRect(DockRect, MousePos)
  then begin
    Result := alTop;
    DockRect.Left := 0;
    DockRect.Right := DockHost.ClientWidth;
    DockRect.Bottom := DockHost.ClientHeight div 2;
  end
end;

function TDockingHelper.DockingIsAllowed(const DockClient: TControl): Boolean;
begin
  Result := DockClient is TFormDockable;
end;

procedure TDockingHelper.DoFloatDueToNotAllowedDockingTarget(const DockedForm: IDockable);
begin
  Assert(Assigned(DockedForm));
  DockedForm.Do_Float;
end;

procedure TDockingHelper.FloatLastDockedClient;
var
  StandardDockHost: TWinControl;
begin
  StandardDockHost := FHost.GetStandardDockHost;
  (StandardDockHost.DockClients[0] as TFormDockableBase).Do_Float;
end;

function TDockingHelper.GetDockHostTabsIfDockedOnControl(const HostDockSite: TWinControl): TForm;
var
  iIndex: Integer;
  Control: TControl;
begin
  Assert(Assigned(HostDockSite));
  Result := nil;

  for iIndex := 0 to HostDockSite.DockClientCount - 1
  do begin
    Control := HostDockSite.DockClients[iIndex];
    if Control is TFormDockHostTabs then
    begin
      Exit(Control as TForm);
    end;
  end;
end;

procedure TDockingHelper.HandleAttachedActions(const HostDockSite: TWinControl; const Visible: Boolean);
var
  iIndex: Integer;
  AttachedAction: TAction;
begin
  for iIndex := 0 to HostDockSite.DockClientCount - 1
  do begin
    AttachedAction := (HostDockSite.DockClients[iIndex] as TFormDockableBase).ControlingAction;
    if Assigned(AttachedAction) then AttachedAction.Checked := Visible and HostDockSite.DockClients[iIndex].Visible;
  end;
end;

function TDockingHelper.IntersectAnchors(const Anchors1, Anchors2: TAnchors): TAnchors;
begin
  Result := [];
  if (akLeft in Anchors1) and (akLeft in Anchors2) then Result := Result + [akLeft];
  if (akTop in Anchors1) and (akTop in Anchors2) then Result := Result + [akTop];
  if (akRight in Anchors1) and (akRight in Anchors2) then Result := Result + [akRight];
  if (akBottom in Anchors1) and (akBottom in Anchors2) then Result := Result + [akBottom];
end;

procedure TDockingHelper.MarkClientAsDocked(const DragDockObject: TDragDockObject);
begin
  if DragDockObject is TTransparentDragDockObject
  then (DragDockObject as TTransparentDragDockObject).DockSuccess := True;
end;

procedure TDockingHelper.ReDockLastDockedClient(const NewHostDockSite: TWinControl);
var
  StandardDockHost: TWinControl;
  FormDockable: TFormDockable;
begin
  StandardDockHost := FHost.GetStandardDockHost;
  FormDockable := (StandardDockHost.DockClients[0] as TFormDockable);

  FormDockable.Hide;
  try
    FormDockable.Do_Float;
    FormDockable.ManualDock(NewHostDockSite, nil, alClient)
  finally
    FormDockable.Show;
  end;
end;

procedure TDockingHelper.ReleaseLastDockedClient;
var
  HostDockSite: TWinControl;
begin
  HostDockSite := (FHost as TWinControl).HostDockSite;
  if Assigned(HostDockSite)
  then Self.RedockLastDockedClient(HostDockSite)
  else Self.FloatLastDockedClient;
end;

procedure TDockingHelper.ResetAnchors;
var
  iIndex: Integer;
  FormDockable: TFormDockableBase;
  HostDockSite: TWinControl;
  FormDocked: TFormDockableBase;
begin
  FormDockable := FHost as TFormDockableBase;
  FormDockable.AllowedAnchors := [akLeft, akTop, akRight, akBottom];
  HostDockSite := FormDockable.GetStandardDockHost;

  for iIndex := 0 to HostDockSite.DockClientCount - 1
  do begin
    FormDocked := HostDockSite.DockClients[iIndex] as TFormDockableBase;
    FormDockable.AllowedAnchors := Self.IntersectAnchors(FormDockable.AllowedAnchors, FormDocked.AllowedAnchors);
  end;
end;

function TDockingHelper.TransdockingIsAllowed(const DockClient: TControl): Boolean;
var
  DragDockObject: TTransparentDragDockObject;
begin
  DragDockObject := nil;
  if DockClient is TFormDockableBase then DragDockObject := (DockClient as TFormDockableBase).DragDockObject;
  Result := Self.TransdockingIsAllowed(DragDockObject);
end;

function TDockingHelper.TransdockingIsAllowed(const DragDockObject: TTransparentDragDockObject): Boolean;
begin
  Result := (not Assigned(DragDockObject)) or (DragDockObject.DockSuccess);
end;

function TDockingHelper.UndockingIsAllowed(const Source: IDockable; const Target: TWinControl): Boolean;
var
  DockableTarget: IDockable;
begin
  Result := True;
  if System.SysUtils.Supports(Target, IDockable, DockableTarget)
  then Result := Self.AnchorsAreSubSet(DockableTarget.AllowedAnchors, Source.AllowedAnchors);
end;

procedure TDockingHelper.UpdateCaption(const Exclude: TControl);
var
  iIndex: Integer;
  HostDockSite: TWinControl;
  HostForm: TForm;
  DockForm: TForm;
begin
  // If a dockable form is undocking, it will pass itself in as Exclude because
  // even it hasn't actually been taken out of the DockClient array at this point
  Assert(FHost is TControl);
  HostDockSite := FHost.GetStandardDockHost;
  HostForm := FHost as TForm;
  HostForm.Caption := '';

  for iIndex := 0 to HostDockSite.DockClientCount - 1
  do begin

    if HostDockSite.DockClients[iIndex].Visible and (HostDockSite.DockClients[iIndex] <> Exclude)
    then begin
      DockForm := HostDockSite.DockClients[iIndex] as TForm;
      if HostForm.Caption = ''
      then HostForm.Caption := DockForm.Caption
      else HostForm.Caption := HostForm.Caption + ' • ' + DockForm.Caption;
    end;

  end;

end;

{$ENDREGION}

end.
