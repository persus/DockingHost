{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
  Note: Host for dockable forms. It is of the shape where it is floating and
        other dockable forms are docked side by side or on top of each other.
        This form is not dackable.
===============================================================================}

unit ClassTFormDockHostJoin;

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

  ClassTFormDockableBase;
{$ENDREGION}

type

  TFormDockHostJoin = class(TFormDockableBase)
  protected
    function GetStandardCloseAction: TCloseAction; override;

  public
    function GetStandardDockHost: TWinControl; override;
    procedure UpdateCaption(const Exclude: TControl); override;

  published
    procedure ControlDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer); override;
    procedure ControlDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState;
                var Accept: Boolean);
    procedure ControlGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
                MousePos: TPoint; var CanDock: Boolean);
  end;

implementation

{$R *.dfm}

{$REGION 'uses'}
uses
  ClassTFormDockHost,
  ClassTFormDockHostTabs;
{$ENDREGION}

{$REGION 'TFormDockHostJoin'}

procedure TFormDockHostJoin.ControlDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  Self.DockManager.ResetBounds(True);
  inherited;
end;

procedure TFormDockHostJoin.ControlDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
begin
  Accept := Self.DockingIsAllowed(Source.Control);
end;

procedure TFormDockHostJoin.ControlGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := Self.DockingIsAllowed(DockClient);
end;

function TFormDockHostJoin.GetStandardCloseAction: TCloseAction;
begin
  Result := caFree;
end;

function TFormDockHostJoin.GetStandardDockHost: TWinControl;
begin
  Result := Self;
end;

procedure TFormDockHostJoin.UpdateCaption(const Exclude: TControl);
begin
  FDockingHelper.UpdateCaption(Exclude);
end;

{$ENDREGION}

end.

