{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
===============================================================================}

unit IntfDockable;

interface

uses
  System.Classes,
  System.Types,
  System.UITypes,
  VCL.ActnList,
  VCL.Controls;

type

  IDockable = interface
    ['{3AD715CF-16E3-41BC-B049-71B54810C045}']

    procedure DisableDockingAbility;
    procedure EnableDockingAbility;

    procedure Do_Dock(const DockSite: TAnchorKind; const DockType: TAlign; const Minimized: Boolean); overload;
    procedure Do_Dock(const DockHost: TWinControl; const DockType: TAlign); overload;
    procedure Do_Float;

    function Get_BoundsRect: TRect;
    procedure Set_BoundsRect(const Rect: TRect);

    function DockingIsAllowed(const DockClient: TControl): Boolean;
    procedure UpdateCaption(const Exclude: TControl);

    function GetStandardDockHost: TWinControl;
    function GetHostDockSite: TWinControl;

    function GetAllowedAnchors: TAnchors;
    procedure SetAllowedAnchors(const Value: TAnchors);
    function GetControlingAction: TAction;
    procedure SetControlingAction(const Value: TAction);
    function GetStandardCloseAction: TCloseAction;
    procedure SetStandardCloseAction(const Value: TCloseAction);

    property AllowedAnchors: TAnchors read GetAllowedAnchors write SetAllowedAnchors;
    property BoundsRect: TRect read Get_BoundsRect write Set_BoundsRect;
    property ControlingAction: TAction read GetControlingAction write SetControlingAction;
    property StandardCloseAction: TCloseAction read GetStandardCloseAction write SetStandardCloseAction;

  end;

implementation

end.
