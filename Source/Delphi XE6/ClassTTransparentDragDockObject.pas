{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
  Note: Semi-transparent dragging-object for dockable-forms
===============================================================================}

unit ClassTTransparentDragDockObject;

interface

{$REGION 'uses'}
uses
  System.Classes,
  System.SysUtils,

  WinAPI.Windows,

  VCL.Controls,
  VCL.Forms,
  VCL.DockTabSet,
  VCL.Graphics,

  ClassTTransparentForm;
{$ENDREGION}

type

  TTransparentDragDockObject = class(TDragDockObjectEx)
  {$REGION 'protected'}
  protected
    FTransparentForm: TTransparentForm;
    FDockSuccess: Boolean;
    function GetBoundsRect: TRect;
    function GetEraseWhenMoving: Boolean; override;
    procedure DrawDragDockImage; override;
    procedure EraseDragDockImage; override;
  {$ENDREGION}

  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    property BoundsRect: TRect read GetBoundsRect;
    property DockSuccess: Boolean read FDockSuccess write FDockSuccess;
  end;

implementation

{$REGION 'TTransparentDragDockObject'}

constructor TTransparentDragDockObject.Create(AControl: TControl);
const
  AlphaBlendValue = 100;
begin
  inherited;
  if not Assigned(FTransparentForm)
  then begin
    FTransparentForm := TTransparentForm.CreateNew(Application);
    FTransparentForm.AlphaBlend:= True;
    FTransparentForm.AlphaBlendValue:= AlphaBlendValue;
    FTransparentForm.BorderStyle:= bsNone;
    FTransparentForm.Color:= clHighlight;
    FTransparentForm.FormStyle:= fsStayOnTop;
  end;
end;

destructor TTransparentDragDockObject.Destroy;
begin
  FreeAndNil(FTransparentForm);
  inherited;
end;

procedure TTransparentDragDockObject.DrawDragDockImage;
begin
  if Assigned(FTransparentForm)
  then begin
    FTransparentForm.BoundsRect:= DockRect;
    if not FTransparentForm.Visible then FTransparentForm.Show;
  end;
end;

procedure TTransparentDragDockObject.EraseDragDockImage;
begin
  FTransparentForm.Hide;
end;

function TTransparentDragDockObject.GetBoundsRect: TRect;
begin
  Result := FTransparentForm.BoundsRect;
end;

function TTransparentDragDockObject.GetEraseWhenMoving: Boolean;
begin
  Result:= False;
end;

{$ENDREGION}

end.
