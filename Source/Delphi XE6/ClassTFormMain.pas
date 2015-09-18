{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
  Note: This is the MainForm and the standard-host for doackable forms.
===============================================================================}

unit ClassTFormMain;

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

  ClassTFormDockHost, Vcl.ToolWin;
{$ENDREGION}

type

  TFormMain = class(TFormDockHost)
  private
    procedure SetAsMainForm;

  public
    procedure AfterConstruction; override;

  published
    tb_Main: TToolBar;
    tbtn_Static: TToolButton;
    tbtn_Dynamic: TToolButton;
    al_Main: TActionList;
    act_Static: TAction;
    act_Dynamic: TAction;

    procedure act_DynamicExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

{$REGION 'uses'}
uses
  DockExceptions,
  ClassTFormDockable,
  ClassTFormDockHostTabs,
  ClassTFormDockHostJoin;
{$ENDREGION}

{$REGION 'TFormMain'}

{$ENDREGION}

procedure TFormMain.act_DynamicExecute(Sender: TObject);
var
  DockableForm: TFormDockable;
begin
  DockableForm := TFormDockable.Create(nil, [akLeft, akRight, akBottom], caFree, nil);
  DockableForm.Caption := 'Dynamic form';
  Self.ShowDockableForm(DockableForm);
end;

procedure TFormMain.AfterConstruction;
var
  DockableForm: TFormDockable;
begin
  Self.SetAsMainForm;
  DockableForm := TFormDockable.Create(nil, [akLeft, akRight, akBottom], caHide, act_Static);
  DockableForm.Caption := 'Static form';
  DockableForm.Do_Dock(akLeft, alLeft, False);
  act_Static.Checked := True;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  act_Static.OnExecute := Self.TriggerDockableForm;
end;

procedure TFormMain.SetAsMainForm;
var
  P: Pointer;
begin
  P := @Application.MainForm;
  Pointer(P^) := Self;
end;

end.
