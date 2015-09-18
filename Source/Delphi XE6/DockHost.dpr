{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
===============================================================================}

program DockHost;

uses
  Vcl.Forms,
  IntfDockable in 'IntfDockable.pas',
  DockExceptions in 'DockExceptions.pas',
  ClassTTransparentForm in 'ClassTTransparentForm.pas',
  ClassTFormMain in 'ClassTFormMain.pas' {FormMain},
  ClassTFormDockHost in 'ClassTFormDockHost.pas' {FormDockHost},
  ClassTFormDockableBase in 'ClassTFormDockableBase.pas' {FormDockableBase},
  ClassTFormDockable in 'ClassTFormDockable.pas' {FormDockable},
  ClassTFormDockHostJoin in 'ClassTFormDockHostJoin.pas' {FormDockHostJoin},
  ClassTFormDockHostTabs in 'ClassTFormDockHostTabs.pas' {FormDockHostTabs},
  ClassTDockingHelper in 'ClassTDockingHelper.pas',
  ClassTTransparentDragDockObject in 'ClassTTransparentDragDockObject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
