{===============================================================================
  Author: Susic, Perica (pero.s@gmx.de)
  License: BSD (http://www.opensource.org/licenses/bsd-license.php)
===============================================================================}

unit ClassTTransparentForm;

interface

uses
  WinAPI.Windows,
  VCL.Controls,
  VCL.Forms;

type

  TTransparentForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

procedure TTransparentForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

end.


