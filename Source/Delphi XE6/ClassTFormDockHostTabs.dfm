object FormDockHostTabs: TFormDockHostTabs
  Left = 565
  Top = 626
  BorderStyle = bsSizeToolWin
  Caption = 'DackHostTabs'
  ClientHeight = 222
  ClientWidth = 296
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 296
    Height = 222
    Align = alClient
    DockSite = True
    TabOrder = 0
    TabPosition = tpBottom
    OnDockDrop = ControlDockDrop
    OnDockOver = ControlDockOver
    OnDrawTab = PageControlDrawTab
    OnGetSiteInfo = ControlGetSiteInfo
    OnUnDock = ControlUnDock
  end
end