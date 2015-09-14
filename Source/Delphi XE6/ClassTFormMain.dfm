inherited FormMain: TFormMain
  Caption = 'DockingHost example'
  ClientHeight = 546
  ClientWidth = 686
  OnCreate = FormCreate
  ExplicitWidth = 702
  ExplicitHeight = 584
  PixelsPerInch = 96
  TextHeight = 13
  inherited spl_DockLeft: TSplitter
    Top = 29
    Height = 517
    ExplicitTop = 29
    ExplicitHeight = 517
  end
  inherited spl_DockRight: TSplitter
    Left = 634
    Top = 29
    Height = 517
    ExplicitLeft = 634
    ExplicitTop = 29
    ExplicitHeight = 517
  end
  inherited pnl_DockLeft: TPanel
    Top = 29
    Height = 517
    ExplicitTop = 29
    ExplicitHeight = 517
  end
  inherited dts_DockTabSetLeft: TDockTabSet
    Top = 29
    Height = 517
    ExplicitTop = 29
    ExplicitHeight = 517
  end
  inherited pnl_DockRight: TPanel
    Left = 638
    Top = 29
    Height = 517
    ExplicitLeft = 638
    ExplicitTop = 29
    ExplicitHeight = 517
  end
  inherited dts_DockTabSheetRight: TDockTabSet
    Left = 662
    Top = 29
    Height = 517
    ExplicitLeft = 662
    ExplicitTop = 29
    ExplicitHeight = 517
  end
  inherited pnl_Main: TPanel
    Top = 29
    Width = 582
    Height = 517
    ExplicitTop = 29
    ExplicitWidth = 582
    ExplicitHeight = 517
    inherited spl_DockBottom: TSplitter
      Top = 465
      Width = 582
      ExplicitTop = 465
      ExplicitWidth = 582
    end
    inherited dts_DockTabSetBottom: TDockTabSet
      Top = 493
      Width = 582
      ExplicitTop = 493
      ExplicitWidth = 582
    end
    inherited pnl_DockBottom: TPanel
      Top = 469
      Width = 582
      ExplicitTop = 469
      ExplicitWidth = 582
    end
  end
  object tb_Main: TToolBar
    Left = 0
    Top = 0
    Width = 686
    Height = 29
    ButtonHeight = 19
    ButtonWidth = 127
    Caption = 'tb_Main'
    List = True
    ShowCaptions = True
    TabOrder = 5
    object tbtn_Static: TToolButton
      Left = 0
      Top = 0
      Action = act_Static
    end
    object tbtn_Dynamic: TToolButton
      Left = 127
      Top = 0
      Action = act_Dynamic
    end
  end
  object al_Main: TActionList
    Left = 60
    Top = 37
    object act_Static: TAction
      AutoCheck = True
      Caption = 'Static dockable form'
    end
    object act_Dynamic: TAction
      Caption = 'Dynamic dockable forms'
      OnExecute = act_DynamicExecute
    end
  end
end
