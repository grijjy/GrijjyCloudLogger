object FormConnect: TFormConnect
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Connect'
  ClientHeight = 162
  ClientWidth = 207
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxSettings: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 191
    Height = 113
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    Caption = 'Connection settings'
    TabOrder = 0
    object LabelDispatcher: TLabel
      Left = 12
      Top = 19
      Width = 58
      Height = 13
      Caption = 'Dispatcher:'
    end
    object LabelService: TLabel
      Left = 12
      Top = 67
      Width = 38
      Height = 13
      Caption = 'Service:'
    end
    object EditDispatcher: TEdit
      Left = 12
      Top = 36
      Width = 169
      Height = 21
      TabOrder = 0
    end
    object EditService: TEdit
      Left = 12
      Top = 84
      Width = 169
      Height = 21
      TabOrder = 1
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 129
    Width = 207
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonConnect: TButton
      Left = 44
      Top = 3
      Width = 75
      Height = 25
      Caption = 'Connect'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 124
      Top = 3
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
