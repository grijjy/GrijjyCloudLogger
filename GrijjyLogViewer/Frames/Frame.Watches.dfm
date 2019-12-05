object FrameWatches: TFrameWatches
  Left = 0
  Top = 0
  Width = 756
  Height = 420
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object ListViewWatches: TListView
    Left = 0
    Top = 26
    Width = 756
    Height = 394
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 200
      end
      item
        Alignment = taRightJustify
        Caption = 'Value'
        Width = 100
      end>
    ColumnClick = False
    DoubleBuffered = True
    OwnerData = True
    OwnerDraw = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = ListViewWatchesCustomDrawItem
  end
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 756
    Height = 26
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '  Watches'
    Color = 10526880
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    object CheckBoxAutoRequest: TCheckBox
      AlignWithMargins = True
      Left = 660
      Top = 0
      Width = 96
      Height = 26
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      Caption = 'Auto-update'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = CheckBoxAutoRequestClick
    end
    object ButtonRequest: TButton
      Left = 576
      Top = 0
      Width = 81
      Height = 26
      Align = alRight
      Caption = 'Update'
      DisabledImageIndex = 9
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ImageIndex = 7
      ParentFont = False
      TabOrder = 1
      OnClick = ButtonRequestClick
    end
  end
  object TimerUpdate: TTimer
    Enabled = False
    OnTimer = TimerUpdateTimer
    Left = 32
    Top = 80
  end
end
