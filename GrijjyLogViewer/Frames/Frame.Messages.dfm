object FrameMessages: TFrameMessages
  Left = 0
  Top = 0
  Width = 637
  Height = 414
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object SplitterMessages: TSplitter
    Left = 321
    Top = 26
    Width = 4
    Height = 388
    Align = alRight
    Color = 14737632
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitLeft = 485
    ExplicitTop = -131
    ExplicitHeight = 545
  end
  object PanelInspector: TPanel
    Left = 325
    Top = 26
    Width = 312
    Height = 388
    Align = alRight
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Chr('
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object MemoInspector: TMemo
      Left = 0
      Top = 289
      Width = 312
      Height = 99
      Align = alClient
      BevelKind = bkTile
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object PanelObjectInspector: TPanel
      Left = 0
      Top = 0
      Width = 312
      Height = 289
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 1
      Visible = False
      object HeaderControlInspector: THeaderControl
        Left = 2
        Top = 2
        Width = 308
        Height = 24
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Sections = <
          item
            AllowClick = False
            ImageIndex = -1
            Text = 'Name'
            Width = 120
          end
          item
            AllowClick = False
            AutoSize = True
            ImageIndex = -1
            Text = 'Value'
            Width = 188
          end>
        Style = hsFlat
        OnSectionResize = HeaderControlInspectorSectionResize
        ParentFont = False
      end
      object TreeViewInspector: TTreeView
        Left = 2
        Top = 26
        Width = 308
        Height = 261
        Align = alClient
        BevelEdges = [beLeft, beRight, beBottom]
        BorderStyle = bsNone
        DoubleBuffered = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Indent = 16
        ParentDoubleBuffered = False
        ParentFont = False
        ReadOnly = True
        RowSelect = True
        ShowLines = False
        TabOrder = 1
        OnAdvancedCustomDrawItem = TreeViewInspectorAdvancedCustomDrawItem
        OnDeletion = TreeViewInspectorDeletion
      end
    end
  end
  object ListViewMessages: TListView
    Left = 0
    Top = 26
    Width = 321
    Height = 388
    Align = alClient
    BevelKind = bkTile
    BorderStyle = bsNone
    Columns = <
      item
        AutoSize = True
        Caption = 'Message'
      end
      item
        Alignment = taRightJustify
        Caption = 'Time Stamp'
        Width = 75
      end>
    ColumnClick = False
    DoubleBuffered = True
    MultiSelect = True
    OwnerData = True
    OwnerDraw = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    TabOrder = 1
    ViewStyle = vsReport
    OnCustomDrawItem = ListViewMessagesCustomDrawItem
    OnSelectItem = ListViewMessagesSelectItem
  end
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 637
    Height = 26
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '        Messages'
    Color = 10526880
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 2
    object SpeedButtonScrollLock: TSpeedButton
      Left = 0
      Top = 0
      Width = 26
      Height = 26
      Hint = 'Scroll Lock'
      Align = alLeft
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
        FFFFFF000000000000000000000000000000000000000000000000000000FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000000000000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        0000000000000000000000000000000000000000000000000000000000000000
        00FFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000000000
        0000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF000000000000000000000000000000000000000000000000000000FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF000000000000000000000000000000000000000000000000000000FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF000000000000000000000000000000000000000000000000000000FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
        0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButtonScrollLockClick
    end
    object ButtonSave: TButton
      Left = 561
      Top = 0
      Width = 76
      Height = 26
      Align = alRight
      Caption = 'Save'
      DisabledImageIndex = 8
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ImageIndex = 0
      ParentFont = False
      TabOrder = 0
      OnClick = ButtonSaveClick
    end
  end
  object ImageListMessages: TImageList
    ColorDepth = cd32Bit
    Height = 12
    Width = 12
    Left = 46
    Top = 37
    Bitmap = {
      494C01010C00A80064000C000C00FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000300000003000000001002000000000000024
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002020
      2021706E6AC54C4A44F9706E6AC6202020210000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001818181971706DBD4845
      3EFE505258FF4A4C4FFF4A4C54FF48453EFE71706DBD18181819000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000737270A548453EFE4F5054FF636D
      8DFF68749AFF4C4E54FF52608DFF505C82FF494B50FF48453EFE737270AA0000
      000000000000808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF000000000000000000000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF000000000000
      00000000000000000000000000006161618D0101010200000000000000000000
      00000000000000000000000000000000000046433BFF626C8BFF68749AFF6874
      9AFF68749AFF4C4E54FF52608DFF52608DFF52608DFF505B80FF46433BFF0000
      000000000000808080FFF1F1F1FFF0F0F0FFECECECFFEAEAEAFFE9E9E9FFE7E7
      E7FFE4E4E4FF808080FF000000000000000000000000808080FFF1F1F1FFEEEE
      EEFFECECECFFEAEAEAFFE8E8E8FFE5E5E5FFE4E4E4FF808080FF000000000000
      0000000000000000000000000000000000FF606060A505050506000000000000
      00000000000000000000000000000000000046433BFF68749AFF68749AFF6874
      9AFF68749AFF4C4E54FF52608DFF52608DFF52608DFF52608DFF46433BFF0000
      000000000000808080FFF3F3F3FFF1F1F1FFEFEFEFFF000000FFEAEAEAFFE8E8
      E8FFE7E7E7FF808080FF000000000000000000000000808080FFF2F2F2FFF0F0
      F0FFEEEEEEFFEEEEEEFFEBEBEBFFE7E7E7FFE6E6E6FF808080FF000000000000
      0000000000000000000000000000000000FF000000FF565656BA0C0C0C0D0000
      00000000000000000000000000000000000046433BFF68749AFF68749AFF6874
      9AFF68749AFF4C4E54FF52608DFF52608DFF52608DFF52608DFF46433BFF0000
      000000000000808080FFF5F5F5FFF4F4F4FFF1F1F1FF000000FFECECECFFEBEB
      EBFFE8E8E8FF808080FF000000000000000000000000808080FFF5F5F5FFF3F3
      F3FFF0F0F0FFEDEDEDFFECECECFFEBEBEBFFE8E8E8FF808080FF000000000000
      0000000000000000000000000000000000FF000000FF000000FF484848CD1616
      16170000000000000000000000000000000046433BFF68749AFF68749AFF6671
      94FF565A67FF504F4FFF4C5161FF515E88FF52608DFF52608DFF46433BFF0000
      000000000000808080FFF6F6F6FF000000FF000000FF000000FF000000FF0000
      00FFEAEAEAFF808080FF000000000000000000000000808080FFF7F7F7FF0000
      00FF000000FF000000FF000000FF000000FFEAEAEAFF808080FF000000000000
      0000000000000000000000000000000000FF000000FF000000FF000000FF5656
      56B90000000000000000000000000000000046433BFF667296FF575C6BFF5657
      59FF72798BFF818AA1FF72798BFF555657FF4C5264FF515F89FF46433BFF0000
      000000000000808080FFF9F9F9FFF6F6F6FFF6F6F6FF000000FFF1F1F1FFEEEE
      EEFFEDEDEDFF808080FF000000000000000000000000808080FFF9F9F9FFF6F6
      F6FFF5F5F5FFF2F2F2FFF0F0F0FFEFEFEFFFEDEDEDFF808080FF000000000000
      0000000000000000000000000000000000FF000000FF000000FF484848CD1616
      16170000000000000000000000000000000047443EFF535252FF707687FF818A
      A1FF818AA1FF818AA1FF818AA1FF818AA1FF7B859AFF56565BFF46443DFF0000
      000000000000808080FFFBFBFBFFFAFAFAFFF7F7F7FF000000FFF2F2F2FFF0F0
      F0FFEFEFEFFF808080FF000000000000000000000000808080FFFBFBFBFFFAFA
      FAFFF7F7F7FFF5F5F5FFF2F2F2FFF0F0F0FFEFEFEFFF808080FF000000000000
      0000000000000000000000000000000000FF000000FF555555BB0C0C0C0D0000
      000000000000000000000000000000000000737270A449463EFE555657FF7781
      94FF818AA1FF818AA1FF818AA1FF778194FF58585AFF49463EFE737270A30000
      000000000000808080FFFCFCFCFFFBFBFBFFF9F9F9FFF8F8F8FFF5F5F5FFF3F3
      F3FFF0F0F0FF808080FF000000000000000000000000808080FFFCFCFCFFFBFB
      FBFFF8F8F8FFF6F6F6FFF4F4F4FFF3F3F3FFF1F1F1FF808080FF000000000000
      0000000000000000000000000000000000FF606060A505050506000000000000
      000000000000000000000000000000000000000000001818181972706DBE4946
      3FFE5C5D61FF73798CFF5A5B5EFF49463FFE72706DBE18181819000000000000
      000000000000808080FF808080FF808080FF808080FF808080FF808080FF8080
      80FF808080FF808080FF000000000000000000000000808080FF808080FF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FF000000000000
      00000000000000000000000000006161618D0101010200000000000000000000
      0000000000000000000000000000000000000000000000000000000000002121
      21226F6E6AC74A4740FC6F6E6AC7202020210000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000716D65DF43434347000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000716D65DF4343434700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001B271F8F575D4FFC5A554BFF5A55
      4BFF5A554BFF575D4FFC1B271F8E000000000000000000000000000000000000
      0000000000000000000000000000000000005B564CFF67625AF6414141450000
      0000000000000000000000000000000000000000000000000000000000000000
      00005B564CFF686255F641414145000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000575D4FFC59A27BFF59BD8DFF59BD
      8DFF59BD8DFF59A27BFF575D4FFC000000000000000000000000000000000000
      0000000000000000000000000000000000005C574DFFC5C5C2FF666257F64141
      4145000000000000000000000000000000000000000000000000000000000000
      00005C574DFFD1B26BFF666153F6414141450000000000000000000000000000
      00006A7B6AEA5E594FFF5E594FFF5E594FFF5E594FFF5E594FFF5E594FFF5E59
      4FFF5E594FFF5E594FFF667163F3000000005A554BFF59BD8DFF59BD8DFF59BD
      8DFF59BD8DFF59BD8DFF5A554BFF00000000131310770505043F000000000000
      000042424246787670D25F5A50FF5E594EFF6E6960FFEAEAEAFFC3C1BEFF5F5A
      51FF585248FF565146FF74716BCF3F3F3F4342424246787670D25F5A50FF5E59
      4EFF706652FFF9D176FFCEAF69FF615A4BFF585248FF565146FF74716BCF3F3F
      3F435E594FFF53B988FF5E594FFF53BA89FF5E594FFF53BA89FF5E594FFF53BA
      89FF5E594FFF53BA89FF5E594FFF030303045A554BFF59BD8DFF59BD8DFF59BD
      8DFF59BD8DFF59BD8DFF5A554BFF000000000505043F4C493FEA0505043F0000
      0000797771D3C3C2BEFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFBEBDBAFF74716BCF797771D3CDB06FFFF9D27AFFF9D2
      78FFF9D177FFF9D177FFF9D175FFF9D074FFF9D073FFF9D072FFCAAB65FF7370
      6BCF5E594FFF53BA89FF5B705DFF53BA89FF5B705DFF53BA89FF5B705DFF53BA
      89FF5B705DFF53BA89FF5E594FFF000000005A554BFF59BD8DFF59BD8DFF59BD
      8DFF59BD8DFF59BD8DFF59BD8DFF00000000000000000505043F4C493FEA0505
      043F625D53FFEBEBEBFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FF554F45FF635E54FFF9D482FF847558FF5C57
      4DFF5C574DFF5C574DFF847557FFF9D075FFF9D073FFF9D073FFF9CF72FF554F
      45FF5E594FFF53BA89FF53BA89FF53BA89FF53BA89FF53BA89FF53BA89FF53BA
      89FF53BA89FF53BA89FF5B735EFF616B5DF85A554BFF59BD8DFF5B6F5BFF5B56
      4BFF5B564BFF5B564BFF5B564BFF5B564BFF5B564BFF5B564BFF5B564BFF5753
      49FA635E54FFEDEDEDFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FF555046FF635E54FFFAD88BFFF9D27AFFF9D2
      79FFF9D178FFF9D177FFF9D176FFF9D175FFF9D074FFF9D073FFF9D072FF5550
      46FF5E594FFF53BA89FF83776EFF83776EFF53BA89FF83776EFF83776EFF53BA
      89FF83776EFF83776EFF53BA89FF5E594FFF5A554BFF59BD8DFF59BD8DFF59BD
      8DFF59BD8DFF59BD8DFF59BD8DFF00000000000000000505043F4C493FEA0505
      043F645F55FFEDEDEDFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
      EAFFE9E9E9FFE9E9E9FFE9E9E9FF565146FF645F55FFFAD88BFF847558FF5C57
      4DFF5C574DFF5C574DFF5C574DFF5C574DFF5C574DFF847456FFF9D073FF5651
      46FF5E594FFF53BA89FF83776EFF83776EFF53BA89FF83776EFF83776EFF53BA
      89FF83776EFF83776EFF53BA89FF5E594FFF5A554BFF59BD8DFF59BD8DFF59BD
      8DFF59BD8DFF59BD8DFF5A554BFF000000000505043F4C493FEA0505043F0000
      0000645F56FFEDEDEDFFEBEBEBFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
      EAFFEAEAEAFFE9E9E9FFE9E9E9FF575247FF645F56FFFAD88BFFF9D37BFFF9D2
      7AFFF9D279FFF9D278FFF9D177FFF9D176FFF9D175FFF9D074FFF9D073FF5651
      46FF5E594FFF5BB188FF5BB188FF5BB188FF5BB188FF5BB188FF5BB188FF5BB1
      88FF5BB188FF5BB188FF5BB188FF5E594FFF5A554BFF59BD8DFF59BD8DFF59BD
      8DFF59BD8DFF59BD8DFF5A554BFF00000000131310770505043F000000000000
      0000656057FFEDEDEDFFEBEBEBFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
      EAFFEAEAEAFFE9E9E9FFE9E9E9FF585248FF645F56FFFAD88DFF847559FF5C57
      4DFF5C574DFF5C574DFF5C574DFF847557FFF9D175FFF9D075FFF9D073FF5752
      47FF6A7B6AEA5E594FFF5E594FFF5E594FFF5E594FFF5E594FFF5E594FFF5E59
      4FFF5E594FFF5E594FFF5E594FFF6A7B6AEA575D4FFC59A27BFF59BD8DFF59BD
      8DFF59BD8DFF59A27BFF575D4FFC000000000000000000000000000000000000
      00007C7873D3CBCBC8FFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFECECECFFECEC
      ECFFECECECFFECECECFFC2C1BEFF75726CD07C7873D3D5BC85FFFAD88CFFFAD7
      8AFFFAD789FFFAD788FFFAD787FFFAD687FFFAD686FFFAD684FFCDB06EFF7573
      6CD0000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001B271F8F575D4FFC5A554BFF5A55
      4BFF5A554BFF575D4FFC1B271F8F000000000000000000000000000000000000
      0000454545497A7872D3645F55FF625D53FF615C53FF605B51FF5E594FFF5E59
      4EFF5B564CFF5B564BFF76736DD140404044454545497A7872D3645F55FF625D
      53FF615C53FF605B51FF5E594FFF5E594EFF5C574DFF5B564BFF76736DD14040
      4044000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000002020203746E
      6382907D53CD86682BF186682BF1907D53CD746E638202020203000000000000
      000000000000000000000101010265707482598593CD33738AF133738AF15985
      93CD657074820101010200000000000000000000000000000000020202036464
      7182545486CD2D2D75F12D2D75F1545486CD6464718202020203000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000252525268F7A4BD5986F
      17FFD09A21FFE9AD25FFE9AD25FFD09A21FF986F17FF8F7A4BD5232323240000
      00000000000024242425538393D524809EFF33AFD8FF39C4F2FF39C4F2FF33AF
      D8FF24809EFF538393D5222222230000000000000000252525264F4F85D51B1B
      83FF2626B3FF2B2BC8FF2B2BC8FF2626B3FF1B1B83FF4F4F85D5232323240000
      0000000000000000000000000000000000005E615F6B626D5FF459544AFF5954
      4AFF59544AFF59544AFF626D5FF45E615F6B020202038F7A4BD6B6871CFFF5B5
      27FFF6B627FFFCEAC1FFFCEAC1FFF6B627FFF5B527FFB6871CFF8F7A4BD50202
      020301010102538292D52C99BDFF3CCEFEFF3CCFFFFFC7F1FFFFC7F1FFFF3CCF
      FFFF3CCEFEFF2C99BDFF538392D401010102020202034D4D84D621219CFF3333
      D3FF2E2ED3FF2D2DD3FF2D2DD3FF2E2ED3FF3333D3FF21219CFF4F4F85D50202
      020300000000000000000000000000000000626D5EF4589370FF58BC8CFF58BD
      8DFF58BD8DFF58BC8CFF589370FF626D5FF4746F6383986F17FFF6B627FFF6B6
      27FFF6B627FFFFFFFFFFFFFFFFFFF6B627FFF6B627FFF5B527FF986F17FF746E
      63826570748224809EFF3CCFFFFF3CCFFFFF3CCFFFFFC8F1FFFFC8F1FFFF3CCF
      FFFF3CCFFFFF3CCEFEFF24809EFF65707482646472831B1B83FF3333D4FFEBEB
      FBFFD7D7F7FF3E3ED7FF3E3ED7FFD7D7F7FFEBEBFBFF3333D3FF1B1B83FF6464
      71820000000000000000000000000000000059544AFF58BC8CFF598D6DFF58A4
      7BFF58BD8DFF58BD8DFF58BC8CFF59544AFF907E52CED19B21FFF6B627FFF6B6
      27FFF6B627FFFFFFFFFFFFFFFFFFF6B627FFF6B627FFF6B627FFD09A21FF907D
      53CD588593CE33B0D9FF3CCFFFFF3CCFFFFF3CCFFFFF3CCFFFFF3CCFFFFF3CCF
      FFFF3CCFFFFF3CCFFFFF33AFD8FF598593CD545486CE2626B4FF2E2ED3FFD7D7
      F7FFFFFFFFFFDFDFF9FFDFDFF8FFFFFFFFFFD7D7F7FF2E2ED3FF2626B3FF5454
      86CD0000000000000000000000000000000059544AFF58BD8DFF58A47BFF5A5D
      4FFF58A47BFF58BD8DFF58BD8DFF59544AFF866729F2EAAD25FFF6B627FFF6B6
      27FFF6B627FFFFFFFFFFFFFFFFFFF6B627FFF6B627FFF6B627FFE9AD25FF8668
      2BF133738AF139C5F3FF3CCFFFFF3CCFFFFF3CCFFFFFC7F1FFFFC7F1FFFF3CCF
      FFFF3CCFFFFF3CCFFFFF39C4F2FF33738AF12C2C74F22B2BC9FF2D2DD3FF3E3E
      D7FFDFDFF9FFFFFFFFFFFFFFFFFFDFDFF8FF3E3ED7FF2D2DD3FF2B2BC8FF2D2D
      75F10000000000000000000000000000000058BD8DFF58BD8DFF58BD8DFF58A4
      7BFF5A5D4FFF58A47BFF58BD8DFF59544AFF866729F2EAAD25FFF6B627FFF6B6
      27FFF6B627FFFCEAC1FFFCEAC1FFF6B627FFF6B627FFF6B627FFE9AD25FF8668
      2BF133738AF139C5F3FF3CCFFFFF3CCFFFFF3CCFFFFFFFFFFFFFFFFFFFFF3CCF
      FFFF3CCFFFFF3CCFFFFF39C4F2FF33738AF12C2C74F22B2BC9FF2D2DD3FF3E3E
      D7FFDFDFF8FFFFFFFFFFFFFFFFFFDFDFF9FF3E3ED7FF2D2DD3FF2B2BC8FF2D2D
      75F17A7873C05A554AFF5A554AFF5A554AFF5A554AFF5A554AFF5A554AFF5A55
      4AFF5A554AFF5A574BFF58BD8DFF59544AFF907E52CED19B21FFF6B627FFF6B6
      27FFF6B627FFF6B627FFF6B627FFF6B627FFF6B627FFF6B627FFD09A21FF907D
      53CD588593CE33B0D9FF3CCFFFFF3CCFFFFF3CCFFFFFFFFFFFFFFFFFFFFF3CCF
      FFFF3CCFFFFF3CCFFFFF33AFD8FF598593CD545486CE2626B4FF2E2ED3FFD7D7
      F7FFFFFFFFFFDFDFF8FFDFDFF9FFFFFFFFFFD7D7F7FF2E2ED3FF2626B3FF5454
      86CD0000000000000000000000000000000058BD8DFF58BD8DFF58BD8DFF58A4
      7BFF5A5D4FFF58A47BFF58BD8DFF59544AFF746F6383996F18FFF6B627FFF6B6
      27FFF6B627FFFCEAC1FFFCEAC1FFF6B627FFF6B627FFF5B527FF986F17FF746E
      63826570748225819FFF3CCFFFFF3CCFFFFF3CCFFFFFFFFFFFFFFFFFFFFF3CCF
      FFFF3CCFFFFF3CCEFEFF24809EFF65707482646472831B1B84FF3333D4FFEBEB
      FBFFD7D7F7FF3E3ED7FF3E3ED7FFD7D7F7FFEBEBFBFF3333D3FF1B1B83FF6464
      71820000000000000000000000000000000059544AFF58BD8DFF58A47BFF5A5D
      4FFF58A47BFF58BD8DFF58BD8DFF59544AFF020202038F7A4BD6B6871CFFF6B6
      27FFF6B627FFFCEAC2FFFCEAC2FFF6B627FFF5B527FFB6871CFF8F7A4BD50202
      020301010102538292D52C99BDFF3CCFFFFF3CCFFFFFC7F1FFFFC7F1FFFF3CCF
      FFFF3CCEFEFF2C99BDFF538393D501010102020202034D4D84D621219CFF3333
      D4FF2E2ED3FF2D2DD3FF2D2DD3FF2E2ED3FF3333D3FF21219CFF4F4F85D50202
      02030000000000000000000000000000000059544AFF58BC8CFF598D6DFF58A4
      7BFF58BD8DFF58BD8DFF58BC8CFF59544AFF00000000252525268F7A4BD6986F
      17FFD19B21FFEAAD25FFEAAD25FFD19B21FF986F17FF8F7A4BD5252525260000
      00000000000024242425538292D524809EFF33B0D9FF39C5F3FF39C5F3FF33B0
      D9FF24809EFF538393D5242424250000000000000000252525264D4D84D61B1B
      83FF2626B4FF2B2BC9FF2B2BC9FF2626B4FF1B1B83FF4F4F85D5252525260000
      000000000000000000000000000000000000626D5EF4589370FF58BC8CFF58BD
      8DFF58BD8DFF58BC8CFF589370FF626D5EF4000000000000000002020203746F
      6383907E52CE866729F2866729F2907E52CE746F638302020203000000000000
      000000000000000000000101010265707482588593CE33738AF133738AF15885
      93CE657074820101010200000000000000000000000000000000020202036464
      7283545486CE2C2C74F22C2C74F2545486CE6464728302020203000000000000
      0000000000000000000000000000000000005F63616D626D5EF459544AFF5954
      4AFF59544AFF59544AFF626D5EF45F63616D424D3E000000000000003E000000
      2800000030000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000E0F000000FFF0000803000000FFF0000
      001000000E7F0000001000000E3F0000001000000E1F0000001000000E0F0000
      001000000E0F0000001000000E0F0000001000000E1F0000001000000E3F0000
      803000000E7F0000E0F000000FFF0000000F3FF3FFFF0000000F1FF1FFFF0000
      000F0FF0F0010000000000000000000000000000000100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000FFF0000000000000FFF0000C03C03C03FFF0000801801801F000000
      000000000F000000000000000F000000000000000F000000000000000F000000
      0000000000000000000000000F000000000000000F000000000000000F000000
      801801801F000000C03C03C03F00000000000000000000000000000000000000
      000000000000}
  end
  object TimerUpdate: TTimer
    Interval = 100
    OnTimer = TimerUpdateTimer
    Left = 156
    Top = 36
  end
  object SaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save Message'
    Left = 460
    Top = 72
  end
end
