object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Dynamic Binary Analysis (DBA)  for Delphi'
  ClientHeight = 372
  ClientWidth = 1109
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 1109
    Height = 334
    Align = alClient
    TabOrder = 0
    object mmoLog: TMemo
      Left = 1
      Top = 1
      Width = 1107
      Height = 332
      Align = alClient
      Lines.Strings = (
        'mmoLog')
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pnl2: TPanel
    Left = 0
    Top = 334
    Width = 1109
    Height = 38
    Align = alBottom
    TabOrder = 1
    object btnStandard_test: TBitBtn
      Left = 1022
      Top = 6
      Width = 86
      Height = 25
      Caption = 'Standard start '
      TabOrder = 0
      OnClick = btnStandard_testClick
    end
    object btnOp_Pre: TBitBtn
      Left = 903
      Top = 6
      Width = 113
      Height = 25
      Caption = 'Opaque predicates test'
      TabOrder = 1
      OnClick = btnOp_PreClick
    end
    object btnCoverage: TBitBtn
      Left = 427
      Top = 6
      Width = 113
      Height = 25
      Caption = 'code_coverage test'
      TabOrder = 2
      OnClick = btnCoverageClick
    end
    object btnTaint: TBitBtn
      Left = 784
      Top = 5
      Width = 113
      Height = 25
      Caption = 'forward_tainting test'
      TabOrder = 3
      OnClick = btnTaintClick
    end
    object btnslicing: TBitBtn
      Left = 665
      Top = 5
      Width = 113
      Height = 25
      Caption = 'backward slicing test'
      TabOrder = 4
      OnClick = btnslicingClick
    end
    object btnIr: TBitBtn
      Left = 546
      Top = 6
      Width = 113
      Height = 25
      Caption = 'Ir test'
      TabOrder = 5
      OnClick = btnIrClick
    end
    object btnsimply: TBitBtn
      Left = 308
      Top = 6
      Width = 113
      Height = 25
      Caption = 'Simplification test'
      TabOrder = 6
      OnClick = btnsimplyClick
    end
    object btnCallback: TBitBtn
      Left = 189
      Top = 6
      Width = 113
      Height = 25
      Caption = 'Callback test'
      TabOrder = 7
      OnClick = btnCallbackClick
    end
  end
end
