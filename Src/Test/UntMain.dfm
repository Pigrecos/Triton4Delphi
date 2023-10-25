object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Dynamic Binary Analysis (DBA)  for Delphi'
  ClientHeight = 368
  ClientWidth = 1109
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 13
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 1109
    Height = 330
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 1105
    ExplicitHeight = 333
    object mmoLog: TMemo
      Left = 1
      Top = 1
      Width = 1107
      Height = 328
      Align = alClient
      Lines.Strings = (
        'mmoLog')
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitWidth = 1103
      ExplicitHeight = 331
    end
  end
  object pnl2: TPanel
    Left = 0
    Top = 330
    Width = 1109
    Height = 38
    Align = alBottom
    TabOrder = 1
    ExplicitLeft = 1
    ExplicitTop = 335
    object btnStandard_test: TBitBtn
      Left = 946
      Top = 6
      Width = 86
      Height = 25
      Caption = 'Standard'
      TabOrder = 0
      OnClick = btnStandard_testClick
    end
    object btnOp_Pre: TBitBtn
      Left = 736
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Op. predicates'
      TabOrder = 1
      OnClick = btnOp_PreClick
    end
    object btnCoverage: TBitBtn
      Left = 421
      Top = 6
      Width = 99
      Height = 25
      Caption = 'code_coverage'
      TabOrder = 2
      OnClick = btnCoverageClick
    end
    object btnTaint: TBitBtn
      Left = 841
      Top = 6
      Width = 99
      Height = 25
      Caption = 'forward_tainting'
      TabOrder = 3
      OnClick = btnTaintClick
    end
    object btnslicing: TBitBtn
      Left = 631
      Top = 6
      Width = 99
      Height = 25
      Caption = 'backward slicing'
      TabOrder = 4
      OnClick = btnslicingClick
    end
    object btnIr: TBitBtn
      Left = 526
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Ir test'
      TabOrder = 5
      OnClick = btnIrClick
    end
    object btnsimply: TBitBtn
      Left = 316
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Simplification'
      TabOrder = 6
      OnClick = btnsimplyClick
    end
    object btnCallback: TBitBtn
      Left = 211
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Callback'
      TabOrder = 7
      OnClick = btnCallbackClick
    end
    object btn1: TBitBtn
      Left = 106
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Vmp'
      TabOrder = 8
      OnClick = btn1Click
    end
    object btn2: TBitBtn
      Left = 1
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Basic Block'
      TabOrder = 9
      OnClick = btn2Click
    end
  end
end
