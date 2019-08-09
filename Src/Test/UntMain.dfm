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
    object btn1: TBitBtn
      Left = 1016
      Top = 6
      Width = 85
      Height = 25
      Caption = 'Standard start '
      TabOrder = 0
      OnClick = btn1Click
    end
    object btn2: TBitBtn
      Left = 840
      Top = 6
      Width = 155
      Height = 25
      Caption = 'Opaque predicates test'
      TabOrder = 1
      OnClick = btn2Click
    end
    object btn3: TBitBtn
      Left = 664
      Top = 6
      Width = 155
      Height = 25
      Caption = 'code_coverage test'
      TabOrder = 2
      OnClick = btn3Click
    end
  end
end
