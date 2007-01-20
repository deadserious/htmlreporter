object frmGlobals: TfrmGlobals
  Left = 294
  Top = 83
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Global Replacers'
  ClientHeight = 125
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object lvGlobals: TListView
    Left = 0
    Top = 0
    Width = 312
    Height = 125
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
      end
      item
        Caption = 'Value'
      end>
    FlatScrollBars = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvGlobalsDblClick
  end
end
