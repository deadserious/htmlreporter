object frmConnections: TfrmConnections
  Left = 1478
  Top = 140
  Caption = 'Connections'
  ClientHeight = 284
  ClientWidth = 408
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 108
    Height = 13
    Caption = 'Database Connections'
  end
  object Label2: TLabel
    Left = 220
    Top = 8
    Width = 88
    Height = 13
    Caption = 'Connection Name:'
  end
  object Label3: TLabel
    Left = 220
    Top = 54
    Width = 89
    Height = 13
    Caption = 'Connection String:'
  end
  object lbConnections: TListBox
    Left = 12
    Top = 27
    Width = 173
    Height = 206
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbConnectionsClick
  end
  object edtName: TEdit
    Left = 220
    Top = 27
    Width = 137
    Height = 21
    TabOrder = 1
    OnChange = edtNameChange
  end
  object txtString: TMemo
    Left = 220
    Top = 73
    Width = 177
    Height = 160
    TabOrder = 2
    OnChange = edtNameChange
  end
  object btnOK: TButton
    Left = 110
    Top = 247
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 220
    Top = 247
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object btnAdd: TButton
    Left = 191
    Top = 27
    Width = 24
    Height = 25
    Caption = '+'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = btnAddClick
  end
  object btnReplace: TButton
    Left = 191
    Top = 58
    Width = 24
    Height = 25
    Caption = '<'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = btnReplaceClick
  end
  object btnDelete: TButton
    Left = 191
    Top = 89
    Width = 24
    Height = 25
    Caption = '-'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    OnClick = btnDeleteClick
  end
  object btnBuild: TButton
    Left = 363
    Top = 25
    Width = 37
    Height = 25
    Caption = 'Build'
    TabOrder = 8
    OnClick = btnBuildClick
  end
end
