object frmMain: TfrmMain
  Left = 1329
  Top = 23
  Caption = 'HTML Report Designer'
  ClientHeight = 551
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 632
    Height = 532
    ActivePage = tsSource
    Align = alClient
    MultiLine = True
    TabOrder = 0
    OnChange = pcMainChange
    ExplicitHeight = 451
    object tsSource: TTabSheet
      Caption = 'Source'
      ImageIndex = 1
      object txtSource: TSynEdit
        Left = 0
        Top = 0
        Width = 624
        Height = 504
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.AutoSize = True
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.ShowLineNumbers = True
        Gutter.Gradient = True
        Highlighter = SynHTMLSyn1
        MaxUndo = 10240
        Options = [eoAltSetsColumnMode, eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        SearchEngine = SearchEngine
        WantTabs = True
        OnChange = txtSourceChange
        OnReplaceText = txtSourceReplaceText
      end
    end
    object tsPreview: TTabSheet
      Caption = 'Preview'
      ImageIndex = 3
      object htmlPreview: THTMLViewer
        Left = 0
        Top = 0
        Width = 624
        Height = 504
        OnImageRequest = htmlPreviewImageRequest
        TabOrder = 0
        Align = alClient
        PopupMenu = pmPreview
        DefBackground = clWhite
        BorderStyle = htSingle
        HistoryMaxCount = 0
        DefFontName = 'Tahoma'
        DefPreFontName = 'Courier New'
        NoSelect = False
        CharSet = DEFAULT_CHARSET
        PrintMarginLeft = 2.000000000000000000
        PrintMarginRight = 2.000000000000000000
        PrintMarginTop = 2.000000000000000000
        PrintMarginBottom = 2.000000000000000000
        PrintScale = 1.000000000000000000
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 532
    Width = 632
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Disconnected'
  end
  object XPManifest1: TXPManifest
    Left = 348
    Top = 236
  end
  object SynHTMLSyn1: TSynHTMLSyn
    Left = 272
    Top = 232
  end
  object MainMenu1: TMainMenu
    Left = 304
    Top = 224
    object miFile: TMenuItem
      Caption = '&File'
      object miFileNew: TMenuItem
        Caption = '&New'
      end
      object miFileOpen: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = miFileOpenClick
      end
      object miFileReopen: TMenuItem
        Caption = 'Reopen'
        Enabled = False
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miFileSave: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = miFileSaveClick
      end
      object miFileSaveAs: TMenuItem
        Caption = '&Save As...'
        ShortCut = 49235
        OnClick = miFileSaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miFileExit: TMenuItem
        Caption = 'E&xit'
        ShortCut = 32883
        OnClick = miFileExitClick
      end
    end
    object miEdit: TMenuItem
      Caption = '&Edit'
      object miEditSearch: TMenuItem
        Caption = '&Search...'
        ShortCut = 16454
        OnClick = miEditSearchClick
      end
      object miEditReplace: TMenuItem
        Caption = 'Replace...'
        ShortCut = 16466
        OnClick = miEditReplaceClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object miEditFindPrevious: TMenuItem
        Caption = 'Find Previous'
        Enabled = False
        ShortCut = 8306
        OnClick = miEditFindPreviousClick
      end
      object miEditFindNext: TMenuItem
        Caption = 'Find Next'
        Enabled = False
        ShortCut = 114
        OnClick = miEditFindNextClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object miEditOptions: TMenuItem
        Caption = 'Editor Options...'
        ShortCut = 49232
        OnClick = miEditOptionsClick
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object miViewPreview: TMenuItem
        Caption = '&Source/Preview'
        ShortCut = 123
        OnClick = miViewPreviewClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miViewGlobal: TMenuItem
        Caption = 'Global Replacers'
        ShortCut = 16455
        OnClick = miViewGlobalClick
      end
    end
    object Connections1: TMenuItem
      Caption = '&Connections'
      object miConnectionsSelect: TMenuItem
        Caption = '&Select'
      end
      object miConnectionsDisconnect: TMenuItem
        Caption = '&Disconnect'
        OnClick = miConnectionsDisconnectClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miConnectionsConfigure: TMenuItem
        Caption = '&Configure...'
        ShortCut = 49219
        OnClick = miConnectionsConfigureClick
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      Enabled = False
      object Contents1: TMenuItem
        Caption = '&Contents'
      end
      object About1: TMenuItem
        Caption = '&About'
      end
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '.html'
    Filter = 'HTML Files (*.html)|*.html|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open File'
    Left = 134
    Top = 256
  end
  object dlgSave: TSaveDialog
    DefaultExt = '.html'
    Filter = 'HTML Files (*.html)|*.html|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save File As...'
    Left = 160
    Top = 256
  end
  object ADO: TADOConnection
    AfterDisconnect = ADOAfterDisconnect
    Left = 272
    Top = 264
  end
  object Reporter: THTMLReporter
    Replacers = <
      item
        TagName = '%PageBreak%'
        HTML.Strings = (
          
            '<div style="width:1px;height:1px;overflow:hidden;page-break-afte' +
            'r: always;"></div>')
        Timing = rtBeforeData
      end
      item
        TagName = '%AutoPrint%'
        HTML.Strings = (
          '<script lantuage="Javascript">window.print();</script>')
        Timing = rtBeforeData
      end>
    OnAcquireData = ReporterAcquireData
    OnReleaseData = ReporterReleaseData
    BlankGroupText = '&nbsp;'
    DataTag = 'data'
    FormatTag = 'format'
    FieldTag = '{}'
    ReduceLineBreaks = False
    Left = 308
    Top = 268
  end
  object tmrGlobals: TTimer
    Enabled = False
    OnTimer = tmrGlobalsTimer
    Left = 224
    Top = 288
  end
  object pmPreview: TPopupMenu
    Left = 400
    Top = 160
    object miPreviewSource: TMenuItem
      Caption = '&View Source'
      OnClick = miPreviewSourceClick
    end
    object miPreviewDefault: TMenuItem
      Caption = 'View in Default Browser'
      OnClick = miPreviewDefaultClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object miPreviewPrint: TMenuItem
      Caption = 'Print...'
      ShortCut = 16464
      OnClick = miPreviewPrintClick
    end
    object PrintSetup1: TMenuItem
      Caption = 'Print Setup...'
      ShortCut = 49232
      OnClick = PrintSetup1Click
    end
  end
  object dlgPrint: TPrintDialog
    Left = 276
    Top = 296
  end
  object dlgPrintSetup: TPrinterSetupDialog
    Left = 308
    Top = 296
  end
  object SearchEngine: TSynEditSearch
    Left = 144
    Top = 320
  end
  object dlgEditorOptions: TSynEditOptionsDialog
    UseExtendedStrings = False
    Left = 144
    Top = 348
  end
  object RegexSearchEngine: TSynEditRegexSearch
    Left = 176
    Top = 320
  end
  object SynMacroRecorder1: TSynMacroRecorder
    Editor = txtSource
    RecordShortCut = 24658
    PlaybackShortCut = 24656
    Left = 176
    Top = 348
  end
  object PaxScript: TArcHRPaxScript
    HTMLReporter = Reporter
    ScriptTag = 'datascript'
    OnScriptError = PaxScriptScriptError
    Left = 336
    Top = 264
  end
end
