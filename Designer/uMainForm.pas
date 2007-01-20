unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, XPMan, SynEditHighlighter, SynHighlighterHtml, HtmlUn2,
  SynEdit, Htmlview, Menus, DB, ADODB, ArcHTMLReporter, ExtCtrls, IdHTTP,
  Printers, JCLSysInfo, ShellAPI, SynCompletionProposal, SynEditPlugins,
  SynMacroRecorder, SynEditRegexSearch, SynEditOptionsDialog,
  SynEditTypes, SynEditMiscClasses, SynEditSearch, ArcHRScript,
  ArcHRScriptPaxScript, IMP_Classes, IMP_SysUtils, IMP_Variants,
  IMP_Contnrs, {IMP_Windows, }IMP_ShellAPI, IMP_RegStr, IMP_PsAPI, IMP_VarUtils,
  IMP_VarHlpr, IMP_Types, IMP_System, IMP_SysInit, IMP_SysConst,
  IMP_ZLib, IMP_ZLibConst, IMP_WideStrUtils, IMP_WideStrings, IMP_VarConv, IMP_StrUtils,
  IMP_StdConvs, IMP_RTLConsts, IMP_Registry, IMP_Math, IMP_IniFiles, IMP_DateUtils;

type
  THTPTGetter = class(TThread)
  private
    FURL: string;
    Output : TMemoryStream;
  protected
    procedure Execute; override;
  public
    constructor Create(URL : string);
    procedure UpdateGUI;
  end;

  TfrmMain = class(TForm)
    pcMain: TPageControl;
    tsSource: TTabSheet;
    tsPreview: TTabSheet;
    XPManifest1: TXPManifest;
    txtSource: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    htmlPreview: THTMLViewer;
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    N1: TMenuItem;
    miFileExit: TMenuItem;
    View1: TMenuItem;
    miViewPreview: TMenuItem;
    Connections1: TMenuItem;
    miConnectionsSelect: TMenuItem;
    miConnectionsConfigure: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    About1: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    miFileNew: TMenuItem;
    ADO: TADOConnection;
    StatusBar: TStatusBar;
    miConnectionsDisconnect: TMenuItem;
    N2: TMenuItem;
    Reporter: THTMLReporter;
    miFileReopen: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    miViewGlobal: TMenuItem;
    tmrGlobals: TTimer;
    pmPreview: TPopupMenu;
    miPreviewSource: TMenuItem;
    miPreviewPrint: TMenuItem;
    N5: TMenuItem;
    miPreviewDefault: TMenuItem;
    dlgPrint: TPrintDialog;
    PrintSetup1: TMenuItem;
    dlgPrintSetup: TPrinterSetupDialog;
    SearchEngine: TSynEditSearch;
    dlgEditorOptions: TSynEditOptionsDialog;
    RegexSearchEngine: TSynEditRegexSearch;
    SynMacroRecorder1: TSynMacroRecorder;
    miEdit: TMenuItem;
    miEditSearch: TMenuItem;
    miEditReplace: TMenuItem;
    N6: TMenuItem;
    miEditFindPrevious: TMenuItem;
    miEditFindNext: TMenuItem;
    N7: TMenuItem;
    miEditOptions: TMenuItem;
    PaxScript: TArcHRPaxScript;
    procedure PaxScriptScriptError(ASender: TObject; Error: string;
      Continue: Boolean);
    procedure miEditOptionsClick(Sender: TObject);
    procedure miEditReplaceClick(Sender: TObject);
    procedure miEditFindNextClick(Sender: TObject);
    procedure miEditFindPreviousClick(Sender: TObject);
    procedure txtSourceReplaceText(Sender: TObject; const ASearch,
      AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
    procedure miEditSearchClick(Sender: TObject);
    procedure AutoCompleteBeforeExecute(Sender: TObject);
    procedure PrintSetup1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure miPreviewSourceClick(Sender: TObject);
    procedure miPreviewDefaultClick(Sender: TObject);
    procedure miPreviewPrintClick(Sender: TObject);
    procedure htmlPreviewImageRequest(Sender: TObject; const SRC: string;
      var Stream: TMemoryStream);
    procedure tmrGlobalsTimer(Sender: TObject);
    procedure miViewGlobalClick(Sender: TObject);
    procedure ReporterReleaseData(ASender: TObject; Dataset: TDataSet);
    procedure ReporterAcquireData(ASender: TObject; const ID, Src: string;
      var Dataset: TDataSet);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure miConnectionsDisconnectClick(Sender: TObject);
    procedure ADOAfterDisconnect(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miConnectionsConfigureClick(Sender: TObject);
    procedure miViewPreviewClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure txtSourceChange(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
  private
    MRUList : TStringList;
    ConnectedTo : string;
    ConStrings : TStringList;
    PreviewSource : TStringList;
    FHasChanged: Boolean;
    FFilename: string;
    procedure DoSearchReplaceText(AReplace, ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);
    procedure SetHasChanged(const Value: boolean);
    procedure SetFilename(const Value: string);
    { Private declarations }
  public
    fSearchFromCaret: boolean;
    procedure UpdateCaption;
    procedure CheckSearchEnabledStates;
    property Filename : string read FFilename write SetFilename;
    property HasChanged : boolean read FHasChanged write SetHasChanged;
    procedure SaveFile;
    procedure OpenFile;
    procedure SaveFileAs;
    procedure RefreshPreview;
    procedure NewFromTemplate(ASender : TObject);
    procedure SelectConnection(ASender : TObject);
    procedure ReopenFile(ASender : TObject);
    procedure CheckForChanges;
    procedure RebuildMRU;
    procedure RebuildConnections;
    procedure AddMRU(aFilename : string);
  end;

var
  frmMain: TfrmMain;

var
  INIFilename : string;
  SettingsFilename : string;

  gbSearchBackwards: boolean;
  gbSearchCaseSensitive: boolean;
  gbSearchFromCaret: boolean;
  gbSearchSelectionOnly: boolean;
  gbSearchTextAtCaret: boolean;
  gbSearchWholeWords: boolean;
  gbSearchRegex: boolean;

  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

implementation

uses IniFiles, uConnections, uGlobal, uSearchTextForm, uReplaceTextForm,
  uConfirmReplaceForm;

{$R *.dfm}

procedure TfrmMain.ShowSearchReplaceDialog(AReplace: boolean);
var
  dlg: TfrmTextSearch;
begin
  Statusbar.SimpleText := '';
  if AReplace then
    dlg := TfrmTextReplace.Create(Self)
  else
    dlg := TfrmTextSearch.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromCursor := gbSearchFromCaret;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then begin
      // if something is selected search for that text
      if txtSource.SelAvail and (txtSource.BlockBegin.Line = txtSource.BlockEnd.Line)
      then
        SearchText := txtSource.SelText
      else
        SearchText := txtSource.GetWordAtRowCol(txtSource.CaretXY);
    end;
    SearchTextHistory := gsSearchTextHistory;
    if AReplace then with dlg as TfrmTextReplace do begin
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
    end;
    SearchWholeWords := gbSearchWholeWords;
    if ShowModal = mrOK then begin
      gbSearchBackwards := SearchBackwards;
      gbSearchCaseSensitive := SearchCaseSensitive;
      gbSearchFromCaret := SearchFromCursor;
      gbSearchSelectionOnly := SearchInSelectionOnly;
      gbSearchWholeWords := SearchWholeWords;
      gbSearchRegex := SearchRegularExpression;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TfrmTextReplace do begin
        gsReplaceText := ReplaceText;
        gsReplaceTextHistory := ReplaceTextHistory;
      end;
      fSearchFromCaret := gbSearchFromCaret;
      if gsSearchText <> '' then begin
        DoSearchReplaceText(AReplace, gbSearchBackwards);
        fSearchFromCaret := TRUE;
      end;
    end;
  finally
    dlg.Free;
  end;
  CheckSearchEnabledStates;
end;

procedure TfrmMain.DoSearchReplaceText(AReplace: boolean;
  ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  //Status.SimpleText := '';
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  if gbSearchRegex then
    txtSource.SearchEngine := RegexSearchEngine
  else
    txtSource.SearchEngine := SearchEngine;
  if txtSource.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    //Statusbar.SimpleText := STextNotFound;
    if ssoBackwards in Options then
      txtSource.BlockEnd := txtSource.BlockBegin
    else
      txtSource.BlockBegin := txtSource.BlockEnd;
    txtSource.CaretXY := txtSource.BlockBegin;
  end;

  if frmConfirmReplace <> nil then
    frmConfirmReplace.Free;
end;

procedure TfrmMain.AddMRU(aFilename : string);
var
  i : integer;
begin
  for i := MRUList.Count - 1 downto 0 do
    if lowercase(MRUList[i]) = lowercase(aFilename) then
    begin
      MRUList.Delete(i);
    end;
  MRUList.Add(aFilename);
  if MRUList.Count > 15 then
    MRUList.Delete(0);
  RebuildMRU;
end;

procedure TfrmMain.ADOAfterDisconnect(Sender: TObject);
begin
  StatusBar.SimpleText := 'Disconnected';
  ConnectedTo := '';
end;

procedure TfrmMain.CheckForChanges;
begin
  if HasChanged then
    case MessageDlg('Your document has been changed.  Do you wish to save before exiting?',mtConfirmation,[mbYes, mbNo, mbCancel],0) of
      mrYes:    SaveFile;
      mrNo:     ;
      mrCancel: Abort;
    end;
end;

procedure TfrmMain.CheckSearchEnabledStates;
begin
  miEditFindNext.Enabled := gsSearchText <> '';
  miEditFindPrevious.Enabled := gsSearchText <> '';
  miEditReplace.Enabled := txtSource.ReadOnly;
end;

procedure TfrmMain.miEditFindPreviousClick(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, TRUE);
end;

procedure TfrmMain.miEditOptionsClick(Sender: TObject);
var
  tmp : TSynEditorOptionsContainer;
begin
  tmp := TSynEditorOptionsContainer.Create(nil);
  try
    tmp.Assign(txtSource);
    if dlgEditorOptions.Execute(tmp) then
    begin
      txtSource.Assign(tmp);
    end;
  finally
    tmp.Free;
  end;
end;

procedure TfrmMain.miEditReplaceClick(Sender: TObject);
begin
  ShowSearchReplaceDialog(TRUE);
end;

procedure TfrmMain.miEditFindNextClick(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, FALSE);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CheckForChanges;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  sr : TSearchRec;
  sl : TStringList;
  i : integer;
  ini : TIniFile;
  mi : TMenuItem;
  s: string;
  tmp : TSynEditorOptionsContainer;
begin
  tmp := TSynEditorOptionsContainer.Create(nil);
  try
    if FileExists(SettingsFilename) then
    begin
      ReadComponentResFile(SettingsFilename,tmp);
      txtSource.Assign(tmp);
    end;
  finally
    tmp.Free;
  end;

  PreviewSource := TStringList.Create;
  MRUList := TStringList.Create;
  ConStrings := TStringList.Create;
  pcMain.ActivePageIndex := 0;
  sl := TStringList.Create;
  try
    if not Boolean(FindFirst(ExtractFilePath(Application.ExeName)+'Templates\*.html',faAnyFile,sr)) then
      try
        repeat
          sl.Add(SR.Name);
        until not not boolean(FindNext(SR));
      finally
        FindClose(SR);
      end;
    sl.Sort;
    for i := 0 to sl.Count - 1 do
    begin
       mi := TMenuItem.Create(miFileNew);
       mi.Caption := Copy(sl[i],4,Length(sl[i])-8);
       mi.Tag := StrToInt(Copy(sl[i],1,2));
       if mi.Caption <> '-' then
         mi.OnClick := NewFromTemplate;
       miFileNew.Add(mi);
    end;
    RebuildConnections;
    ini := TIniFile.Create(INIFilename);
    try
      for i := 0 to 100 do
      begin
        s := ini.ReadString('MRU',IntToStr(i),'');
        if s <> '' then
          MRUList.Add(s);
      end;
    finally
      ini.Free;
    end;
  finally
     sl.Free;
  end;
  RebuildMRU;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  ini : TIniFile;
  i : integer;
  tmp : TSynEditorOptionsContainer;
begin
  ADO.Close;
  pcMain.ActivePage := tsSource;
  htmlPreview.Clear;
  ConStrings.Free;
  ini := TiniFile.Create(INIFilename);
  try
    for i := 0 to MRUList.Count - 1 do
      ini.WriteString('MRU',IntToStr(i),MRUList[i]);

    ini.WriteBool('SearchReplace','SearchBackwards',gbSearchBackwards);
    ini.WriteBool('SearchReplace','SearchCaseSensitive',gbSearchCaseSensitive);
    ini.WriteBool('SearchReplace','SearchFromCaret',gbSearchFromCaret);
    ini.WriteBool('SearchReplace','SearchSelectionOnly',gbSearchSelectionOnly);
    ini.WriteBool('SearchReplace','SearchTextAtCaret',gbSearchTextAtCaret);
    ini.WriteBool('SearchReplace','SearchWholeWords',gbSearchWholeWords);
    ini.WriteBool('SearchReplace','SearchRegex',gbSearchRegex);

    ini.WriteString('SearchReplace','SearchText',gsSearchText);
    ini.WriteString('SearchReplace','SearchTextHistory',gsSearchTextHistory);
    ini.WriteString('SearchReplace','ReplaceText',gsReplaceText);
    ini.WriteString('SearchReplace','ReplaceTextHistory',gsReplaceTextHistory);

    tmp := TSynEditorOptionsContainer.Create(nil);
    try
      tmp.Assign(txtSource);
      WriteComponentResFile(SettingsFilename,tmp);
    finally
      tmp.Free;
    end;

  finally
    ini.Free;
  end;
  MRUList.Free;
  PreviewSource.Free;
end;

procedure TfrmMain.htmlPreviewImageRequest(Sender: TObject; const SRC: string;
  var Stream: TMemoryStream);
begin
  Stream := WAITSTREAM;
  THTPTGetter.Create(SRC);
end;

procedure TfrmMain.miViewGlobalClick(Sender: TObject);
begin
  if not frmGlobals.Visible then
  begin
    frmGlobals.Show;
    frmGlobals.BringToFront;
  end else
    frmGlobals.BringToFront;
  miViewGlobal.Checked := frmGlobals.Visible;
end;

procedure TfrmMain.RebuildConnections;
var
  ini : TIniFile;
  I: Integer;
  mi: TMenuItem;
  sl: TStringList;
begin
  ini := TIniFile.Create(INIFilename);
  sl := TStringList.Create;
  try
    ConStrings.Clear;
    miConnectionsSelect.Clear;
    ini.ReadSection('Connections',sl);
    for I := 0 to sl.Count - 1 do
    begin
      ConStrings.Add(ini.ReadString('Connections',sl[i],''));

      mi := TMenuItem.Create(miConnectionsSelect);
      mi.Caption := sl[i];
      mi.Tag := i;
      mi.RadioItem := True;
      mi.OnClick := SelectConnection;
      miConnectionsSelect.Add(mi);
    end;
  finally
    sl.Free;
    ini.Free;
  end;
end;

procedure TfrmMain.RebuildMRU;
var
  i : integer;
  mi : TMenuItem;
begin
  miFileReopen.Clear;
  for i := MRUList.Count - 1 downto 0 do
  begin
    mi := TMenuItem.Create(miFileNew);
    mi.Caption := ExtractFileName(MRUList[i]);
    mi.Tag := i;
    mi.OnClick := ReopenFile;
    miFileReopen.Add(mi);
  end;
  miFileReopen.Enabled := miFileReopen.Count > 0;
end;

procedure TfrmMain.RefreshPreview;
var
  i : integer;
begin
  if pcMain.ActivePage = tsPreview then
  begin
    htmlPreview.Clear;
    Reporter.HTML.Text := txtSource.Lines.Text;
    if Reporter.HTML.Count > 0 then
    begin
      Reporter.Replacers.Clear;
      Reporter.Replacers.CreateDefaultReplacers;
      for i := 0 to frmGlobals.lvGlobals.Items.Count - 1 do
      begin
        Reporter.Replacers.AddReplacer(Copy(frmGlobals.lvGlobals.Items[i].Caption,2,Length(frmGlobals.lvGlobals.Items[i].Caption)-2),frmGlobals.lvGlobals.Items[i].SubItems[0]);
      end;
      Reporter.Execute;
      PreviewSource.Assign(Reporter.Output);
      htmlPreview.LoadStrings(Reporter.Output);
    end;
  end;
end;

procedure TfrmMain.ReopenFile(ASender: TObject);
begin
  CheckForChanges;
  txtSource.Lines.LoadFromFile(MRUList[TMenuItem(ASender).Tag]);
  tmrGlobalsTimer(nil);
  Filename := MRUList[TMenuItem(ASender).Tag];
  htmlPreview.Clear;
  HasChanged := False;
  AddMRU(FileName);
end;

procedure TfrmMain.ReporterAcquireData(ASender: TObject; const ID,
  Src: string; var Dataset: TDataSet);
begin
  if ConnectedTo = '' then
  begin
    pcMain.ActivePage := tsSource;
    raise Exception.Create('Cannot preview until connected.  Please select a connection.');
  end;
  
  Dataset := TADOQuery.Create(nil);
  try
    TADOQuery(Dataset).Connection := ADO;
    TADOQuery(Dataset).SQL.Text := Src;
    try
      TADOQuery(Dataset).Open;
    except
      on e: exception do
      begin
        case MessageDlg('There has been an problem opening the dataset: '+e.Message+#13#10#13#10+Src+#13#10#13#10+'Would you like to continue processing the report?',mtError,[mbYes, mbNo],0) of
          mrNo :
            begin
               pcMain.ActivePage := tsSource;
               Abort;
            end;
        end;
      end;
    end;
  except
    FreeAndNil(DataSet);
    raise;
  end;
end;

procedure TfrmMain.ReporterReleaseData(ASender: TObject;
  Dataset: TDataSet);
begin
  Dataset.Free;
end;

procedure TfrmMain.miConnectionsConfigureClick(Sender: TObject);
begin
  with TfrmConnections.Create(Self) do
  try
    if ShowModal=mrOK then
    begin
      RebuildConnections;
    end;
  finally
    free;
  end;
end;

procedure TfrmMain.miConnectionsDisconnectClick(Sender: TObject);
begin
  ADO.Close;
end;

procedure TfrmMain.miEditSearchClick(Sender: TObject);
begin
  ShowSearchReplaceDialog(FALSE);
end;

procedure TfrmMain.miFileExitClick(Sender: TObject);
begin
  CheckForChanges;
  Application.Terminate;
end;

procedure TfrmMain.miFileOpenClick(Sender: TObject);
begin
  OpenFile;
end;

procedure TfrmMain.miFileSaveAsClick(Sender: TObject);
begin
  SaveFileAs;
end;

procedure TfrmMain.miFileSaveClick(Sender: TObject);
begin
  SaveFile;
end;

procedure TfrmMain.miPreviewDefaultClick(Sender: TObject);
begin
  PreviewSource.SaveToFile(GetWindowsTempFolder+'~designerpreview.html');
  ShellExecute(Self.WindowHandle,'open',PChar(GetWindowsTempFolder+'~designerpreview.html'),'',PChar(GetWindowsTempFolder),sw_ShowMaximized);
end;

procedure TfrmMain.miPreviewPrintClick(Sender: TObject);
begin
  if dlgPrint.Execute(Self.WindowHandle) then
  begin
    htmlPreview.Print(0,High(Integer));
  end;
end;

procedure TfrmMain.miPreviewSourceClick(Sender: TObject);
begin
  PreviewSource.SaveToFile(GetWindowsTempFolder+'~designerpreview.txt');
  ShellExecute(Self.WindowHandle,'open',PChar(GetWindowsTempFolder+'~designerpreview.txt'),'',PChar(GetWindowsTempFolder),sw_ShowMaximized);
end;

procedure TfrmMain.miViewPreviewClick(Sender: TObject);
begin
  if pcMain.ActivePage = tsSource then
    pcMain.ActivePage := tsPreview
  else
    pcMain.ActivePage := tsSource;
  pcMainChange(pcMain);
end;

procedure TfrmMain.NewFromTemplate(ASender: TObject);
var
  sFile : string;
begin
  CheckForChanges;
  sFile := IntToStr(TMenuItem(ASender).Tag);
  while length(sFile)<2 do
    sFile := '0'+sFile;

  sFile := sFile+'-'+TMenuItem(ASender).Caption+'.html';
  sFile := StringReplace(sFile,'&','',[rfReplaceAll]);
  txtSource.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+'Templates\'+sFile);
  htmlPreview.Clear;
  FileName := '';
  CheckSearchEnabledStates;
  tmrGlobalsTimer(tmrGlobals);
end;

procedure TfrmMain.OpenFile;
begin
  if dlgOpen.Execute(Self.WindowHandle) then
  begin
    htmlPreview.Clear;
    txtSource.Lines.LoadFromFile(dlgOpen.FileName);
    tmrGlobalsTimer(nil);
    Filename := dlgOpen.Filename;
    HasChanged := False;
    AddMRU(dlgOpen.FileName);
    CheckSearchEnabledStates;
  end;
end;

procedure TfrmMain.PaxScriptScriptError(ASender: TObject; Error: string;
  Continue: Boolean);
begin
  Raise Exception.Create(Error);
end;

procedure TfrmMain.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tsPreview then
  begin
    RefreshPreview;
  end;
end;

procedure TfrmMain.PrintSetup1Click(Sender: TObject);
begin
  dlgPrintSetup.Execute(Self.WindowHandle);
end;

procedure TfrmMain.SaveFile;
begin
  if Filename = '' then
  begin
    SaveFileAs;
    exit;
  end;

  txtSource.Lines.SaveToFile(Filename);
  HasChanged := False;
end;

procedure TfrmMain.SaveFileAs;
begin
  dlgSave.FileName := FFilename;
  if dlgSave.Execute(Self.WindowHandle) then
  begin
    txtSource.Lines.SaveToFile(dlgSave.FileName);
    Filename := dlgSave.FileName;
    AddMRU(dlgSave.FileName);
  end;
  HasChanged := False;
end;

procedure TfrmMain.SelectConnection(ASender: TObject);
  function ExpandVars(str : string) : string;
  begin
    Result := StringReplace(str,'$(apppath)',ExtractFilePath(Application.ExeName),[rfReplaceAll, rfIgnoreCase]);
  end;
begin
  TMenuItem(ASender).Checked := True;
  ADO.Close;
  ADO.ConnectionString := ExpandVars(ConStrings[TMenuItem(ASender).Tag]);
  ADO.LoginPrompt := Pos('password',lowercase(ADO.ConnectionString))=0;
  ADO.Open;
  ConnectedTo := StringReplace(TMenuItem(ASender).Caption,'&','',[rfReplaceAll]);
  Statusbar.SimpleText := 'Connected: '+ConnectedTo;
end;

procedure TfrmMain.SetFilename(const Value: string);
begin
  FFilename := Value;
  UpdateCaption;
end;

procedure TfrmMain.SetHasChanged(const Value: boolean);
begin
  FHasChanged := Value;
  UpdateCaption;
end;

procedure TfrmMain.AutoCompleteBeforeExecute(Sender: TObject);
  procedure LoadOpenTags(list : TStrings);
  begin
    list.Add('<html>');
    list.Add('<body>');
  end;
  procedure LoadCloseTags(list : TStrings);
  begin
    list.Add('</html>');
    list.Add('</body>');
  end;
var
  sWord : string;
begin
{  AutoComplete.AutoCompleteList.Clear;
  sWord := txtSource.WordAtCursor;
  if (Copy(sWord,1,1)='<') and
     (Copy(sWord,length(sWord)-1,1) <> '>')  then
  begin
    if Copy(sWord,2,1)<>'/' then
      LoadOpenTags(AutoComplete.AutoCompleteList)
    else
      LoadCloseTags(AutoComplete.AutoCompleteList);
  end else
  begin
    LoadOpenTags(AutoComplete.AutoCompleteList);
    LoadCloseTags(AutoComplete.AutoCompleteList);
  end;}
end;

procedure TfrmMain.tmrGlobalsTimer(Sender: TObject);
begin
  tmrGlobals.Enabled := False;
  TFindGlobals.Create(txtSource.Lines);
end;

procedure TfrmMain.txtSourceChange(Sender: TObject);
begin
  tmrGlobals.Enabled := False;
  HasChanged := True;
  tmrGlobals.Enabled := True;
end;

procedure TfrmMain.txtSourceReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    Action := raSkip
  else begin
    APos := txtSource.ClientToScreen(
      txtSource.RowColumnToPixels(
      txtSource.BufferToDisplayPos(
        BufferCoord(Column, Line) ) ) );
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    if frmConfirmReplace = nil then
      frmConfirmReplace := TfrmConfirmReplace.Create(Application);
    frmConfirmReplace.PrepareShow(EditRect, APos.X, APos.Y,
      APos.Y + txtSource.LineHeight, ASearch);
    case frmConfirmReplace.ShowModal of
      mrYes: Action := raReplace;
      mrYesToAll: Action := raReplaceAll;
      mrNo: Action := raSkip;
      else Action := raCancel;
    end;
  end;
end;

procedure TfrmMain.UpdateCaption;
var
  sFile : string;
begin
  sFile := FFilename;
  if sFile = '' then
    sFile := '(unknown)';
  if FHasChanged then
    sFile := sFile+' *';
  
  Caption := 'HTML Report Designer ['+sFile+']';
end;

{ THTPTGetter }

constructor THTPTGetter.Create(URL: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FURL:= URL;
  Output := TMemoryStream.Create;
  Resume;
end;

procedure THTPTGetter.Execute;
var
  http: TIdHTTP;
begin
  http := TIdHTTP.Create(nil);
  try
    http.Get(FURL,Output);
  finally
    http.Free;
  end;
  Synchronize(UpdateGUI);
end;

procedure THTPTGetter.UpdateGUI;
begin
  frmMain.htmlPreview.InsertImage(FURL,Output);
  Output.Free;
end;

initialization
  INIFilename := ExtractFilePath(Application.ExeName)+'Designer.ini';
  SettingsFilename := ExtractFilePath(Application.Exename)+'Designer.settings';

end.
