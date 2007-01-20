unit ArcHTMLReporter;

interface

uses
  SysUtils, Classes, DB, Variants, ArcHRFastStrings, ArcTagParser, Math;

{$R 'HTMLReporter.dcr'}

type TAggType = (atAvg, atCount, atMax, atMin, atSum);
const AGGTYPE : array[low(TAggType)..high(TAggType)] of string =
                ('avg','count','max','min','sum');

type
  TAggObject = class(TObject)
  private
    FFieldTag : string;
    FID : string;
    FName : string;
    FCount : integer;
    FActive: array[low(TAggType)..high(TAggType)] of boolean;
    function GetActive(at: TAggType): boolean;
    procedure SetActive(at: TAggType; const Value: boolean);
    function GetFullName: string;
  public
    constructor Create(Parser : TTagParser; List : TList; AID, AName, AFieldTag : string); virtual;
    destructor Destroy; override;

    function Value(AggType : TAggType) : string; virtual; abstract;
    procedure NewValue(Value : Variant); virtual;
    property ID : string read FID;
    property Name : string read FName;
    property Active[at : TAggType] : boolean read GetActive write SetActive;
    property FullName : string read GetFullName;
    property Count : integer read FCount;
  end;

  TAggDate = class(TAggObject)
  private
    Max : TDateTime;
    Min : TDateTime;
    Values : Array of Extended;
  public
    constructor Create(Parser : TTagParser; List: TList; AID, AName, FieldTag: string); override;
    destructor Destroy; override;

    procedure NewValue(Value: Variant); override;
    function Value(AggType: TAggType): string; override;
  end;

  TAggNumber = class(TAggObject)
  private
    Max : Extended;
    Min : Extended;
    Values : Array of Extended;
  public
    constructor Create(Parser : TTagParser; List: TList; AID, AName, FieldTag: string); override;
    destructor Destroy; override;

    procedure NewValue(Value: Variant); override;
    function Value(AggType: TAggType): string; override;
  end;

  TAggString = class(TAggObject)
  private
    Min : string;
    Max : string;
    Values : Array of string;
  public
    constructor Create(Parser : TTagParser; List: TList; AID, AName, FieldTag: string); override;
    destructor Destroy; override;

    procedure NewValue(Value: Variant); override;
    function Value(AggType: TAggType): string; override;
  end;

  TArcAcquireDataEvent = procedure(ASender : TObject; const ID, Src : string; var Dataset : TDataset) of object;
  TArcReleaseDataEvent = procedure(ASender : TObject; Dataset : TDataset) of object;

  TArcReplacerTiming = (rtBeforeData, rtAfterData);

  TArcReplacer = class(TCollectionItem)
  private
    FHTML: TStrings;
    FTagName: string;
    FTiming: TArcReplacerTiming;
    procedure SetHTML(const Value: TStrings);
    procedure SetTagName(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property TagName : string read FTagName write SetTagName;
    property HTML : TStrings read FHTML write SetHTML;
    property Timing : TArcReplacerTiming read FTiming write FTiming default rtAfterData;
  end;

  TArcReplacers = class(TOwnedCollection)
  private
    function GetItem(idx: integer): TArcReplacer;
    procedure SetItem(idx: integer; const Value: TArcReplacer);
  public
    function Add : TArcReplacer;
    property Items[idx : integer] : TArcReplacer read GetItem write SetItem; default;
    procedure CreateDefaultReplacers;
    function AddReplacer(TagName, ReplaceHTML : string; ATiming : TArcReplacerTiming = rtBeforeData) : TArcReplacer;
  end;

  TArcTagEvent = procedure(Sender : TObject; Tag : TTagItem; var Skip : boolean) of object; 
  TArcTextEvent = procedure(Sender : TObject; var Text : string; var Skip : boolean) of object; 
  TArcDataReplaceEvent = procedure(Sender : TObject; DataField : string; var ReplaceText : string) of object;

  TArcTagHandler = procedure (ASender : TObject; ATag : TTagItem; var AReplaceText : string; var IsTag : boolean) of object;

  TDataReplaced = (drNone, drBefore, drAfter, drBoth);
  TTagHandler = class(TObject)
  public
    Tag : string;
    Handler : TArcTagHandler;
    Initer : TNotifyEvent;
    DataReplaced : TDataReplaced;
  end;

  THTMLReporter = class(TComponent)
  private
    FHandlers : TList; 
    FHTML : TStrings;
    FParser : TTagParser;
    FOnReleaseData: TArcReleaseDataEvent;
    FReplacers: TArcReplacers;
    FOnAcquireData: TArcAcquireDataEvent;
    FOutput: TStrings;
    FBlankGroupText: string;
    FDataTag: string;
    FFieldTag: string;
    FFormatTag: string;
    FOnBeforeTag: TArcTagEvent;
    FOnBeforeText: TArcTextEvent;
    FReduceLineBreaks: boolean;
    FOnDataReplace: TArcDataReplaceEvent;
    procedure SetFieldTag(const Value: string);
    procedure SetHTML(const Value: TStrings);
    procedure SetReplacers(const Value: TArcReplacers);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Output : TStrings read FOutput;
    procedure Execute;
    function RegisterHandler(const Tag : string; Handler : TArcTagHandler; Initer : TNotifyEvent; DataReplaced : TDataReplaced) : integer;
    procedure UnregisterHandler(HandlerID : integer);
  published
    property HTML : TStrings read FHTML write SetHTML;
    property Replacers : TArcReplacers read FReplacers write SetReplacers;
    property OnAcquireData : TArcAcquireDataEvent read FOnAcquireData write FOnAcquireData;
    property OnReleaseData : TArcReleaseDataEvent read FOnReleaseData write FOnReleaseData;
    property BlankGroupText : string read FBlankGroupText write FBlankGroupText;
    property DataTag : string read FDataTag write FDataTag;
    property FormatTag : string read FFormatTag write FFormatTag;
    property FieldTag : string read FFieldTag write SetFieldTag;
    property OnBeforeTag : TArcTagEvent read FOnBeforeTag write FOnBeforeTag;
    property OnDataReplace : TArcDataReplaceEvent read FOnDataReplace write FOnDataReplace;
    property OnBeforeText : TArcTextEvent read FOnBeforeText write FOnBeforeText;
    property ReduceLineBreaks : boolean read FReduceLineBreaks write FReduceLineBreaks;
  end;

procedure Register;

function StrToAggType(at : string) : TAggType;
function AggTypeToStr(at : TAggType) : string;
procedure WipeStringArray(var ary : array of string);

implementation

procedure Register;
begin
  RegisterComponents('Arcana', [THTMLReporter]);
end;

function StrToAggType(at : string) : TAggType;
var
  i : TAggType;
  b : boolean;
begin
  Result := atAvg;
  b := False;
  at := lowercase(at);
  for i := low(TAggType) to High(TAggType) do
    if AGGTYPE[i] = at then
    begin
      b := True;
      Result := i;
      break;
    end;
  if not b then
    raise Exception.Create('Invalid TAggType string.');
end;

function AggTypeToStr(at : TAggType) : string;
begin
  Result := AGGTYPE[at];
end;

procedure WipeStringArray(var ary : array of string);
var
  i : integer;
begin
  for i := low(ary) to high(ary) do
    ary[i] := '';
end;

{ TArcReplacers }

function TArcReplacers.GetItem(idx: integer): TArcReplacer;
begin
  Result := TArcReplacer(inherited Items[idx]);
end;

procedure TArcReplacers.SetItem(idx: integer; const Value: TArcReplacer);
begin
  inherited Items[idx] := Value;
end;

procedure TArcReplacers.CreateDefaultReplacers;
begin
  AddReplacer('%PageBreak%', '<div style="width:1px;height:1px;overflow:hidden;page-break-after: always;"></div>');
  AddReplacer('%AutoPrint%', '<script lantuage="Javascript">window.print();</script>');
  (*AddReplacer('%PrintConfig%','<style>' +
    '@media print {' +
    '  @page         { margin: 0.5in;0.5in;0.5in;0.5in;}' +
    '}</style>');
  AddReplacer('%PrintConfig.Portrait%','<style>' +
    '@media print {' +
    '  @page         { margin: 0.5in;0.5in;0.5in;0.5in;size:portrait;}' +
    '}</style>');
  AddReplacer('%PrintConfig.Landscape%','<style>' +
    '@media print {' +
    '  @page         { margin: 0.5in;0.5in;0.5in;0.5in;size:landscape;}' +
    '}</style>');*)
end;

function TArcReplacers.Add: TArcReplacer;
begin
  Result := TArcReplacer(inherited Add);
end;

function TArcReplacers.AddReplacer(TagName, ReplaceHTML: string; ATiming : TArcReplacerTiming = rtBeforeData): TArcReplacer;
begin
  Result := Add;
  Result.TagName := TagName;
  Result.HTML.Text := ReplaceHTML;
  Result.Timing := ATiming;
end;

{ TArcReplacer }

constructor TArcReplacer.Create(Collection: TCollection);
begin
  inherited;
  FHTML := TStringList.Create;
end;

destructor TArcReplacer.Destroy;
begin
  FHTML.Free;
  inherited;
end;

function TArcReplacer.GetDisplayName: string;
begin
  Result := FTagName;
  if Result = '' then
    Result := '(enter TagName)';
end;

procedure TArcReplacer.SetHTML(const Value: TStrings);
begin
  FHTML.Assign(Value);
end;

procedure TArcReplacer.SetTagName(const Value: string);
begin
  FTagName := Value;
end;

{ THTMLReporter }

constructor THTMLReporter.Create(AOwner: TComponent);
begin
  inherited;
  FHandlers := TList.Create;
  FFieldTag := '{}';
  FDataTag := 'data';
  FFormatTag := 'format';
  FParser := TTagParser.Create;
  FHTML := TStringList.Create;
  FOutput := TStringList.Create;
  FReplacers := TArcReplacers.Create(Self,TArcReplacer);
  FReplacers.CreateDefaultReplacers;
  FBlankGroupText := '&nbsp;'
end;

destructor THTMLReporter.Destroy;
var
  i: Integer;
begin
  for i := 0 to FHandlers.Count - 1 do
    TObject(FHandlers[i]).Free;
  FHandlers.Free;

  FParser.Free;
  FReplacers.Free;
  FHTML.Free;
  FOutput.Free;
  inherited;
end;

procedure THTMLReporter.SetFieldTag(const Value: string);
begin
  if Length(Value) <> 2 then
    raise Exception.Create('You must enter only two characters, the first to open a data field and the second to close the datafield.');
  if (FastCharPos(Value,'<',1) > 0) or (FastcharPos(Value,'>',1) > 0) then
    raise Exception.Create('You cannot use < or > for a field tag');
  FFieldTag := Value;
end;

procedure THTMLReporter.SetHTML(const Value: TStrings);
begin
  FHTML.Assign(Value);
end;

procedure THTMLReporter.SetReplacers(const Value: TArcReplacers);
begin
  FReplacers.Assign(Value);
end;

procedure THTMLReporter.UnregisterHandler(HandlerID: integer);
var
  idx : integer;
begin
  idx := FHandlers.IndexOf(Pointer(HandlerID));
  if idx >=0 then
  begin
    TObject(FHandlers[idx]).Free;
    FHandlers.Delete(idx);
  end;
end;

procedure THTMLReporter.Execute;
var
  lGroups : TList;
  function DoDataSection(tpNew : TTagParser) : string;
  var
    aryData : array of TDataset;
    aryDataLastValues : array of array of String;
    aryDataNewGroup : array of array of Boolean;
    aryDataID : array of String;
    aryDataIdx : array of Integer;
    bNothingToDo: Boolean;

    function ReplaceIt(str : string) : string;
    var
      i, x : integer;
      sID, sName, sVal : string;
      at: TAggType;
    begin
      Result := str;
      for i := Low(aryData) to High(aryData) do
      begin
        if aryData[i] <> nil then
        begin
          for x := 0 to aryData[i].Fields.Count - 1 do
          begin
            if aryData[i].Fields[x].DataType in [ftMemo, ftBlob] then
              sVal := aryData[i].Fields[x].AsString
            else
              sVal := aryData[i].Fields[x].DisplayText;
            sID := aryDataID[i];
            sName := aryData[i].Fields[x].FieldName;

            if Assigned(FOnDataReplace) then
              FOnDataReplace(Self,sName,sVal);

            Result := FastReplace(Result,FFieldTag[1]+sName+FFieldTag[2], sVal, False);
            Result := FastReplace(Result,FFieldTag[1]+sID+'.'+sName+FFieldTag[2], sVal, False);
            if aryDataNewGroup[i][x] then
              Result := FastReplace(Result,FFieldTag[1]+sID+'.'+sName+':group}', sVal, False)
            else
              Result := FastReplace(Result,FFieldTag[1]+sID+'.'+sName+':group}', FBlankGroupText, False);
          end;
        end;
      end;
      for i := 0 to lGroups.Count - 1 do
      begin
        for at := low(TAggType) to high(TAggType) do
        begin
          if TAggObject(lGroups[i]).Active[at] then
          begin
            Result := FastReplace(Result,FFieldTag[1]+TAggObject(lGroups[i]).Name+':'+AggTypeToStr(at)+FFieldTag[2], TAggObject(lGroups[i]).Value(at), False);
            Result := FastReplace(Result,FFieldTag[1]+TAggObject(lGroups[i]).FullName+':'+AggTypeToStr(at)+FFieldTag[2], TAggObject(lGroups[i]).Value(at), False);
          end;
        end;
      end;
    end;
    procedure DoAggData;
      function FindGroup(ID, Name : string) : TAggObject;
      var
        i : integer;
      begin
        Result := nil;
        for I := 0 to lGroups.Count - 1 do
          if (TAggObject(lGroups[i]).Name = Name) and
             (TAggObject(lGroups[i]).ID = ID) then
          begin
            Result := TAggObject(lGroups[i]);
            break;
          end;
      end;
      procedure GroupDateValue(id, name : string; Value : TDateTime);
      var
        grp : TAggObject;
      begin
        grp := FindGroup(ID,Name);
        if grp = nil then
          TAggDate.Create(FParser, lGroups,ID, Name, FFieldTag).NewValue(Value)
        else
          grp.NewValue(Value)
      end;
      procedure GroupStringValue(id, name, value : string);
      var
        grp : TAggObject;
      begin
        grp := FindGroup(ID,Name);
        if grp = nil then
          TAggString.Create(FParser, lGroups,ID, Name, FFieldTag).NewValue(Value)
        else
          grp.NewValue(Value)
      end;
      procedure GroupNumberValue(id, name : string; Value : Extended);
      var
        grp : TAggObject;
      begin
        grp := FindGroup(ID,Name);
        if grp = nil then
          TAggNumber.Create(FParser, lGroups,ID, Name, FFieldTag).NewValue(Value)
        else
          grp.NewValue(Value)
      end;
    var
      i, x : integer;
      sName, sID : string;
    begin
      for i := Low(aryData) to High(aryData) do
      begin
        for x := 0 to aryData[i].Fields.Count - 1 do
        begin
          sID := aryDataID[i];
          sName := aryData[i].Fields[x].FieldName;
          case aryData[i].Fields[x].DataType of
            ftUnknown, ftString, ftFixedChar, ftWideString,
            ftFixedWideChar:
              GroupStringValue(sID, sName, aryData[i].Fields[x].DisplayText);

            ftMemo, ftFmtMemo, ftVariant,
            ftWideMemo:
              GroupStringValue(sID, sName, aryData[i].Fields[x].AsString);

            ftGuid:
              GroupStringValue(sID, sName, aryData[i].Fields[x].AsString);

            ftFloat,
            ftCurrency,
            ftSmallint, ftInteger, ftWord,
            ftBCD, ftAutoInc,
            ftLargeint:
              GroupNumberValue(sID, sName, aryData[i].Fields[x].AsFloat);

            ftBoolean:
              GroupStringValue(sID, sName, BoolToStr(aryData[i].Fields[x].AsBoolean,True));

            ftDate, ftTime, ftDateTime, ftTimeStamp,
            ftOraTimeStamp:
              begin
                GroupDateValue(sID, sName, aryData[i].Fields[x].AsDateTime);
              end;

            ftBytes, ftVarBytes, ftBlob, ftGraphic, ftParadoxOle,
            ftDBaseOle, ftTypedBinary, ftCursor, ftADT, ftArray,
            ftReference, ftDataSet, ftOraBlob, ftOraClob, ftInterface,
            ftIDispatch, ftFMTBcd,
            ftOraInterval:;
          end;
        end;
      end;
    end;
    function CheckRegisteredTags(SrcTag : TTagItem; New : TTagParser) : boolean;
  var
      i: Integer;
      bIsTag : boolean;
      sTag : string;
      ti : TTagItem;
      sResult: string;
    begin
      Result := False;
      if FHandlers.Count > 0 then
      begin
        ti := TTagItem.Create;
        try
          sTag := lowercase(SrcTag.Name);
          for i := 0 to FHandlers.Count - 1 do
          begin
            if TTagHandler(FHandlers[i]).Tag = sTag then
            begin
              result := True;
              if TTagHandler(FHandlers[i]).DataReplaced in [drBefore, drBoth] then
                ti.Text := ReplaceIt(SrcTag.Text)
              else
                ti.Text := SrcTag.Text;

              TTagHandler(FHandlers[i]).Handler(Self, ti, sResult, bIsTag);

              if TTagHandler(FHandlers[i]).DataReplaced in [drAfter, drBoth] then
                sResult := ReplaceIt(sResult);

              if (sResult <> '') and SrcTag.HasParam('visible') then
              begin
              if bIsTag then
                New.AddTag.Text := sResult
              else
                New.AddText.Text := sResult;
            end;
          end;
          end;
        finally
          ti.Free;
        end;
      end;
    end;
  var
    i, y, iDepth : integer;
    s, sTag, sID, sSrc : string;
    bIsTag : boolean;
    fld : TField;
    bSkip : boolean;
  begin
    setLength(aryData,0);
    setLength(aryDataID,0);
    setLength(aryDataIdx,0);
    setLength(aryDataLastValues,0);
    setLength(aryDataNewGroup,0);
    try
      iDepth := 0;
      y := 0;
      bNothingToDo := False;
      while y < FParser.Count - 1 do
      begin
        bIsTag := (FParser.Data[y] is TTagItem);

        if bIsTag then
          s := FParser.Tags[y].Text
        else
          s := FParser.Texts[y].Text;

        if bIsTag then
        begin
          bSkip := False;
          if Assigned(FOnBeforeTag) then
            FOnBeforeTag(Self,FParser.Tags[y], bSkip);

          if not bSkip then
          begin
            sTag := lowercase(FParser.Tags[y].Name);

            if (not bNothingToDo) then
              if CheckRegisteredTags(FParser.Tags[y], tpNew) then
              begin
                inc(y);
                continue;
              end;


            if sTag = FDataTag then
            begin
              sID := ReplaceIt(TTagItem(FParser.Data[y]).FindParamValue('ID'));
              sSrc := ReplaceIt(TTagItem(FParser.Data[y]).FindParamValue('src'));

              inc(iDepth);
              setLength(aryData,iDepth);
              setLength(aryDataID,iDepth);
              setLength(aryDataIdx,iDepth);

              if not bNothingToDo then
              begin
                FOnAcquireData( Self, sID, sSrc, aryData[iDepth-1]);

                if aryData[iDepth-1] = nil then
                  raise Exception.Create('OnAcquireData returned no dataset');

                bNothingToDo := aryData[iDepth-1].EOF;

                SetLength(aryDataLastValues,iDepth);
                SetLength(aryDataNewGroup,iDepth);

                SetLength(aryDataLastValues[iDepth-1],aryData[iDepth-1].Fields.Count);
                SetLength(aryDataNewGroup[iDepth-1],aryData[iDepth-1].Fields.Count);
                for i := 0 to aryData[iDepth-1].Fields.Count - 1 do
                begin
                  aryDataLastValues[iDepth-1][i] := aryData[iDepth-1].Fields[i].AsString;
                  aryDataNewGroup[iDepth-1][i] := True;
                end;

                aryDataID[iDepth-1] := TTagItem(FParser.Data[y]).FindParamValue('ID');
                aryDataIdx[iDepth-1] := y;
              end else
              begin
                aryData[iDepth-1] := nil;
                SetLength(aryDataLastValues,iDepth);
                SetLength(aryDataNewGroup,iDepth);

                SetLength(aryDataLastValues[iDepth-1],0);
                SetLength(aryDataNewGroup[iDepth-1],0);
                aryDataID[iDepth-1] := '';
                aryDataIdx[iDepth-1] := y;
              end;
            end else
              if sTag = '/'+FDataTag then
              begin
                if not bNothingToDo then
                begin
                  DoAggData;
                  aryData[iDepth-1].Next;
                  if aryData[iDepth-1].EOF then
                  begin
                    if Assigned(FOnReleaseData) then
                      FOnReleaseData(Self,aryData[iDepth-1]);


                    aryDataID[iDepth-1]:= '';
                    aryDataIdx[iDepth-1]:= 0;

                    dec(iDepth);

                    setLength(aryData,iDepth);
                    setLength(aryDataID,iDepth);
                    setLength(aryDataIdx,iDepth);
                    bNothingToDo := False;
                  end else
                  begin
                    for i := 0 to aryData[iDepth-1].Fields.Count - 1 do
                    begin
                      aryDataNewGroup[iDepth-1][i] := aryDataLastValues[iDepth-1][i] <> aryData[iDepth-1].Fields[i].AsString;
                      aryDataLastValues[iDepth-1][i] := aryData[iDepth-1].Fields[i].AsString;
                    end;

                    y := aryDataIdx[iDepth-1];
                  end;
                end else
                begin
                  dec(iDepth);
                  setLength(aryData,iDepth);
                  setLength(aryDataID,iDepth);
                  setLength(aryDataIdx,iDepth);
                  if iDepth > 0 then
                    bNothingToDo := (aryData[iDepth-1] = nil) or (aryData[iDepth-1].EOF)
                  else
                    bNothingToDo := true;
                end;
              end else
                if sTag = FFormatTag then
                begin
                  s := FParser.Tags[y].FindParamValue('id');
                  for i := 0 to iDepth - 1 do
                  begin
                    if (aryData[i] <> nil) and (aryDataID[i] = s) then
                    begin
                      fld := aryData[i].FindField(FParser.Tags[y].FindParamValue('field'));
                      if fld is TNumericField then
                        TNumericField(fld).DisplayFormat := FParser.Tags[y].FindParamValue('format');
                      if fld is TDateTimeField then
                        TDateTimeField(fld).DisplayFormat := FParser.Tags[y].FindParamValue('format');
                      if fld is TSQLTimeStampField then
                        TSQLTimeStampField(fld).DisplayFormat := FParser.Tags[y].FindParamValue('format');
                      if fld is TAggregateField then
                        TAggregateField(fld).DisplayFormat := FParser.Tags[y].FindParamValue('format');
                      if fld is TAggregateField then
                        TAggregateField(fld).DisplayFormat := FParser.Tags[y].FindParamValue('format');
                      break;
                    end;
                  end;
                end else
                  if not bNothingToDo then
                    tpNew.AddTag.Text := ReplaceIt('<'+s+'>');
          end;
        end else
        begin
          bSkip := False;
          if Assigned(FOnBeforeText) then
            FOnBeforeText(Self,s,bSkip);
          if (not bSkip) and (not bNothingToDo) then
          begin
            if (FReduceLineBreaks) then
            begin
              if Trim(s)<>'' then
                tpNew.AddText.Text := ReplaceIt(s);
            end else
              tpNew.AddText.Text := ReplaceIt(s);
          end;
        end;
        inc(y);
      end;
    finally
      for i := 0 to high(aryDataLastValues) do
        WipeStringArray(aryDataLastValues[i]);
    end;
  end;
  procedure DoReplacers(Parser : TTagParser; rt : TArcReplacerTiming);
  var
    i, y : integer;
    s : string;
  begin
    for i := 0 to FReplacers.Count -1 do
    begin
      if FReplacers[i].Timing = rt then
      begin
        for y := 0 to Parser.Count - 1 do
        begin
          if Parser.Data[y] is TTagItem then
          begin
            s := TTagItem(Parser.Data[y]).Text;
            TTagItem(Parser.Data[y]).Text :=
              FastReplace(s, FFieldTag[1]+FReplacers[i].TagName+FFieldTag[2],Copy(FReplacers[i].HTML.Text,1,length(FReplacers[i].HTML.Text)-2),False);
          end else
            if Parser.Data[y] is TStringList then
            begin
              s := TStringList(Parser.Data[y]).Text;
              TStringList(Parser.Data[y]).Text :=
                FastReplace(s, FFieldTag[1]+FReplacers[i].TagName+FFieldTag[2],Copy(FReplacers[i].HTML.Text,1,length(FReplacers[i].HTML.Text)-2),False);
            end;
        end;
      end;
    end;
  end;
var
  tpNew : TTagParser;
  i : integer;
begin
  if not Assigned(FOnAcquireData) then
    raise Exception.Create('OnAcquireData Event is not assigned');

  for i := 0 to FHandlers.Count - 1 do
    TTagHandler(FHandlers[i]).Initer(Self);

  tpNew := TTagParser.Create;
  try
    lGroups := TList.Create;
    try
      FParser.Clear;
      FParser.FileData.Assign(FHTML);
      FParser.Parse;
      DoReplacers(FParser, rtBeforeData);
      DoDataSection(tpNew);
      DoReplacers(tpNew, rtAfterData);
      tpNew.Combine;
      FOutput.Assign(tpNew.FileData);
    finally
      for i := 0 to lGroups.Count - 1 do
        TObject(lGroups[i]).Free;
      lGroups.Free;
    end;
  finally
    tpNew.Free;
  end;
  end;

function THTMLReporter.RegisterHandler(const Tag : string; Handler: TArcTagHandler; Initer : TNotifyEvent;  DataReplaced : TDataReplaced): integer;
var
  th : TTagHandler;
begin
  th := TTagHandler.Create;
  th.Tag := lowercase(Tag);
  th.Handler := Handler;
  th.Initer := Initer;
  th.DataReplaced := DataReplaced;
  FHandlers.Add(th);
  Result := Integer(th);
end;

{ TAggObject }

constructor TAggObject.Create(Parser : TTagParser; List: TList; AID, AName, AFieldTag : string);
var
  sFile, s, s2 : string;
  iLen : integer;
  at : TAggType;
begin
  inherited Create;
  List.Add(Self);

  FFieldTag := AFieldTag;

  sFile := Parser.FileData.Text;
  iLen := Length(sFile);
  
  for at := low(TAggType) to high(TAggType) do
  begin
    s := FFieldTag[1]+AName+':'+AggTypeToStr(at)+FFieldTag[2];
    s2 := FFieldTag[1]+AID+'.'+AName+':'+AggTypeToStr(at)+FFieldTag[2];
    FActive[at] := (FastPos(sFile,s,iLen,length(s),1) > 0) or
                  (FastPos(sFile,s2,iLen,length(s2),1) > 0);
  end;

  FName := AName;
  FID := AID;
end;

destructor TAggObject.Destroy;
begin

  inherited;
end;

function TAggObject.GetActive(at: TAggType): boolean;
begin
  Result := FActive[at];
end;

function TAggObject.GetFullName: string;
begin
  Result := FID+'.'+FName;
end;

procedure TAggObject.NewValue(Value: Variant);
begin
  inc(FCount);
end;

procedure TAggObject.SetActive(at: TAggType; const Value: boolean);
begin
  FActive[at] := Value;
end;

{ TAggDate }

constructor TAggDate.Create(Parser : TTagParser; List: TList; AID, AName, FieldTag: string);
begin
  inherited;
  Self.Min := EncodeDate(3999,12,31);
  Self.Max := 0;
end;

destructor TAggDate.Destroy;
begin
  SetLength(Values,0);
  inherited;
end;

procedure TAggDate.NewValue(Value: Variant);
begin
  inherited;
  SetLength(Values,Count);
  Values[Count-1] := Value;
  if Value < Self.Min then
    Self.Min := Value;
  if Value > Self.Max then
    Self.Max := Value;
end;

function TAggDate.Value(AggType: TAggType): string;
  function DoSum : TDateTime;
  var
    i: Integer;
  begin
    Result := 0;
    for i := low(Values) to High(Values) do
      Result := Result+Values[i];
  end;
  function DateStr(dt : TDateTime) : string;
  begin
    DateTimeToString(Result,'yyyy-mm-dd hh:nn:ss.zzz',dt);
  end;
begin
  case AggType of
    atAvg: Result := DateStr(DoSum / Count);
    atCount: Result := IntToStr(Count);
    atMax: Result := DateStr(Self.Max);
    atMin: Result := DateStr(Self.Min);
    atSum: Result := DateStr(DoSum);
  end;
end;

{ TAggNumber }

constructor TAggNumber.Create(Parser : TTagParser; List: TList; AID, AName, FieldTag: string);
begin
  inherited;
  Self.Min := High(Integer);
  Self.Max := Low(Integer);
end;

destructor TAggNumber.Destroy;
begin
  SetLength(Values,0);
  inherited;
end;

procedure TAggNumber.NewValue(Value: Variant);
begin
  inherited;
  SetLength(Values,Count);
  Values[Count-1] := Value;
  if Value < Self.Min then
    Self.Min := Value;
  if Value > Self.Max then
    Self.Max := Value;
end;

function TAggNumber.Value(AggType: TAggType): string;
  function DoSum : Extended;
  var
    i: Integer;
  begin
    Result := 0;
    for i := low(Values) to High(Values) do
      Result := Result+Values[i];
  end;
begin
  case AggType of
    atAvg: Result := FloatToStr(DoSum / Count);
    atCount: Result := FloatToStr(Count);
    atMax: Result := FloatToStr(Self.Max);
    atMin: Result := FloatToStr(Self.Min);
    atSum: Result := FloatToStr(DoSum);
  end;
end;

{ TAggString }

constructor TAggString.Create(Parser : TTagParser; List: TList; AID, AName, FieldTag: string);
begin
  inherited;
  Self.Min := '';
  Self.Max := '';
end;

destructor TAggString.Destroy;
begin
  WipeStringArray(Values);
  SetLength(Values,0);
  inherited;
end;

procedure TAggString.NewValue(Value: Variant);
begin
  inherited;
  SetLength(Values,Count);
  Values[Count-1] := Value;
  if (Self.Min = '') or (CompareText(Value,Self.Min) < 0) then
    Self.Min := Value;
  if (Self.Max = '') or (CompareText(Value,Self.Max) > 0) then
    Self.Max := Value;
end;

function TAggString.Value(AggType: TAggType): string;
  function DoSum : string;
  var
    i: Integer;
  begin
    Result := '';
    for i := low(Values) to High(Values) do
      Result := Result+Values[i];
  end;
begin
  case AggType of
    atAvg: Result := '';
    atCount: Result := IntToStr(Count);
    atMax: Result := Self.Max;
    atMin: Result := Self.Min;
    atSum: Result := DoSum;
  end;
end;

end.
