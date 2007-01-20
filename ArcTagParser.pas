unit ArcTagParser;

interface

uses
  SysUtils, Classes, StrUtils, ArcHRFastStrings;

type
  TParamItem = class(TObject)
  private
    FName : string;
    FValue : string;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    property Text : string read GetText write SetText;
    property Name : string read FName write FName;
    property Value : string read FValue write FValue;
  end;

  TTagItem = class(TObject)
  private
    FParams: TList;
    FName: string;
    FText: string;
    procedure SetText(const Value: string);
    function GetParam(idx: integer): TParamItem;
    function GetParamCount: integer;
  protected
    procedure ClearParams;
    procedure ParseTag;
    //procedure SetEntities;
  public
    constructor Create;
    destructor Destroy; override;
    function FindParamValue(const Name: string): string;
    function HasParam(const Name : string) : boolean;
    property Params[Index: integer]: TParamItem read GetParam;
    property ParamCount: integer read GetParamCount;
    property Name: string read FName;
    property Text: string read FText write SetText;
    function AddParam : TParamItem;
    procedure DeleteParam(idx : integer); 
  end;

  TTagParser = class(TObject)
  private
    FFileData : TStringList;
    FTags: TList;
    function GetTag(idx: integer): TTagItem;
    function GetText(idx: integer): TStringList;
    procedure SetFileData(const Value: TStringList);
    procedure SetData(idx: integer; const Value: TObject);
    function GetCount: integer;
    function GetData(idx: integer): TObject;
  public
    constructor Create;
    destructor Destroy; override;

    function AddTag : TTagitem;
    function AddText : TStringList;
    procedure Delete(idx : integer);
    procedure Clear;

    procedure Parse;
    procedure Combine;

    property Count: integer read GetCount;
    property Data[idx: integer]: TObject read GetData write SetData;
    property Tags[idx: integer]: TTagItem read GetTag;
    property Texts[idx: integer]: TStringList read GetText;
    property FileData : TStringList read FFileData write SetFileData;
  end;

implementation

{ TParamItem }

function TParamItem.GetText: string;
var
  sQuote : string;
begin
  if FValue <> '' then
  begin
    if FastCharPos(FValue,' ',1) > 0 then
      sQuote := '"';
    Result := FName+'='+sQuote+FValue+sQuote;
  end else
    Result := FName;
end;

procedure TParamItem.SetText(const Value: string);
var
  iPos: integer;
begin
  iPos := FastCharPos(Value,'=',1);
  if iPos>0 then
  begin
    FName := Copy(Value,1,iPos-1);
    FValue := Copy(Value,iPos+1,High(Integer));
  end else
  begin
    FName := Value;
    FValue := '';
  end;
end;

{ TTagItem }

function TTagItem.AddParam: TParamItem;
begin
  Result := TParamItem.Create;
  FParams.Add(Result);
end;

procedure TTagItem.ClearParams;
var
  i : integer;
begin
  for i := 0 to FParams.Count - 1 do
    TObject(FParams[i]).Free;
  FParams.Clear;
end;

constructor TTagItem.Create;
begin
  inherited Create;
  FParams := TList.Create;
end;

procedure TTagItem.DeleteParam(idx: integer);
begin
  Params[idx].Free;
  FParams.Delete(idx);
end;

destructor TTagItem.Destroy;
begin
  ClearParams;
  FParams.Free;
  inherited Destroy;
end;

function TTagItem.FindParamValue(const Name : string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FParams.Count - 1 do
    if CompareText(Params[i].Name, Name) = 0 then
    begin
      Result := Params[i].Value;
      break;
    end;
end;

function TTagItem.GetParam(idx: integer): TParamItem;
begin
  Result := TParamItem(FParams[idx]);
end;

function TTagItem.GetParamCount: integer;
begin
  Result := FParams.Count;
end;

function TTagItem.HasParam(const Name: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FParams.Count - 1 do
    if CompareText(Params[i].Name, Name) = 0 then
    begin
      Result := True;
      break;
    end;
end;

procedure TTagItem.ParseTag;
  function NextParam(var Caret: integer): string;
  var
    iLen : integer;
  begin
    Result := '';
    iLen := Length(FText);
    while Caret < iLen do
      if (FText[Caret] = ' ') then
        Inc(Caret)
      else
        break;

    while (Caret <= iLen) and (FText[Caret] <> '=') and (FText[Caret] <> ' ') do
    begin
      Result := Result + Copy(FText, Caret, 1);
      Inc(Caret);
    end;

    if (Caret < iLen) and (FText[Caret] = '=') then
      if FText[Caret + 1] = '"' then
      begin
        Result := Result + Copy(FText, Caret, 3);
        Caret := Caret + 3;
        if FText[Caret - 1] <> '"' then
          repeat
            Result := Result + FText[Caret];
            Inc(Caret);
          until (Caret > iLen) or (FText[Caret - 1] = '"');
      end else
        while (Caret <= iLen) and (FText[Caret] <> ' ') do
        begin
          Result := Result + FText[Caret];
          Inc(Caret);
        end;
  end;

  procedure ParseParam(var Str: string; out AName, AValue: string);
    procedure StripQuotes(var Str: string);
    var
      iPos: integer;
    begin
      repeat
        iPos := FastCharPos(Str,'"',1);
        if iPos <> 0 then Delete(Str, iPos, 1);
      until iPos = 0;
    end;
  var
    pc: PChar;
  begin
    Str := Str + #0;
    pc := @Str[1];
    AName := '';
    AValue := '';
    while (pc^ <> '=') and (pc^ <> #0) do
    begin
      AName := AName + pc^;
      Inc(pc);
    end;
    if pc^ = '=' then
      Inc(pc);
    while pc^ <> #0 do
    begin
      AValue := AValue + pc^;
      StripQuotes(AValue);
      Inc(pc);
    end;
    Delete(Str, Length(Str), 1);
  end;
var
  i, iLen: integer;
  NextWord, NameOut, ValueOut, ParamStr: string;
  pi: TParamItem;
begin
  ClearParams;
  i := 1;
  FName := UpperCase(NextParam(i));
  iLen := Length(FText);

  if Length(FName) >= 3 then
    if CompareStr(Copy(FName, 1, 3), '!--') = 0 then
    begin
      i := 4;
      while (FText[i] = ' ') do
        Inc(i);
      pi := AddParam;
      ParamStr := Copy(FText, i, iLen - i);
      while (ParamStr[Length(ParamStr)] = ' ') or
            (ParamStr[Length(ParamStr)] = '-') do
        Delete(ParamStr, Length(ParamStr), 1);
      pi.Text := ParamStr;
      Exit;
    end;

  while i <= ilen do
  begin
    NextWord := NextParam(i);
    if NextWord = '' then
       break
    else
    begin
      pi := AddParam;
      ParseParam(NextWord, NameOut, ValueOut);
      pi.Text := NextWord;
      pi.Name := NameOut;
      pi.Value := ValueOut;
    end;
  end;
end;

procedure TTagItem.SetText(const Value: string);
begin
  FText := Value;
  ParseTag;
end;

{ TTagParser }

constructor TTagParser.Create;
begin
  inherited Create;
  FFileData := TStringList.Create;
  FTags := TList.Create;
end;

procedure TTagParser.Delete(idx: integer);
var
  o : TObject;
begin
  o := FTags[idx];
  o.Free;
  FTags.Delete(idx);
end;

destructor TTagParser.Destroy;
var
  i: integer;
begin
  FFileData.Free;
  for i := 0 to FTags.Count - 1 do
    TTagItem(FTags[i]).Free;
  FTags.Free;
  inherited Destroy;
end;

function TTagParser.GetCount: integer;
begin
  Result := FTags.Count;
end;

function TTagParser.GetData(idx: integer): TObject;
begin
  Result := TObject(FTags[idx]);
end;

function TTagParser.GetTag(idx: integer): TTagItem;
begin
  if TObject(FTags[idx]) is TTagItem then
    Result := TTagItem(FTags[idx])
  else
    Result := nil;
end;

function TTagParser.GetText(idx: integer): TStringList;
begin
  if TObject(FTags[idx]) is TStringList then
    Result := TStringList(FTags[idx])
  else
    Result := nil;
end;

procedure TTagParser.SetFileData(const Value: TStringList);
begin
  FFileData.Assign(Value);
end;

procedure TTagParser.SetData(idx: integer; const Value: TObject);
begin
  FTags[idx] := Value;
end;

procedure TTagParser.Parse;
var
  oCurrent : TObject;

  procedure OpenTag(ATag: boolean);
  begin
    if ATag then
      oCurrent := TTagItem.Create
    else
      oCurrent := TStringList.Create;
    FTags.Add(oCurrent);
  end;

var
  pc: PChar;
  bInTag, bInStr: boolean;
  cStr : Char;
  i: Cardinal;
  s : string;
begin
  if FileData.Text='' then
    exit;
  oCurrent := nil;
  bInTag := false;
  bInStr := False;
  cStr := #0;
  pc := @FileData.Text[1];

  for i := 1 to Length(FileData.Text) do
  begin
    if (not bInStr) and (pc^ = '<') then
    begin
      if not bInTag then
      begin
        if (s <> '') and (oCurrent is TStringList) then
        begin
          TStringList(oCurrent).Add(s);
          s := '';
        end;
        OpenTag(true);
        bInTag := true;
      end;
    end else
      if (not bInStr) and (pc^ = '>') then
      begin
        if (s <> '') and (oCurrent is TTagItem) then
        begin
          TTagItem(oCurrent).Text := s;
          s := '';
        end;
        oCurrent := nil;
        bInTag := false;
      end else
        if (pc^ in ['''','"']) and bInTag then
        begin
          if bInStr and (pc^=cStr) then
          begin
            bInStr := False;
            cStr := #0;
            s := s+pc^;
          end else
          begin
            if not bInStr then
            begin
              bInStr := True;
              cStr := pc^;
            end;
            s := s+pc^;
          end;
        end else
          if (not Assigned(oCurrent)) and ((Ord(pc^) >= 21) or (Ord(pc^) in [13,10]))  and (bInTag = false) then
          begin
            OpenTag(false);
            if (Ord(pc^) > 33) or (Ord(pc^) in [13,10]) then
              s := pc^;
          end else
            if ((Ord(pc^) >= 32) or (Ord(pc^) in [13,10])) and (Assigned(oCurrent)) then
              if bInTag then
                s := s + pc^
              else
                s := s+pc^;
    Inc(pc);
  end;
  if (s <> '') and (oCurrent is TTagItem) then
  begin
    TTagItem(oCurrent).Text := s;
    s := '';
  end;
  if (s <> '') and (oCurrent is TStringList) then
  begin
    TStringList(oCurrent).Add(s);
    s := '';
  end;
  oCurrent := nil;
end;

function TTagParser.AddTag: TTagitem;
begin
  Result := TTagItem.Create;
  FTags.Add(Result);
end;

function TTagParser.AddText: TStringList;
begin
  Result := TStringList.Create;
  FTags.Add(Result);
end;

procedure TTagParser.Clear;
var
  i : integer;
  o : TObject;
begin
  for i := 0 to FTags.Count - 1 do
  begin
    o := FTags[i];
    o.Free;
  end;
  FTags.Clear;
end;

procedure TTagParser.Combine;
var
  i : integer;
  s, sFile : string;
begin
  FFileData.Clear;
  for i := 0 to FTags.Count - 1 do
    if TObject(FTags[i]) is TTagItem then
    begin
      sFile := sFile + TTagItem(FTags[i]).Text;
    end else
      if TObject(FTags[i]) is TStringList then
      begin
        s := TStringList(FTags[i]).Text;
        sFile := sFile + Copy(s,1,length(s)-2);
      end;
  FFileData.Text := sFile;
end;

end.
