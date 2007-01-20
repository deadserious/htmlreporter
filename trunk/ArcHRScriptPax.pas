unit ArcHRScriptPax;

interface

uses Classes, SysUtils, Types, ArcHTMLReporter, ArcTagParser, ArcHRScript,
  PaxScripter, PaxJavaScript, PaxPascal, PaxC, PaxBasic, IMP_Classes,
  IMP_StrUtils, IMP_DateUtils, IMP_IniFiles, IMP_SysUtils, IMP_Math, IMP_Registry;

type
  TPaxLang = (pxPascal, pxC, pxJavascript, pxBasic);
  TPaxLangs = set of TPaxLang;
  THTMLReportPaxScript = class(THTMLReportScriptBase)
  private
    FLanguages: TPaxLangs;
    FScripter : TPaxScripter;
    FPaxBasic : TPaxBasic;
    FPaxC : TPaxC;
    FPaxJavascript : TPaxJavaScript;
    FPaxPascal : TPaxPascal;
    FModules : TStringList;
    OutText : string;
  protected
    procedure ClearModules; virtual;
    procedure ScriptPrint(Sender: TPaxScripter; const S: String);
  public
    procedure InitHandler(ASender: TObject); override;
    procedure OnShowError(Sender : TPaxScripter);
    procedure TagHandler(ASender: TObject; ATag: TTagItem; var AReplaceText: string; var IsTag: Boolean); override;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Languages : TPaxLangs read FLanguages write FLanguages default [pxPascal, pxC, pxJavascript, pxBasic];
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Arcana', [THTMLReportPaxScript]);
end;

{ THTMLReportPaxScript }

procedure THTMLReportPaxScript.ClearModules;
var
  i: Integer;
begin
  for i := 0 to FModules.Count - 1 do
    TStringList(FModules.Objects[i]).Free;
  FModules.Clear;
end;

constructor THTMLReportPaxScript.Create(AOwner: TComponent);
begin
  inherited;
  FModules := TStringList.Create;
  FLanguages := [pxPascal, pxC, pxJavascript, pxBasic];
  FScripter := TPaxScripter.Create(Self);
  FScripter.OnPrint := ScriptPrint;
  FScripter.OnShowError := OnShowError;
  FPaxBasic := TPaxBasic.Create(Self);
  FPaxC := TPaxC.Create(Self);
  FPaxJavascript := TPaxJavaScript.Create(Self);
  FPaxPascal := TPaxPascal.Create(Self);
end;

destructor THTMLReportPaxScript.Destroy;
begin
  ClearModules;
  FModules.Free;
  inherited;
end;

procedure THTMLReportPaxScript.InitHandler(ASender: TObject);
begin
  inherited;
  ClearModules;
  FScripter.ResetScripter;
end;

procedure THTMLReportPaxScript.OnShowError(Sender: TPaxScripter);
var
  sMsg : string;
begin
  sMsg := 'Script Compilation Error'#13#10#13#10+
          'Module: '+FScripter.ErrorModuleName+#13#10#13#10+
          'Line: '+IntToStr(FScripter.ErrorLine)+'  Char: '+IntToStr(FScripter.ErrorPos)+#13#10#13#10+
          'Message: '+FScripter.ErrorDescription;
  if not ReportError(sMsg) then
    raise EAbort.Create(FScripter.ErrorDescription);
end;

procedure THTMLReportPaxScript.ScriptPrint(Sender: TPaxScripter; const S: String);
begin
  OutText := s;
end;

procedure THTMLReportPaxScript.TagHandler(ASender: TObject; ATag: TTagItem;
  var AReplaceText: string; var IsTag: Boolean);
var
  sLang, sModule, sScript : string;
  idx : integer;
  i: Integer;
begin
  inherited;
  OutText := '';
  sLang := ATag.FindParamValue('language');
  sModule := ATag.FindParamValue('module');
  sScript := ATag.FindParamValue('src');

  if sLang='' then
    sLang := 'javascript';
  if sModule ='' then
    sModule := 'main'+sLang;

  idx := FModules.IndexOfName(sModule);
  if idx < 0 then
  begin
    idx := FModules.AddObject(sModule+'='+sLang, TStringList.Create);
    TStringList(FModules.Objects[idx]).Add('uses SysUtils, Classes, StrUtils, DateUtils, Math, IniFiles, Registry;');
  end;
  TStringList(FModules.Objects[idx]).Add(sScript);

  //sModule := sModule+IntToStr(Integer(ATag));

  FScripter.ResetScripter;
  FScripter.RegisterVariable('Text','string',@OutText);
  for i := 0 to FModules.Count - 1 do
  begin
    FScripter.AddModule(FModules.Names[i],'pax'+FModules.ValueFromIndex[i]);
    FScripter.AddCode(FModules.Names[i],TStringList(FModules.Objects[i]).Text);
  end;
  //FScripter.Compile;
  FScripter.Run;
  AReplaceText := OutText;
  IsTag := False;
end;

end.
