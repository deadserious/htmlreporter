unit ArcHRScript;

interface

uses Classes, SysUtils, Types, ArcHTMLReporter, ArcTagParser;

type
  TArcHRScriptError = procedure(ASender : TObject; Error : string; Continue : boolean) of object;
  THTMLReportScriptBase = class(TComponent)
  private
    FHTMLReporter : THTMLReporter;
    FHandlerID : integer;
    FScriptTag: string;
    FDataReplaced: TDataReplaced;
    FOnScriptError: TArcHRScriptError;
    procedure SetDataReplaced(const Value: TDataReplaced);
    procedure SetScriptTag(const Value: string);
  protected
    function ReportError(Error : string) : boolean; virtual;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    procedure SetHTMLReporter(const Value: THTMLReporter); virtual;
    procedure InitHandler(ASender : TObject); virtual;
    procedure TagHandler(ASender : TObject; ATag : TTagItem; var AReplaceText : string; var IsTag : boolean); virtual;
  published
    property HTMLReporter : THTMLReporter read FHTMLReporter write SetHTMLReporter;
    property ScriptTag : string read FScriptTag write SetScriptTag;
    property DataReplaced : TDataReplaced read FDataReplaced write SetDataReplaced default drBoth;
    property OnScriptError : TArcHRScriptError read FOnScriptError write FOnScriptError;
  end;

implementation

{ TArcHRScriptBase }

constructor THTMLReportScriptBase.Create(AOwner: TComponent);
begin
  inherited;
  FDataReplaced := drBoth;
  FScriptTag := 'datascript';
end;

destructor THTMLReportScriptBase.Destroy;
begin
  inherited;
end;

procedure THTMLReportScriptBase.InitHandler(ASender: TObject);
begin
 // To be Overridden
end;

function THTMLReportScriptBase.ReportError(Error: string) : boolean;
begin
  Result := False;
  if Assigned(FOnScriptError) then
    FOnScriptError(Self, Error, Result);
end;

procedure THTMLReportScriptBase.SetDataReplaced(const Value: TDataReplaced);
var
  rpt : THTMLReporter;
begin
  FDataReplaced := Value;
  rpt := FHTMLReporter;
  HTMLReporter := nil;
  HTMLReporter := rpt;
end;

procedure THTMLReportScriptBase.SetHTMLReporter(const Value: THTMLReporter);
var
  i: Integer;
begin
  if FHTMLReporter = Value then
    exit;
  if (FHTMLReporter <> nil) then
  begin
    FHTMLReporter.UnregisterHandler(Integer(FHandlerID));
    FHTMLReporter.RemoveFreeNotification(Self);
  end;

  FHTMLReporter := Value;

  if FHTMLReporter <> nil then
  begin
    FHTMLReporter.FreeNotification(Self);
    FHandlerID := FHTMLReporter.RegisterHandler(FScriptTag, TagHandler, InitHandler, FDataReplaced);
  end;
end;

procedure THTMLReportScriptBase.SetScriptTag(const Value: string);
var
  rpt : THTMLReporter;
begin
  FScriptTag := Value;

  rpt := FHTMLReporter;
  HTMLReporter := nil;
  HTMLReporter := rpt;
end;

procedure THTMLReportScriptBase.TagHandler(ASender: TObject; ATag: TTagItem;
  var AReplaceText: string; var IsTag: boolean);
begin
  // To be overridden;
end;

end.
