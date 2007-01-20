unit uGlobal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls;

type
  TFindGlobals = class(TThread)
  private
    FHTML : TStringList;
    FVars : TStringList;
  protected
    procedure Execute; override;
  public
    constructor Create(HTML : TStrings); virtual;
    destructor Destroy; override;
    procedure UpdateGUI;  
  end;

  TfrmGlobals = class(TForm)
    lvGlobals: TListView;
    procedure lvGlobalsDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    Values : TStringList;
  end;

var
  frmGlobals: TfrmGlobals;

implementation

uses IniFiles, uMainForm;

{$R *.dfm}

procedure TfrmGlobals.FormCreate(Sender: TObject);
begin
  Values := TStringList.Create;
end;

procedure TfrmGlobals.FormDestroy(Sender: TObject);
begin
  Values.Free;
end;

procedure TfrmGlobals.FormResize(Sender: TObject);
begin
  lvGlobals.Columns[0].Width := Width div 2;
  lvGlobals.Columns[1].Width := Width-Width div 2;
end;

procedure TfrmGlobals.lvGlobalsDblClick(Sender: TObject);
var
  ini : TIniFile;
  sVal : string;
begin
  if lvGlobals.Selected = nil then
    exit;
  sVal := lvGlobals.Selected.SubItems[0];
  if InputQuery('Global Replacer Value','Enter a Value for '+lvGlobals.Selected.Caption+':',sVal) then
  begin
    lvGlobals.Selected.Subitems[0] := sVal;

    ini := TIniFile.Create(INIFilename);
    try
      ini.WriteString('ReplacerHistory',lvGlobals.Selected.Caption,sVal);
    finally
      ini.Free;
    end;

    frmMain.RefreshPreview;
  end;
end;

{ TFindGlobals }

constructor TFindGlobals.Create(HTML : TStrings);
begin
  inherited Create(True);
  FHTML  := TStringList.Create;
  FVars := TStringList.Create;
  FreeOnTerminate := True;

  FHTML.Assign(HTML);
  Resume;
end;

destructor TFindGlobals.Destroy;
begin
  FVars.Free;
  FHTML.Free;
  inherited;
end;

procedure TFindGlobals.Execute;
var
  iPosO, iPosC : integer;
  sSrc : string;
begin
  sSrc := FHTML.Text;

  iPosO := Pos('{%',sSrc);
  while iPosO > 0 do
  begin
    sSrc := Copy(sSrc,iPosO,High(Integer));
    iPosC := Pos('%}',sSrc);
    FVars.Add(Copy(sSrc,1,iPosC+1));
    sSrc := Copy(sSrc,iPosC+2,High(Integer));
    iPosO := Pos('{%',sSrc);
  end;
  Synchronize(UpdateGUI);  
end;

procedure TFindGlobals.UpdateGUI;
var
  s : string;
  ini : TIniFile;
  i,y : integer;
  bFound : boolean;
  li : TListItem;
begin
  for i := frmGlobals.lvGlobals.Items.Count - 1 downto 0 do
  begin
    if FVars.IndexOf(frmGlobals.lvGlobals.Items[i].Caption) < 0 then
      frmGlobals.lvGlobals.Items[i].Free;
  end;

  for i := 0 to FVars.Count - 1 do
  begin
    bFound := False;
    for y := 0 to frmGlobals.lvGlobals.Items.Count - 1 do
    begin
      bFound := lowercase(frmGlobals.lvGlobals.Items[y].Caption) = lowercase(FVars[i]);
      if bFound then
        break;
    end;
    if not bFound then
    begin
      for y := 0 to frmMain.Reporter.Replacers.Count - 1 do
      begin
        bFound :=  '{'+lowercase(frmMain.Reporter.Replacers[y].TagName)+'}' = lowercase(FVars[i]);
        if bFound then
          break;
      end;              
    end;
    if not bFound then
    begin
      li := frmGlobals.lvGlobals.Items.Add;
      li.Caption := FVars[i];

      ini := TIniFile.Create(INIFilename);
      try
        s := ini.ReadString('ReplacerHistory',FVars[i],'');
      finally
        ini.Free;
      end;

      li.SubItems.Add(s);
      //frmGlobals.Show;
      //frmGlobals.BringToFront;
      //frmMain.miViewGlobal.Checked := frmGlobals.Visible;
    end;
  end;
end;

end.
