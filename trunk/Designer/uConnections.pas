unit uConnections;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ADODB;

type
  TfrmConnections = class(TForm)
    lbConnections: TListBox;
    edtName: TEdit;
    txtString: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    btnAdd: TButton;
    btnReplace: TButton;
    btnDelete: TButton;
    btnBuild: TButton;
    procedure edtNameChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbConnectionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    ConStrings : TStringList;
  public
    procedure CheckEnabledStates;
  end;

var
  frmConnections: TfrmConnections;

implementation

uses uMainForm, IniFiles;

{$R *.dfm}

procedure TfrmConnections.btnAddClick(Sender: TObject);
begin
  ConStrings.Add(txtString.Text);
  lbConnections.Items.AddObject(edtName.Text,TObject(ConStrings.Count-1));
end;

procedure TfrmConnections.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmConnections.btnDeleteClick(Sender: TObject);
begin
  if lbConnections.ItemIndex > 0 then
    lbConnections.Items.Delete(lbConnections.ItemIndex);
end;

procedure TfrmConnections.btnOKClick(Sender: TObject);
var
  ini : TIniFile;
  I: Integer;
begin
  ini := TIniFile.Create(INIFilename);
  try
    ini.EraseSection('Connections');
    for I := 0 to lbConnections.Items.Count - 1 do
      ini.WriteString('Connections',lbConnections.Items[i],ConStrings[Integer(lbConnections.Items.Objects[i])]);
  finally
    ini.Free;
  end;

  ModalResult := mrOK;
end;

procedure TfrmConnections.btnReplaceClick(Sender: TObject);
begin
  if lbConnections.ItemIndex >= 0 then
  begin
    ConStrings[Integer(lbConnections.Items.Objects[lbConnections.ItemIndex])] := txtString.Text;
    lbConnections.Items[lbConnections.ItemIndex] := edtName.Text;
  end;
end;

procedure TfrmConnections.btnBuildClick(Sender: TObject);
var
  CS : WideString;
begin
   CS:= txtString.Lines.Text;
   txtString.Lines.Text := PromptDataSource(Self.Handle, CS);
end;

procedure TfrmConnections.CheckEnabledStates;
begin
  btnAdd.Enabled := (edtName.Text <> '') and (txtString.Text <> '');
  btnDelete.Enabled := (lbConnections.ItemIndex >= 0);
  btnReplace.Enabled := btnAdd.Enabled and (lbConnections.ItemIndex >= 0);
  btnBuild.Enabled := (edtName.Text <> '');
end;

procedure TfrmConnections.edtNameChange(Sender: TObject);
begin
  CheckEnabledStates;
end;

procedure TfrmConnections.FormCreate(Sender: TObject);
var
  ini : TIniFile;
  sl : TStringList;
  I: Integer;
begin
  ConStrings := TStringList.Create;
  ini := TIniFile.Create(INIFilename);
  sl := TStringList.Create;
  try
    ini.ReadSection('Connections',sl);
    for I := 0 to sl.Count - 1 do
    begin
      ConStrings.Add(ini.ReadString('Connections',sl[i],''));
      lbConnections.Items.AddObject(sl[i],TObject(i));
    end;
  finally
    sl.Free;
    ini.Free;
  end;
  lbConnections.Sorted := True;
  CheckEnabledStates;
end;

procedure TfrmConnections.FormDestroy(Sender: TObject);
begin
  ConStrings.Free;
end;

procedure TfrmConnections.lbConnectionsClick(Sender: TObject);
begin
  if lbConnections.ItemIndex >= 0 then
  begin
    edtName.Text := lbConnections.Items[lbConnections.ItemIndex];
    txtString.Text := ConStrings[Integer(lbConnections.Items.Objects[lbConnections.ItemIndex])];
  end;
  CheckEnabledStates;
end;

end.
