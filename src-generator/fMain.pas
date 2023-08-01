unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  Olf.FMX.AboutDialog,
  FMX.Menus,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  uProject,
  FMX.TabControl,
  FMX.TreeView,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit;

type
{$SCOPEDENUMS ON}
  TSMGScreen = (None, Home, Project);

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    OlfAboutDialog1: TOlfAboutDialog;
    mnuMacOS: TMenuItem;
    mnuFile: TMenuItem;
    mnuNewFile: TMenuItem;
    mnuOpenFile: TMenuItem;
    mnuSave: TMenuItem;
    mnuClose: TMenuItem;
    mnuTools: TMenuItem;
    mnuOptions: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuQuit: TMenuItem;
    lButtons: TLayout;
    btnNewProjectFile: TButton;
    btnOpenProjectFile: TButton;
    lHomeScreen: TLayout;
    lProjectScreen: TLayout;
    OpenProjectDialog: TOpenDialog;
    SaveProjectDialog: TSaveDialog;
    tvProject: TTreeView;
    Splitter1: TSplitter;
    tcProject: TTabControl;
    tiProjectEdit: TTabItem;
    tiMessageEdit: TTabItem;
    tiFieldEdit: TTabItem;
    VertScrollBox1: TVertScrollBox;
    VertScrollBox2: TVertScrollBox;
    VertScrollBox3: TVertScrollBox;
    lblProjectDescription: TLabel;
    edtProjectName: TEdit;
    lblProjectName: TLabel;
    lblProjectDelphiUnitName: TLabel;
    edtProjectDelphiUnitName: TEdit;
    edtProjectDescription: TMemo;
    GridPanelLayout1: TGridPanelLayout;
    btnProjectSave: TButton;
    btnProjectCancel: TButton;
    mnuProject: TMenuItem;
    mnuDelphiExport: TMenuItem;
    ExportSaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure OlfAboutDialog1URLClick(const AURL: string);
    procedure mnuOptionsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuNewFileClick(Sender: TObject);
    procedure mnuOpenFileClick(Sender: TObject);
    procedure mnuCloseClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvProjectChange(Sender: TObject);
    procedure edtProjectNameChange(Sender: TObject);
    procedure btnProjectCancelClick(Sender: TObject);
    procedure btnProjectSaveClick(Sender: TObject);
    procedure mnuDelphiExportClick(Sender: TObject);
  private
    FOpenedProject: TProject;
    FCurrentScreen: TSMGScreen;
    procedure SetOpenedProject(const Value: TProject);
    procedure SetCurrentScreen(const Value: TSMGScreen);
  protected
    procedure InitHomeScreen;
    procedure InitProjectScreen;
    procedure UpdateButtonsAndMenus;
    procedure RefreshFormCaption;
    procedure InitEditProjectTab;
    procedure InitEditMessageTab;
    procedure InitEditFieldTab;
  public
    property OpenedProject: TProject read FOpenedProject write SetOpenedProject;
    property CurrentScreen: TSMGScreen read FCurrentScreen
      write SetCurrentScreen;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  FMX.DialogService,
  System.IOUtils,
  u_urlOpen;

procedure TForm1.btnProjectCancelClick(Sender: TObject);
begin
  InitEditProjectTab;
end;

procedure TForm1.btnProjectSaveClick(Sender: TObject);
begin
  edtProjectName.Text := edtProjectName.Text.trim;
  if edtProjectName.Text.IsEmpty then
  begin
    edtProjectName.SetFocus;
    raise Exception.Create('Your project needs a name !');
  end;
  OpenedProject.Name := edtProjectName.Text;
  (tvProject.tagobject as ttreeviewitem).Text := OpenedProject.Name;
  RefreshFormCaption;

  OpenedProject.Description := edtProjectDescription.Text;

  edtProjectDelphiUnitName.Text := edtProjectDelphiUnitName.Text.trim;
  if edtProjectDelphiUnitName.Text.IsEmpty then
    OpenedProject.DelphiUnitName := ''
  else
    OpenedProject.DelphiUnitName :=
      ToDelphiConst(edtProjectDelphiUnitName.Text);
end;

procedure TForm1.edtProjectNameChange(Sender: TObject);
begin
  edtProjectDelphiUnitName.TextPrompt := OpenedProject.DefaultDelphiUnitName
    (edtProjectName.Text);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  mnuCloseClick(Sender);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FOpenedProject := nil;
  FCurrentScreen := TSMGScreen.None;
  tcProject.TabPosition := TTabPosition.None;

  // Window title
  RefreshFormCaption;

  // Menu
{$IF Defined(MACOS) and not Defined(IOS)}
  mnuAbout.Parent := mnuMacOS;
  mnuOptions.Parent := mnuMacOS;
  mnuQuit.Visible := false;
{$ENDIF}
  mnuOptions.free;
  // TODO : show the "tools/options" menu when an option dialog box will be available
  mnuMacOS.Visible := (mnuMacOS.ItemsCount > 0);
  mnuTools.Visible := (mnuTools.ItemsCount > 0);
  mnuHelp.Visible := (mnuHelp.ItemsCount > 0);

  CurrentScreen := TSMGScreen.Home;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if assigned(OpenedProject) then
    freeandnil(OpenedProject);
end;

procedure TForm1.InitEditFieldTab;
begin
  // TODO : à compléter
end;

procedure TForm1.InitEditMessageTab;
begin
  // TODO : à compléter
end;

procedure TForm1.InitEditProjectTab;
begin
  edtProjectName.Text := OpenedProject.Name;
  edtProjectDescription.Text := OpenedProject.Description;
  if (OpenedProject.DelphiUnitName = OpenedProject.DefaultDelphiUnitName) then
    edtProjectDelphiUnitName.Text := ''
  else
    edtProjectDelphiUnitName.Text := OpenedProject.DelphiUnitName;

  tthread.ForceQueue(nil,
    procedure
    begin
      edtProjectName.SetFocus;
    end);
end;

procedure TForm1.InitHomeScreen;
begin
  RefreshFormCaption;
end;

procedure TForm1.InitProjectScreen;
var
  msg: tmessage;
  fld: TMessageField;
  ProjectItem, MessageItem, FieldItem: ttreeviewitem;
begin
  tvProject.Clear;
  tvProject.tagobject := nil;

  ProjectItem := ttreeviewitem.Create(tvProject);
  ProjectItem.Parent := tvProject;
  ProjectItem.Text := OpenedProject.Name;
  ProjectItem.tagobject := OpenedProject;
  ProjectItem.Tag := 0;

  for msg in OpenedProject.Messages do
  begin
    MessageItem := ttreeviewitem.Create(tvProject);
    MessageItem.Parent := ProjectItem;
    MessageItem.Text := msg.Name;
    MessageItem.tagobject := msg;
    MessageItem.Tag := 1;
    for fld in msg.Fields do
    begin
      FieldItem := ttreeviewitem.Create(tvProject);
      FieldItem.Parent := MessageItem;
      FieldItem.Text := fld.Name;
      FieldItem.tagobject := fld;
      FieldItem.Tag := 2;
    end;
  end;

  tvProject.Selected := ProjectItem;

  RefreshFormCaption;
end;

procedure TForm1.mnuAboutClick(Sender: TObject);
begin
  OlfAboutDialog1.Execute;
end;

procedure TForm1.mnuCloseClick(Sender: TObject);
begin
  if not assigned(OpenedProject) then
    exit;

  if OpenedProject.HasChanged then
    TDialogService.MessageDialog
      ('Current project has been changed. Do you want to save it ?',
      tmsgdlgtype.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
      TMsgDlgBtn.mbYes, 0,
      procedure(Const AModalResult: TModalResult)
      begin
        if AModalResult = mryes then
          mnuSaveClick(Sender);
        freeandnil(OpenedProject);
        CurrentScreen := TSMGScreen.Home;
      end)
  else
  begin
    freeandnil(OpenedProject);
    CurrentScreen := TSMGScreen.Home;
  end;
end;

procedure TForm1.mnuDelphiExportClick(Sender: TObject);
begin
  ExportSaveDialog.DefaultExt := 'pas';
  ExportSaveDialog.Filter := 'Pascal file|*.pas';

  if ExportSaveDialog.InitialDir.IsEmpty then
    ExportSaveDialog.InitialDir := tpath.GetDirectoryName
      (OpenProjectDialog.FileName);
  // TODO : restore previous "exportsavedialog" from settings or the project settings

  ExportSaveDialog.FileName := tpath.Combine(ExportSaveDialog.InitialDir,
    OpenedProject.DelphiUnitName + '.' + ExportSaveDialog.DefaultExt);

  if ExportSaveDialog.Execute and (length(trim(ExportSaveDialog.FileName)) > 0)
    and (tpath.GetExtension(ExportSaveDialog.FileName) = '.' +
    ExportSaveDialog.DefaultExt) then
  begin
    tfile.WriteAllText(ExportSaveDialog.FileName, OpenedProject.AsDelphi,
      tencoding.UTF8);
    ShowMessage('Project exported.');
  end;
end;

procedure TForm1.mnuNewFileClick(Sender: TObject);
begin
  mnuCloseClick(Sender);

  OpenedProject := TProject.Create;
  OpenedProject.Name := 'new socket messaging project';
  CurrentScreen := TSMGScreen.Project;
end;

procedure TForm1.mnuOpenFileClick(Sender: TObject);
begin
  mnuCloseClick(Sender);

  if OpenProjectDialog.InitialDir.IsEmpty then
    OpenProjectDialog.InitialDir := tpath.getdocumentspath;
  // TODO : restore previous "initialdir" from settings

  if OpenProjectDialog.Execute and (length(trim(OpenProjectDialog.FileName)) >
    0) and (tpath.GetExtension(OpenProjectDialog.FileName) = '.' +
    OpenProjectDialog.DefaultExt) and tfile.Exists(OpenProjectDialog.FileName)
  then
  begin
    OpenedProject := TProject.Create;
    OpenedProject.LoadFromFile(OpenProjectDialog.FileName);
    CurrentScreen := TSMGScreen.Project;
  end;
end;

procedure TForm1.mnuOptionsClick(Sender: TObject);
begin
  ShowMessage('No settings for now.');
  // TODO : à compléter
end;

procedure TForm1.mnuQuitClick(Sender: TObject);
begin
  close;
end;

procedure TForm1.mnuSaveClick(Sender: TObject);
begin
  if not assigned(OpenedProject) then
    exit;

  if not OpenedProject.FileName.IsEmpty then
  begin
    OpenedProject.SaveToFile;
    exit;
  end;

  if SaveProjectDialog.InitialDir.IsEmpty then
    SaveProjectDialog.InitialDir := tpath.getdocumentspath;
  // TODO : restore previous "savedialog" from settings

  if SaveProjectDialog.Execute and (length(trim(SaveProjectDialog.FileName)) >
    0) and (tpath.GetExtension(SaveProjectDialog.FileName) = '.' +
    SaveProjectDialog.DefaultExt) then
  begin
    OpenedProject.SaveToFile(SaveProjectDialog.FileName);
    RefreshFormCaption;
  end;
end;

procedure TForm1.OlfAboutDialog1URLClick(const AURL: string);
begin
  url_Open_In_Browser(AURL);
end;

procedure TForm1.RefreshFormCaption;
begin
  caption := OlfAboutDialog1.Titre + ' ' + OlfAboutDialog1.VersionNumero;
{$IFDEF DEBUG}
  caption := '[DEBUG] ' + caption;
{$ENDIF}
  if assigned(OpenedProject) then
  begin
    caption := caption + ' - ' + OpenedProject.Name;
    if OpenedProject.HasChanged then
      caption := caption + '*';
  end;
end;

procedure TForm1.SetCurrentScreen(const Value: TSMGScreen);
begin
  if FCurrentScreen = Value then
    exit;

  FCurrentScreen := Value;

  case FCurrentScreen of
    TSMGScreen.Home:
      InitHomeScreen;
    TSMGScreen.Project:
      InitProjectScreen;
    TSMGScreen.None:
      ;
  else
    raise Exception.Create('Can''t show screen ' + ord(FCurrentScreen)
      .tostring + ' !');
  end;

  lHomeScreen.Visible := (FCurrentScreen = TSMGScreen.Home);
  lProjectScreen.Visible := (FCurrentScreen = TSMGScreen.Project);

  UpdateButtonsAndMenus;
end;

procedure TForm1.SetOpenedProject(const Value: TProject);
begin
  FOpenedProject := Value;
end;

procedure TForm1.tvProjectChange(Sender: TObject);
begin
  if assigned(tvProject.tagobject) then
  begin
    // TODO : check if something has changed nd ask for a SAVE or CANCEL operation on it
  end;

  tvProject.tagobject := tvProject.Selected;
  if assigned(tvProject.Selected) then
  begin
    case tvProject.Selected.Tag of
      0:
        begin
          InitEditProjectTab;
          tcProject.ActiveTab := tiProjectEdit;
        end;
      1:
        begin
          InitEditMessageTab;
          tcProject.ActiveTab := tiMessageEdit;
        end;
      2:
        begin
          InitEditFieldTab;
          tcProject.ActiveTab := tiFieldEdit;
        end;
    end;
    tcProject.Visible := true;
  end
  else
    tcProject.Visible := false;
end;

procedure TForm1.UpdateButtonsAndMenus;
begin
  mnuSave.Enabled := assigned(OpenedProject);
  mnuClose.Enabled := assigned(OpenedProject);
  mnuProject.Enabled := assigned(OpenedProject);
  mnuDelphiExport.Enabled := assigned(OpenedProject);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
TDialogService.PreferredMode := TDialogService.TPreferredMode.Sync;

finalization

end.
