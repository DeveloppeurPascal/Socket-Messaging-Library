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
  uProject;

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
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
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

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  mnuCloseClick(Sender);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FOpenedProject := nil;
  FCurrentScreen := TSMGScreen.None;

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

procedure TForm1.InitHomeScreen;
begin
  RefreshFormCaption;
end;

procedure TForm1.InitProjectScreen;
begin
  // TODO : à compléter
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

  if OpenDialog1.InitialDir.IsEmpty then
    OpenDialog1.InitialDir := tpath.getdocumentspath;
  // TODO : restore previous "initialdir" from settings

  if OpenDialog1.Execute and (length(trim(OpenDialog1.FileName)) > 0) and
    (tpath.GetExtension(OpenDialog1.FileName) = '.' + OpenDialog1.DefaultExt)
    and tfile.Exists(OpenDialog1.FileName) then
  begin
    OpenedProject := TProject.Create;
    OpenedProject.LoadFromFile(OpenDialog1.FileName);
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

  if SaveDialog1.InitialDir.IsEmpty then
    SaveDialog1.InitialDir := tpath.getdocumentspath;
  // TODO : restore previous "savedialog" from settings

  if SaveDialog1.Execute and (length(trim(SaveDialog1.FileName)) > 0) and
    (tpath.GetExtension(SaveDialog1.FileName) = '.' + SaveDialog1.DefaultExt)
  then
  begin
    OpenedProject.SaveToFile(SaveDialog1.FileName);
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

  case FCurrentScreen of
    TSMGScreen.None:
      ;
    TSMGScreen.Home:
      lHomeScreen.Visible := false;
    TSMGScreen.Project:
      lProjectScreen.Visible := false;
  else
    raise exception.Create('Can''t hide screen ' + ord(FCurrentScreen)
      .tostring + ' !');
  end;

  FCurrentScreen := Value;

  case FCurrentScreen of
    TSMGScreen.Home:
      begin
        InitHomeScreen;
        lHomeScreen.Visible := true;
      end;
    TSMGScreen.Project:
      begin
        InitProjectScreen;
        lProjectScreen.Visible := true;
      end;
  else
    raise exception.Create('Can''t show screen ' + ord(FCurrentScreen)
      .tostring + ' !');
  end;

  UpdateButtonsAndMenus;
end;

procedure TForm1.SetOpenedProject(const Value: TProject);
begin
  FOpenedProject := Value;
end;

procedure TForm1.UpdateButtonsAndMenus;
begin
  mnuSave.Enabled := assigned(OpenedProject);
  mnuClose.Enabled := assigned(OpenedProject);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
TDialogService.PreferredMode := TDialogService.TPreferredMode.Sync;

finalization

end.
