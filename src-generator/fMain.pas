unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Olf.FMX.AboutDialog, FMX.Menus;

type
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
    procedure FormCreate(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure OlfAboutDialog1URLClick(const AURL: string);
    procedure mnuOptionsClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses u_urlOpen;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Window title
  caption := OlfAboutDialog1.Titre + ' ' + OlfAboutDialog1.VersionNumero;
{$IFDEF DEBUG}
  caption := '[DEBUG] ' + caption;
{$ENDIF}

  // Menu
{$IF Defined(MACOS) and not Defined(IOS)}
  mnuAbout.Parent := mnuMacOS;
  mnuOptions.Parent := mnuMacOS;
  mnuQuit.Visible := false;
{$ENDIF}
  mnuOptions.free;
  // TODO : show the option when an option dialog box is available
  mnuMacOS.Visible := (mnuMacOS.ItemsCount > 0);
  mnuTools.Visible := (mnuTools.ItemsCount > 0);
  mnuHelp.Visible := (mnuHelp.ItemsCount > 0);
end;

procedure TForm1.mnuAboutClick(Sender: TObject);
begin
  OlfAboutDialog1.Execute;
end;

procedure TForm1.mnuOptionsClick(Sender: TObject);
begin
  ShowMessage('No settings for now.');
end;

procedure TForm1.mnuQuitClick(Sender: TObject);
begin
  close;
end;

procedure TForm1.OlfAboutDialog1URLClick(const AURL: string);
begin
  url_Open_In_Browser(AURL);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

finalization

end.
