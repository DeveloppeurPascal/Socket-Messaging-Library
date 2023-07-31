unit fTestProject;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  uProject;

procedure TForm2.Button1Click(Sender: TObject);
var
  i, j: integer;
  PRJ1, prj2: TProject;
  s: string;
  msg: tmessage;
  fld: TMessageField;
begin
  randomize;
  PRJ1 := TProject.Create;
  try
    s := TGUID.NewGuid.ToString;
    PRJ1.Name := 'U' + s.Substring(0, random(s.length - 10) + 10) +
      random(1000).ToString;
    PRJ1.Description := '';

    i := random(30) + 1;
    while (i > 0) do
    begin
      msg := tmessage.Create(PRJ1.Messages);
      msg.MessageID := i;
      s := TGUID.NewGuid.ToString;
      msg.Name := 'M' + s.Substring(0, random(s.length - 10) + 10) +
        random(1000).ToString;
      msg.Description := '';
      msg.RegisterMessageInTheServer := random(100) > 50;
      msg.RegisterMessageInTheClient := random(100) > 50;
      PRJ1.Messages.Add(msg);

      j := random(20) + 1;
      while (j > 0) do
      begin
        fld := TMessageField.Create(msg.Fields);
        fld.Order := random(100);
        s := TGUID.NewGuid.ToString;
        fld.Name := 'P' + s.Substring(0, random(s.length - 10) + 10) +
          random(1000).ToString;
        fld.Description := '';
        // fld.DelphiFieldName
        case random(5) of
          4:
            begin
              fld.DelphiFieldType := 'Boolean';
              if random(100) > 50 then
                fld.DefaultValue := 'true'
              else
                fld.DefaultValue := 'false';
            end;
          3:
            begin
              fld.DelphiFieldType := 'Integer';
              fld.DefaultValue := random(maxint).ToString;
            end;
          2:
            begin
              fld.DelphiFieldType := 'Single';
              fld.DefaultValue := (random(maxint) / 3.1415)
                .ToString.Replace(',', '.');
            end;
          1:
            begin
              fld.DelphiFieldType := 'TDate';
              fld.DefaultValue := 'StrToDate(''' + datetostr(now) + ''')';
            end;
        else
          fld.DelphiFieldType := 'String';
          fld.DefaultValue := TGUID.NewGuid.ToString.QuotedString;
        end;
        msg.Fields.Add(fld);
        dec(j);
      end;

      dec(i);
    end;

    Memo1.Text := PRJ1.AsDelphi;

    prj2 := TProject.Create;
    try
      prj2.AsJSON := PRJ1.AsJSON;
      assert(PRJ1.AsDelphi = prj2.AsDelphi, 'Format error for AsJSON');
    finally
      prj2.Free;
    end;

    prj2 := TProject.Create;
    try
      prj2.Asstring := PRJ1.Asstring;
      assert(PRJ1.AsDelphi = prj2.AsDelphi, 'Format error for AsString');
    finally
      prj2.Free;
    end;
  finally
    PRJ1.Free;
  end;
end;

procedure TForm2.Memo1Click(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
  Memo1.ResetSelection;
  showmessage('Ctrl+C done');
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
