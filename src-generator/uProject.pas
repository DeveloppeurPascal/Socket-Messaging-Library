unit uProject;

interface

uses
  System.JSON,
  System.Generics.Collections,
  Olf.Net.Socket.Messaging;

type
  TMessageFieldsList = class;
  TMessage = class;
  TMessagesList = class;
  TProject = class;

  TMessageField = class
  private
    FParent: TMessageFieldsList;
    FName: string;
    FDefaultValue: string;
    FDelphiFieldName: string;
    FOrder: integer;
    FDescription: string;
    FDelphiFieldType: string;
    procedure SetDefaultValue(const Value: string);
    procedure SetDelphiFieldName(const Value: string);
    procedure SetDelphiFieldType(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetName(const Value: string);
    procedure SetOrder(const Value: integer);
    procedure SetAsJSON(const Value: TJSONObject);
    function GetAsJSON: TJSONObject;
    function GetDelphiFieldName: string;
  protected
    procedure ValueChanged;
  public
    property Order: integer read FOrder write SetOrder;
    property Name: string read FName write SetName;
    property Description: string read FDescription write SetDescription;
    property DefaultValue: string read FDefaultValue write SetDefaultValue;
    property DelphiFieldName: string read GetDelphiFieldName
      write SetDelphiFieldName;
    property DelphiFieldType: string read FDelphiFieldType
      write SetDelphiFieldType;
    property AsJSON: TJSONObject read GetAsJSON write SetAsJSON;
    constructor Create(AParent: TMessageFieldsList); virtual;
    function DefaultDelphiFieldName(AName: string = ''): string;
  end;

  TMessageFieldsList = class(tobjectlist<TMessageField>)
  private
    FParent: TMessage;
    function GetAsJSON: TJSONArray;
    procedure SetAsJSON(const Value: TJSONArray);
  protected
    procedure ValueChanged;
  public
    property AsJSON: TJSONArray read GetAsJSON write SetAsJSON;
    procedure SortByOrder;
    constructor Create(AParent: TMessage); virtual;
    function GetMaxOrder: integer;
    function Add(Const Value: TMessageField): integer;
  end;

  TMessage = class
  private
    FParent: TMessagesList;
    FName: string;
    FRegisterMessageInTheClient: boolean;
    FRegisterMessageInTheServer: boolean;
    FDelphiClassName: string;
    FFields: TMessageFieldsList;
    FDescription: string;
    FMessageID: TOlfMessageId;
    procedure SetDelphiClassName(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetFields(const Value: TMessageFieldsList);
    procedure SetMessageID(const Value: TOlfMessageId);
    procedure SetName(const Value: string);
    procedure SetRegisterMessageInTheClient(const Value: boolean);
    procedure SetRegisterMessageInTheServer(const Value: boolean);
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(const Value: TJSONObject);
    function GetDelphiClassName: string;
  protected
    procedure ValueChanged;
  public
    property MessageID: TOlfMessageId read FMessageID write SetMessageID;
    property Name: string read FName write SetName;
    property Description: string read FDescription write SetDescription;
    property DelphiClassName: string read GetDelphiClassName
      write SetDelphiClassName;
    property RegisterMessageInTheServer: boolean
      read FRegisterMessageInTheServer write SetRegisterMessageInTheServer;
    property RegisterMessageInTheClient: boolean
      read FRegisterMessageInTheClient write SetRegisterMessageInTheClient;
    property Fields: TMessageFieldsList read FFields write SetFields;
    property AsJSON: TJSONObject read GetAsJSON write SetAsJSON;
    constructor Create(AParent: TMessagesList); virtual;
    destructor Destroy; override;
    function DefaultDelphiClassName(AName: string = ''): string;
  end;

  TMessagesList = class(tobjectlist<TMessage>)
  private
    FParent: TProject;
    function GetAsJSON: TJSONArray;
    procedure SetAsJSON(const Value: TJSONArray);
  protected
    procedure ValueChanged;
    function GetDelphiInterface: string;
    function GetDelphiImplementation: string;
  public
    property AsJSON: TJSONArray read GetAsJSON write SetAsJSON;
    procedure SortByMessageID;
    procedure SortByDelphiClassName;
    constructor Create(AParent: TProject);
    function GetMaxMessageID: TOlfMessageId;
    function Add(Const Value: TMessage): integer;
  end;

  TProject = class
  private
    FFileName: string;
    FMessages: TMessagesList;
    FHasChanged: boolean;
    FName: string;
    FDescription: string;
    FDelphiUnitName: string;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    procedure SetMessages(const Value: TMessagesList);
    procedure SetHasChanged(const Value: boolean);
    procedure SetDelphiUnitName(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetName(const Value: string);
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(const Value: TJSONObject);
    function GetAsDelphi: string;
    function GetDelphiUnitName: string;
  protected
    procedure ValueChanged;
  public
    property Name: string read FName write SetName;
    property Description: string read FDescription write SetDescription;
    property DelphiUnitName: string read GetDelphiUnitName
      write SetDelphiUnitName;
    property Messages: TMessagesList read FMessages write SetMessages;
    property HasChanged: boolean read FHasChanged write SetHasChanged;
    property AsString: string read GetAsString write SetAsString;
    property AsJSON: TJSONObject read GetAsJSON write SetAsJSON;
    property AsDelphi: string read GetAsDelphi;
    property FileName: string read FFileName;
    procedure SaveToFile(AFileName: string = ''; AForceWrite: boolean = true);
    procedure LoadFromFile(AFileName: string);
    constructor Create; virtual;
    destructor Destroy; override;
    function DefaultDelphiUnitName(AName: string = ''): string;
  end;

function ToDelphiConst(Texte: string; AllowDot: boolean = false): string;
function WrapTextWithPrefix(Const Prefix, Texte: string;
  Const Suffix: string = ''; Const MaxCol: integer = 80): string;

implementation

uses
{$IF Defined(FRAMEWORK_VCL)}
  VCL.Forms,
{$ELSE}
  FMX.Forms,
{$ENDIF}
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.StrUtils,
  System.Character,
  System.Generics.Defaults;

function ToDelphiConst(Texte: string; AllowDot: boolean): string;
// TODO : move this function in DeveloppeurPascal/Librairies
var
  c: char;
  i: integer;
  PremierCaractere: boolean;
begin
  Result := '';
  Texte := Texte.Trim;
  PremierCaractere := true;
  for i := 0 to Length(Texte) - 1 do
  begin
    c := Texte.Chars[i];
    if c.IsInArray(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
    begin
      if PremierCaractere then
        Result := Result + '_' + c
      else
        Result := Result + c;
    end
    else if c.tolower.IsInArray(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
      'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
      'y', 'z']) then
      Result := Result + c
    else if c.IsInArray(['@']) then
      Result := Result + '_'
    else if c.IsInArray(['#']) then
      Result := Result + '_'
    else if c.IsInArray(['£']) then
      Result := Result + '_'
    else if c.IsInArray(['€']) then
      Result := Result + 'EUR'
    else if c.IsInArray(['$']) then
      Result := Result + 'USD'
    else if c.IsInArray(['_', '-', ' ']) then
      Result := Result + '_'
    else if c.IsInArray(['à', 'â', 'ä', 'å']) then
      Result := Result + 'a'
    else if c.IsInArray(['é', 'è', 'ë', 'ê']) then
      Result := Result + 'e'
    else if c.IsInArray(['ï', 'î']) then
      Result := Result + 'i'
    else if c.IsInArray(['ô', 'ö', 'ø']) then
      Result := Result + 'o'
    else if c.IsInArray(['ü', 'û', 'ù']) then
      Result := Result + 'u'
    else if c.IsInArray(['Š']) then
      Result := Result + 'S'
    else if c.IsInArray(['ž']) then
      Result := Result + 'z'
    else if c.IsInArray(['æ']) then
      Result := Result + 'ae'
    else if c.IsInArray(['ç', 'č']) then
      Result := Result + 'c'
    else if AllowDot and c.IsInArray(['.']) then
      Result := Result + '.';
    PremierCaractere := false;
  end;
  while Result.IndexOf('__') > -1 do
    Result := ReplaceText(Result, '__', '_');
end;

function WrapTextWithPrefix(Const Prefix, Texte: string;
  Const Suffix: string = ''; Const MaxCol: integer = 80): string;
var
  sl: tstringlist;
  i: integer;
begin
  Result := '';
  sl := tstringlist.Create;
  try
    sl.Text := wraptext(Texte);
    for i := 0 to sl.Count - 1 do
    begin
      if not Result.IsEmpty then
        Result := Result + sLineBreak;
      Result := Result + Prefix + sl[i].Trim + Suffix;
    end;
  finally
    sl.Free;
  end;
end;

{ TMessageField }

constructor TMessageField.Create(AParent: TMessageFieldsList);
begin
  FParent := AParent;
  FName := '';
  FDefaultValue := '';
  FDelphiFieldName := '';
  FOrder := 0;
  FDescription := '';
  FDelphiFieldType := '';
end;

function TMessageField.GetAsJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('order', FOrder);
  Result.AddPair('name', FName);
  Result.AddPair('description', FDescription);
  Result.AddPair('delphifieldname', FDelphiFieldName);
  Result.AddPair('delphifieldtype', FDelphiFieldType);
  Result.AddPair('defaultvalue', FDefaultValue);
end;

function TMessageField.DefaultDelphiFieldName(AName: string): string;
begin
  if AName.IsEmpty then
    Result := ToDelphiConst(name)
  else
    Result := ToDelphiConst(AName);
end;

function TMessageField.GetDelphiFieldName: string;
begin
  if FDelphiFieldName.IsEmpty then
    Result := DefaultDelphiFieldName
  else
    Result := FDelphiFieldName;
end;

procedure TMessageField.SetAsJSON(const Value: TJSONObject);
begin
  if not assigned(Value) then
    exit;

  ValueChanged;
  if not Value.TryGetValue<integer>('order', FOrder) then
    FOrder := 0;
  if not Value.TryGetValue<string>('name', FName) then
    FName := '';
  if not Value.TryGetValue<string>('description', FDescription) then
    FDescription := '';
  if not Value.TryGetValue<string>('delphifieldname', FDelphiFieldName) then
    FDelphiFieldName := '';
  if not Value.TryGetValue<string>('delphifieldtype', FDelphiFieldType) then
    FDelphiFieldType := '';
  if not Value.TryGetValue<string>('defaultvalue', FDefaultValue) then
    FDefaultValue := '';
end;

procedure TMessageField.SetDefaultValue(const Value: string);
begin
  if (FDefaultValue = Value) then
    exit;
  ValueChanged;
  FDefaultValue := Value;
end;

procedure TMessageField.SetDelphiFieldName(const Value: string);
begin
  if (FDelphiFieldName = Value) then
    exit;
  ValueChanged;
  FDelphiFieldName := Value;
end;

procedure TMessageField.SetDelphiFieldType(const Value: string);
begin
  if (FDelphiFieldType = Value) then
    exit;
  ValueChanged;
  FDelphiFieldType := Value;
end;

procedure TMessageField.SetDescription(const Value: string);
begin
  if (FDescription = Value) then
    exit;
  ValueChanged;
  FDescription := Value;
end;

procedure TMessageField.SetName(const Value: string);
begin
  if (FName = Value) then
    exit;
  ValueChanged;
  FName := Value;
end;

procedure TMessageField.SetOrder(const Value: integer);
begin
  if (FOrder = Value) then
    exit;
  ValueChanged;
  FOrder := Value;
end;

procedure TMessageField.ValueChanged;
begin
  if assigned(FParent) then
    FParent.ValueChanged;
end;

{ TMessageFieldsList }

function TMessageFieldsList.Add(const Value: TMessageField): integer;
begin
  if (Value.Order < 1) then
    Value.Order := GetMaxOrder + 1;
  Result := inherited Add(Value);
end;

constructor TMessageFieldsList.Create(AParent: TMessage);
begin
  inherited Create(true);
  FParent := AParent;
end;

function TMessageFieldsList.GetAsJSON: TJSONArray;
var
  i: integer;
begin
  Result := TJSONArray.Create;
  for i := 0 to Count - 1 do
    Result.Add(items[i].AsJSON);
end;

function TMessageFieldsList.GetMaxOrder: integer;
var
  fld: TMessageField;
begin
  Result := 0;
  for fld in self do
    if Result < fld.Order then
      Result := fld.Order;
end;

procedure TMessageFieldsList.SetAsJSON(const Value: TJSONArray);
var
  jsv: tjsonvalue;
  field: TMessageField;
begin
  clear;
  if not assigned(Value) then
    exit;
  if Value.Count > 0 then
    for jsv in Value do
      if (jsv is TJSONObject) then
      begin
        field := TMessageField.Create(self);
        field.AsJSON := jsv as TJSONObject;
        Add(field);
      end;
  SortByOrder;
end;

procedure TMessageFieldsList.SortByOrder;
begin
  Sort(TComparer<TMessageField>.Construct(
    function(const a, b: TMessageField): integer
    begin
      if a.FOrder = b.FOrder then
        Result := 0
      else if a.FOrder > b.FOrder then
        Result := 1
      else
        Result := -1;
    end));
end;

procedure TMessageFieldsList.ValueChanged;
begin
  if assigned(FParent) then
    FParent.ValueChanged;
end;

{ TMessage }

constructor TMessage.Create(AParent: TMessagesList);
begin
  FParent := AParent;
  Fields := TMessageFieldsList.Create(self);
  FName := '';
  FRegisterMessageInTheClient := true;
  FRegisterMessageInTheServer := true;
  FDelphiClassName := '';
  FDescription := '';
  FMessageID := 0;
end;

destructor TMessage.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TMessage.GetAsJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('msgid', FMessageID);
  Result.AddPair('name', FName);
  Result.AddPair('description', FDescription);
  Result.AddPair('delphiclassname', FDelphiClassName);
  Result.AddPair('regclient', FRegisterMessageInTheClient);
  Result.AddPair('regserver', FRegisterMessageInTheServer);
  Fields.SortByOrder;
  Result.AddPair('fields', Fields.AsJSON);
end;

function TMessage.DefaultDelphiClassName(AName: string): string;
begin
  if AName.IsEmpty then
    Result := 'T' + ToDelphiConst(name)
  else
    Result := 'T' + ToDelphiConst(AName);
  if not Result.tolower.EndsWith('message') then
    Result := Result + 'Message';
end;

function TMessage.GetDelphiClassName: string;
begin
  if FDelphiClassName.IsEmpty then
    Result := DefaultDelphiClassName
  else
    Result := FDelphiClassName;
end;

procedure TMessage.SetAsJSON(const Value: TJSONObject);
var
  jsa: TJSONArray;
begin
  if not assigned(Value) then
    exit;

  ValueChanged;
  if not Value.TryGetValue<TOlfMessageId>('msgid', FMessageID) then
    FMessageID := 0;
  if not Value.TryGetValue<string>('name', FName) then
    FName := '';
  if not Value.TryGetValue<string>('description', FDescription) then
    FDescription := '';
  if not Value.TryGetValue<string>('delphiclassname', FDelphiClassName) then
    FDelphiClassName := '';
  if not Value.TryGetValue<boolean>('regclient', FRegisterMessageInTheClient)
  then
    FRegisterMessageInTheClient := true;
  if not Value.TryGetValue<boolean>('regserver', FRegisterMessageInTheServer)
  then
    FRegisterMessageInTheServer := true;
  if not Value.TryGetValue<TJSONArray>('fields', jsa) then
  begin
    jsa := TJSONArray.Create;
    try
      Fields.AsJSON := jsa;
    finally
      jsa.Free;
    end;
  end
  else
    Fields.AsJSON := jsa;
end;

procedure TMessage.SetDelphiClassName(const Value: string);
begin
  if (FDelphiClassName = Value) then
    exit;
  ValueChanged;
  FDelphiClassName := Value;
end;

procedure TMessage.SetDescription(const Value: string);
begin
  if (FDescription = Value) then
    exit;
  ValueChanged;
  FDescription := Value;
end;

procedure TMessage.SetFields(const Value: TMessageFieldsList);
begin
  if (FFields = Value) then
    exit;
  ValueChanged;
  FFields := Value;
end;

procedure TMessage.SetMessageID(const Value: TOlfMessageId);
begin
  if (FMessageID = Value) then
    exit;
  ValueChanged;
  FMessageID := Value;
end;

procedure TMessage.SetName(const Value: string);
begin
  if (FName = Value) then
    exit;
  ValueChanged;
  FName := Value;
end;

procedure TMessage.SetRegisterMessageInTheClient(const Value: boolean);
begin
  if (FRegisterMessageInTheClient = Value) then
    exit;
  ValueChanged;
  FRegisterMessageInTheClient := Value;
end;

procedure TMessage.SetRegisterMessageInTheServer(const Value: boolean);
begin
  if (FRegisterMessageInTheServer = Value) then
    exit;
  ValueChanged;
  FRegisterMessageInTheServer := Value;
end;

procedure TMessage.ValueChanged;
begin
  if assigned(FParent) then
    FParent.ValueChanged;
end;

{ TMessagesList }

function TMessagesList.Add(const Value: TMessage): integer;
begin
  if (Value.MessageID < 1) then
    Value.MessageID := GetMaxMessageID + 1;
  Result := inherited Add(Value);
end;

constructor TMessagesList.Create(AParent: TProject);
begin
  inherited Create(true);
  FParent := AParent;
end;

function TMessagesList.GetAsJSON: TJSONArray;
var
  i: integer;
begin
  Result := TJSONArray.Create;
  for i := 0 to Count - 1 do
    Result.Add(items[i].AsJSON);
end;

function TMessagesList.GetDelphiImplementation: string;
var
  i, j: integer;
  msg: TMessage;
  fld: TMessageField;
begin
  for i := 0 to Count - 1 do
  begin
    msg := items[i];
    Result := Result + '{ ' + msg.DelphiClassName + ' }' + sLineBreak;
    Result := Result + sLineBreak;

    Result := Result + 'constructor ' + msg.DelphiClassName + '.Create;' +
      sLineBreak;
    Result := Result + 'begin' + sLineBreak;
    Result := Result + '  inherited;' + sLineBreak;
    Result := Result + '  MessageID := ' + msg.MessageID.ToString + ';' +
      sLineBreak;
    for j := 0 to msg.Fields.Count - 1 do
    begin
      fld := msg.Fields[j];
      if (fld.DefaultValue.IsEmpty) then
      else
        Result := Result + '  F' + fld.DelphiFieldName + ' := ' +
          fld.DefaultValue + ';' + sLineBreak;
    end;
    Result := Result + 'end;' + sLineBreak;
    Result := Result + sLineBreak;

    Result := Result + 'function ' + msg.DelphiClassName +
      '.GetNewInstance: TOlfSMMessage;' + sLineBreak;
    Result := Result + 'begin' + sLineBreak;
    Result := Result + '  result := ' + msg.DelphiClassName + '.Create;' +
      sLineBreak;
    Result := Result + 'end;' + sLineBreak;
    Result := Result + sLineBreak;

    Result := Result + 'procedure ' + msg.DelphiClassName +
      '.LoadFromStream(Stream: TStream);' + sLineBreak;
    Result := Result + 'begin' + sLineBreak;
    Result := Result + '  inherited;' + sLineBreak;
    for j := 0 to msg.Fields.Count - 1 do
    begin
      fld := msg.Fields[j];
      if (fld.DelphiFieldType.tolower = 'string') then
        Result := Result + '  F' + fld.DelphiFieldName +
          ' := LoadStringFromStream(Stream);' + sLineBreak
      else
      begin
        Result := Result + '  if (Stream.read(F' + fld.DelphiFieldName +
          ', sizeof(F' + fld.DelphiFieldName + ')) <> sizeof(F' +
          fld.DelphiFieldName + ')) then' + sLineBreak;
        Result := Result + '    raise exception.Create(''Can''''t load "' +
          fld.DelphiFieldName + '" value.'');' + sLineBreak;
        // TODO : use Name instead of DelphiFieldName (or choose in the field editor)
      end;
    end;
    Result := Result + 'end;' + sLineBreak;
    Result := Result + sLineBreak;

    Result := Result + 'procedure ' + msg.DelphiClassName +
      '.SaveToStream(Stream: TStream);' + sLineBreak;
    Result := Result + 'begin' + sLineBreak;
    Result := Result + '  inherited;' + sLineBreak;
    for j := 0 to msg.Fields.Count - 1 do
    begin
      fld := msg.Fields[j];
      if (fld.DelphiFieldType.tolower = 'string') then
        Result := Result + '  SaveStringToStream(F' + fld.DelphiFieldName +
          ', Stream);' + sLineBreak
      else
      begin
        Result := Result + '  Stream.Write(F' + fld.DelphiFieldName +
          ', sizeof(F' + fld.DelphiFieldName + '));' + sLineBreak;
      end;
    end;
    Result := Result + 'end;' + sLineBreak;
    Result := Result + sLineBreak;

    for j := 0 to msg.Fields.Count - 1 do
    begin
      fld := msg.Fields[j];
      Result := Result + 'procedure ' + msg.DelphiClassName + '.Set' +
        fld.DelphiFieldName + '(const Value: ' + fld.DelphiFieldType + ');' +
        sLineBreak;
      Result := Result + 'begin' + sLineBreak;
      Result := Result + '  F' + fld.DelphiFieldName + ' := Value;' +
        sLineBreak;
      Result := Result + 'end;' + sLineBreak;
      Result := Result + sLineBreak;
    end;

  end;
end;

function TMessagesList.GetDelphiInterface: string;
var
  i, j: integer;
  msg: TMessage;
  fld: TMessageField;
begin
  for i := 0 to Count - 1 do
  begin
    msg := items[i];
    if not msg.name.IsEmpty then
    begin
      Result := Result + '  /// <summary>' + sLineBreak;
      Result := Result + WrapTextWithPrefix('  /// ', msg.name) + sLineBreak;
      Result := Result + '  /// </summary>' + sLineBreak;
    end;
    if not msg.Description.IsEmpty then
    begin
      Result := Result + '  /// <remarks>' + sLineBreak;
      Result := Result + WrapTextWithPrefix('  /// ', msg.Description) +
        sLineBreak;
      Result := Result + '  /// </remarks>' + sLineBreak;
    end;
    Result := Result + '  ' + msg.DelphiClassName + ' = class(TOlfSMMessage)' +
      sLineBreak;
    Result := Result + '  private' + sLineBreak;
    for j := 0 to msg.Fields.Count - 1 do
    begin
      fld := msg.Fields[j];
      Result := Result + '    F' + fld.DelphiFieldName + ': ' +
        fld.DelphiFieldType + ';' + sLineBreak;
    end;
    for j := 0 to msg.Fields.Count - 1 do
    begin
      fld := msg.Fields[j];
      Result := Result + '    procedure Set' + fld.DelphiFieldName +
        '(const Value: ' + fld.DelphiFieldType + ');' + sLineBreak;
    end;
    Result := Result + '  public' + sLineBreak;
    for j := 0 to msg.Fields.Count - 1 do
    begin
      fld := msg.Fields[j];
      if not fld.name.IsEmpty then
      begin
        Result := Result + '    /// <summary>' + sLineBreak;
        Result := Result + WrapTextWithPrefix('    /// ', fld.name) +
          sLineBreak;
        Result := Result + '    /// </summary>' + sLineBreak;
      end;
      if not fld.Description.IsEmpty then
      begin
        Result := Result + '    /// <remarks>' + sLineBreak;
        Result := Result + WrapTextWithPrefix('    /// ', fld.Description) +
          sLineBreak;
        Result := Result + '    /// </remarks>' + sLineBreak;
      end;
      Result := Result + '    property ' + fld.DelphiFieldName + ': ' +
        fld.DelphiFieldType + ' read F' + fld.DelphiFieldName + ' write Set' +
        fld.DelphiFieldName + ';' + sLineBreak;
    end;
    Result := Result + '    constructor Create; override;' + sLineBreak;
    Result := Result +
      '    procedure LoadFromStream(Stream: TStream); override;' + sLineBreak;
    Result := Result + '    procedure SaveToStream(Stream: TStream); override;'
      + sLineBreak;
    Result := Result + '    function GetNewInstance: TOlfSMMessage; override;' +
      sLineBreak;
    Result := Result + '  end;' + sLineBreak;
    Result := Result + sLineBreak;
  end;
end;

function TMessagesList.GetMaxMessageID: TOlfMessageId;
var
  msg: TMessage;
begin
  Result := 0;
  for msg in self do
    if Result < msg.MessageID then
      Result := msg.MessageID;
end;

procedure TMessagesList.SetAsJSON(const Value: TJSONArray);
var
  jsv: tjsonvalue;
  msg: TMessage;
begin
  clear;
  if not assigned(Value) then
    exit;
  if Value.Count > 0 then
    for jsv in Value do
      if (jsv is TJSONObject) then
      begin
        msg := TMessage.Create(self);
        msg.AsJSON := jsv as TJSONObject;
        Add(msg);
      end;
  SortByMessageID;
end;

procedure TMessagesList.SortByDelphiClassName;
begin
  Sort(TComparer<TMessage>.Construct(
    function(const a, b: TMessage): integer
    var
      sA, sB: string;
    begin
      if a.FDelphiClassName.IsEmpty then
        sA := a.DefaultDelphiClassName
      else
        sA := a.FDelphiClassName;

      if b.FDelphiClassName.IsEmpty then
        sB := b.DefaultDelphiClassName
      else
        sB := b.FDelphiClassName;

      if sA = sB then
        Result := 0
      else if sA > sB then
        Result := 1
      else
        Result := -1;
    end));
end;

procedure TMessagesList.SortByMessageID;
begin
  Sort(TComparer<TMessage>.Construct(
    function(const a, b: TMessage): integer
    begin
      if a.FMessageID = b.FMessageID then
        Result := 0
      else if a.FMessageID > b.FMessageID then
        Result := 1
      else
        Result := -1;
    end));
end;

procedure TMessagesList.ValueChanged;
begin
  if assigned(FParent) then
    FParent.ValueChanged;
end;

{ TProject }

constructor TProject.Create;
begin
  Messages := TMessagesList.Create(self);
  FHasChanged := false;
  FFileName := '';
end;

destructor TProject.Destroy;
begin
  FMessages.Free;
  inherited;
end;

function TProject.GetAsString: string;
var
  jso: TJSONObject;
begin
  jso := AsJSON;
  try
    Result := jso.ToJSON;
  finally
    jso.Free;
  end;
end;

function TProject.DefaultDelphiUnitName(AName: string): string;
begin
  if AName.IsEmpty then
    Result := ToDelphiConst(name, true)
  else
    Result := ToDelphiConst(AName, true);
end;

function TProject.GetDelphiUnitName: string;
begin
  if FDelphiUnitName.IsEmpty then
    Result := DefaultDelphiUnitName
  else
    Result := FDelphiUnitName;
end;

function TProject.GetAsDelphi: string;
var
  i, j: integer;
  NeedOlfRTLStreamsUnit: boolean;
begin
  NeedOlfRTLStreamsUnit := false;
  for i := 0 to Messages.Count - 1 do
  begin
    for j := 0 to Messages[i].Fields.Count - 1 do
    begin
      NeedOlfRTLStreamsUnit :=
        (Messages[i].Fields[j].DelphiFieldType.tolower = 'string');
      if NeedOlfRTLStreamsUnit then
        break;
    end;
    if NeedOlfRTLStreamsUnit then
      break;
  end;
  Messages.SortByDelphiClassName;
  Result := 'unit ' + DelphiUnitName + ';' + sLineBreak;
  Result := Result + sLineBreak;
  Result := Result + '// ****************************************' + sLineBreak;
  Result := Result + WrapTextWithPrefix('// * ', name) + sLineBreak;
  Result := Result + '// ****************************************' + sLineBreak;
  Result := Result + '// ' + sLineBreak;
  if not Description.IsEmpty then
  begin
    Result := Result + WrapTextWithPrefix('// ', Description) + sLineBreak;
    Result := Result + '// ' + sLineBreak;
  end;
  Result := Result + '// ****************************************' + sLineBreak;
  Result := Result + '// File generator : ' + application.MainForm.Caption +
    sLineBreak;
  Result := Result +
    '// Website : https://socketmessaging.developpeur-pascal.fr/ ' + sLineBreak;
  Result := Result + '// Generation date : ' + DateTimeToStr(now) + sLineBreak;
  Result := Result + '// ' + sLineBreak;
  Result := Result +
    '// Don''t do any change on this file. They will be erased by next generation !'
    + sLineBreak;
  Result := Result + '// ****************************************' + sLineBreak;
  Result := Result + sLineBreak;
  Result := Result +
    '// To compile this unit you need Olf.Net.Socket.Messaging.pas from' +
    sLineBreak;
  Result := Result +
    '// https://github.com/DeveloppeurPascal/Socket-Messaging-Library' +
    sLineBreak;
  Result := Result + '//' + sLineBreak;
  Result := Result + '// Direct link to the file :' + sLineBreak;
  Result := Result +
    '// https://raw.githubusercontent.com/DeveloppeurPascal/Socket-Messaging-Library/main/src-library/Olf.Net.Socket.Messaging.pas'
    + sLineBreak;
  Result := Result + sLineBreak;
  if NeedOlfRTLStreamsUnit then
  begin
    Result := Result +
      '// To compile this unit you need Olf.RTL.Streams.pas from' + sLineBreak;
    Result := Result + '// https://github.com/DeveloppeurPascal/librairies' +
      sLineBreak;
    Result := Result + '//' + sLineBreak;
    Result := Result + '// Direct link to the file :' + sLineBreak;
    Result := Result +
      '// https://raw.githubusercontent.com/DeveloppeurPascal/librairies/master/Olf.RTL.Streams.pas'
      + sLineBreak;
    Result := Result + sLineBreak;
  end;
  Result := Result + 'interface' + sLineBreak;
  Result := Result + sLineBreak;
  Result := Result + 'uses' + sLineBreak;
  Result := Result + '  System.Classes,' + sLineBreak;
  Result := Result + '  Olf.Net.Socket.Messaging;' + sLineBreak;
  Result := Result + sLineBreak;
  if (Messages.Count > 0) then
  begin
    Result := Result + 'type' + sLineBreak;
    Result := Result + Messages.GetDelphiInterface;
  end;
  Result := Result +
    'procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);'
    + sLineBreak;
  Result := Result +
    'procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);'
    + sLineBreak;
  Result := Result + sLineBreak;
  Result := Result + 'implementation' + sLineBreak;
  Result := Result + sLineBreak;
  Result := Result + 'uses' + sLineBreak;
  if NeedOlfRTLStreamsUnit then
    Result := Result + '  Olf.RTL.Streams,' + sLineBreak;
  Result := Result + '  System.SysUtils;' + sLineBreak;
  Result := Result + sLineBreak;
  Result := Result +
    'procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);'
    + sLineBreak;
  Result := Result + 'begin' + sLineBreak;
  for i := 0 to Messages.Count - 1 do
    if Messages[i].RegisterMessageInTheServer then
      Result := Result + '  Server.RegisterMessageToReceive(' + Messages[i]
        .DelphiClassName + '.Create);' + sLineBreak;
  Result := Result + 'end;' + sLineBreak;
  Result := Result + sLineBreak;
  Result := Result +
    'procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);'
    + sLineBreak;
  Result := Result + 'begin' + sLineBreak;
  for i := 0 to Messages.Count - 1 do
    if Messages[i].RegisterMessageInTheClient then
      Result := Result + '  Client.RegisterMessageToReceive(' + Messages[i]
        .DelphiClassName + '.Create);' + sLineBreak;
  Result := Result + 'end;' + sLineBreak;
  Result := Result + sLineBreak;
  if (Messages.Count > 0) then
    Result := Result + Messages.GetDelphiImplementation;
  Result := Result + 'end.' + sLineBreak;
end;

function TProject.GetAsJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('name', FName);
  Result.AddPair('description', FDescription);
  Result.AddPair('delphiunitname', FDelphiUnitName);
  Messages.SortByMessageID;
  Result.AddPair('messages', Messages.AsJSON);
end;

procedure TProject.LoadFromFile(AFileName: string);
begin
  AFileName := AFileName.Trim;

  if AFileName.IsEmpty then
    raise exception.Create('Empty filename !');

  if not tfile.Exists(AFileName) then
    raise exception.Create('This file does not exist !');

  AsString := tfile.ReadAllText(AFileName, tencoding.UTF8);
  FFileName := AFileName;
  HasChanged := false;
end;

procedure TProject.SaveToFile(AFileName: string; AForceWrite: boolean);
begin
  AFileName := AFileName.Trim;
  if AFileName.IsEmpty then
    AFileName := FFileName;

  if AFileName.IsEmpty then
    raise exception.Create('Empty filename !');

  if (not AForceWrite) and tfile.Exists(AFileName) then
    raise exception.Create('This file already exists !');

  tfile.WriteAllText(AFileName, AsString, tencoding.UTF8);
  FFileName := AFileName;
  HasChanged := false;
end;

procedure TProject.SetAsString(const Value: string);
var
  jso: TJSONObject;
begin
  jso := TJSONObject.ParseJSONValue(Value) as TJSONObject;
  if assigned(jso) then
    try
      AsJSON := jso;
    finally
      jso.Free;
    end;
end;

procedure TProject.SetDelphiUnitName(const Value: string);
begin
  if (FDelphiUnitName = Value) then
    exit;
  ValueChanged;
  FDelphiUnitName := Value;
end;

procedure TProject.SetDescription(const Value: string);
begin
  if (FDescription = Value) then
    exit;
  ValueChanged;
  FDescription := Value;
end;

procedure TProject.SetHasChanged(const Value: boolean);
begin
  FHasChanged := Value;
end;

procedure TProject.SetMessages(const Value: TMessagesList);
begin
  if (FMessages = Value) then
    exit;
  ValueChanged;
  FMessages := Value;
end;

procedure TProject.SetName(const Value: string);
begin
  if (FName = Value) then
    exit;
  ValueChanged;
  FName := Value;
end;

procedure TProject.SetAsJSON(const Value: TJSONObject);
var
  jsa: TJSONArray;
begin
  if not assigned(Value) then
    exit;

  ValueChanged;
  if not Value.TryGetValue<string>('name', FName) then
    FName := '';
  if not Value.TryGetValue<string>('description', FDescription) then
    FDescription := '';
  if not Value.TryGetValue<string>('delphiunitname', FDelphiUnitName) then
    FDelphiUnitName := '';
  if not Value.TryGetValue<TJSONArray>('messages', jsa) then
  begin
    jsa := TJSONArray.Create;
    try
      Messages.AsJSON := jsa;
    finally
      jsa.Free;
    end;
  end
  else
    Messages.AsJSON := jsa;
end;

procedure TProject.ValueChanged;
begin
  HasChanged := true;
end;

end.
