{ Copyright 2024-2024 Michalis Kamburelis.
  See LICENSE for licensing. }

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  { Enable https downloads. }
  {$ifdef FPC} OpenSslSockets, {$endif}
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonSend: TCastleButton;
    EditQuery: TCastleEdit;
    LabelAnswer: TCastleLabel;
  private
    procedure ClickSend(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, FpJson, JsonParser,
  CastleDownload, CastleClassUtils, CastleStringUtils, CastleLog;

const
  {$I openai_config.inc}

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickSend(Sender: TObject);

  { Set HTTP POST request to OpenAI,
    with Query as URL part after https://api.openai.com/v1/ .
    Returns parsed JSON (it is caller's responsibility to free it). }
  function OpenAiQuery(const Query: String; const InputContents: String): TJsonData;
  var
    Download: TCastleDownload;
  begin
    Download := TCastleDownload.Create(nil);
    try
      Download.HttpHeader('Authorization', 'Bearer ' + OpenAIApiKey);
      Download.HttpHeader('Content-Type', 'application/json');
      Download.HttpHeader('OpenAI-Beta', 'assistants=v2');
      Download.Url := 'https://api.openai.com/v1/' + Query;
      Download.HttpMethod := hmPost;
      WriteStr(Download.HttpRequestBody, InputContents);
      Download.Start;
      Download.WaitForFinish;
      Result := GetJson(StreamToString(Download.Contents));
    finally FreeAndNil(Download) end;
  end;

var
  Response: TJsonData;
  ThreadId: String;
begin
  LabelAnswer.Caption := 'You asked: ' + EditQuery.Text;

  { Communicate using OpenAI REST API to get the answer to the question.
    See ../test_openai.sh for the curl command that does this,
    with links to docs.
    Here, we just do this in Pascal, using TCastleDownload,
    in a general way. }

  Response := OpenAiQuery('threads', '');
  try
    ThreadId := (Response as TJSONObject).Strings['id'];
    WritelnLog('OpenAI', 'Thread id: ' + ThreadId);
    if not IsPrefix('thread_', ThreadId) then
      raise Exception.Create('Unexpected thread id: ' + ThreadId);
  finally
    FreeAndNil(Response);
  end;
end;

end.
