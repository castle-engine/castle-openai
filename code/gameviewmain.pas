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
  function OpenAiQuery(const Query: String;
    const InputContents: String; const HttpMethod: THttpMethod = hmPost): TJsonData;
  var
    Download: TCastleDownload;
  begin
    Download := TCastleDownload.Create(nil);
    try
      Download.HttpHeader('Authorization', 'Bearer ' + OpenAIApiKey);
      Download.HttpHeader('Content-Type', 'application/json');
      Download.HttpHeader('OpenAI-Beta', 'assistants=v2');
      Download.Url := 'https://api.openai.com/v1/' + Query;
      Download.HttpMethod := HttpMethod;
      WriteStr(Download.HttpRequestBody, InputContents);
      Download.Start;
      Download.WaitForFinish;
      if Download.Status = dsSuccess then
        Result := GetJson(StreamToString(Download.Contents))
      else
        raise Exception.CreateFmt('Error downloading from OpenAI: %s, contents: %s', [
          Download.ErrorMessage,
          StreamToString(Download.Contents)
        ]);
    finally FreeAndNil(Download) end;
  end;

var
  ThreadsResponse, MessageResponse, RunResponse, RunStatusResponse: TJsonData;
  MessageRequest, RunRequest: TJsonObject;
  ThreadId, MessageId, RunId, RunStatus: String;
begin
  LabelAnswer.Caption := 'You asked: ' + EditQuery.Text;

  { Communicate using OpenAI REST API to get the answer to the question.
    See ../test_openai.sh for the curl command that does this,
    with links to docs.
    Here, we just do this in Pascal, using TCastleDownload,
    in a general way. }

  ThreadsResponse := OpenAiQuery('threads', '');
  try
    ThreadId := (ThreadsResponse as TJSONObject).Strings['id'];
    WritelnLog('OpenAI', 'Thread id: ' + ThreadId);
    if not IsPrefix('thread_', ThreadId) then
      raise Exception.Create('Unexpected thread id: ' + ThreadId);
  finally FreeAndNil(ThreadsResponse) end;

  MessageRequest := TJsonObject.Create;
  try
    MessageRequest.Strings['role'] := 'user';
    MessageRequest.Strings['content'] := EditQuery.Text;
    MessageResponse := OpenAiQuery('threads/' + ThreadId + '/messages', MessageRequest.AsJSON);
    try
      MessageId := (MessageResponse as TJSONObject).Strings['id'];
      WritelnLog('OpenAI', 'Message id: ' + MessageId);
      if not IsPrefix('msg_', MessageId) then
        raise Exception.Create('Unexpected message id: ' + MessageId);
    finally FreeAndNil(MessageResponse) end;
  finally FreeAndNil(MessageRequest) end;

  RunRequest := TJsonObject.Create;
  try
    RunRequest.Strings['assistant_id'] := OpenAiAssistantId;
    RunResponse := OpenAiQuery('threads/' + ThreadId + '/runs', RunRequest.AsJSON);
    try
      RunId := (RunResponse as TJSONObject).Strings['id'];
      WritelnLog('OpenAI', 'Run id: ' + RunId);
      if not IsPrefix('run_', RunId) then
        raise Exception.Create('Unexpected run id: ' + RunId);
    finally FreeAndNil(RunResponse) end;
  finally FreeAndNil(RunRequest) end;

  { Query run, until status is completed.
    TODO: Show some progress to the user, allow interrupting. }
  while true do
  begin
    RunStatusResponse := OpenAiQuery('threads/' + ThreadId + '/runs/' + RunId,
      '', hmGet);
    try
      RunStatus := (RunStatusResponse as TJSONObject).Strings['status'];
      WritelnLog('OpenAI', 'Run status: ' + RunStatus);
      if RunStatus = 'completed' then
        Break;
    finally FreeAndNil(RunStatusResponse) end;
    Sleep(500);
  end;

end;

end.
