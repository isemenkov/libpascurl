program pascurl_console;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, tcurl;

type
  TApplication = class(TCustomApplication)
  protected
    session : TSession;
    info : TSessionInfo;

    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TApplication }

procedure TApplication.DoRun;
var
  //ErrorMsg: String;
  url : String;

begin

  (*
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;
  *)
  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if ParamCount = 1 then
  begin
    Url := Params[1];

    session.Url := Url;
    info := TSessionInfo.Create(session);
    if info.Opened then
    begin
      writeln('Response code: ', info.ResponseCode);
      writeln('Content: ', info.Content);
    end;
  end;

  // stop program loop
  Terminate;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;

  //handle := curl_easy_init();
  session := TSession.Create;
end;

destructor TApplication.Destroy;
begin
  //curl_easy_cleanup(handle);

  inherited Destroy;
end;

procedure TApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TApplication;
begin
  Application:=TApplication.Create(nil);
  Application.Title:='Application';
  Application.Run;
  Application.Free;
end.

