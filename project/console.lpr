program console;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, pascurl;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  protected
    session : TSession;
    session_info : TSessionInfo;

    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TApplication }

procedure TApplication.DoRun;
//var
  //ErrorMsg: String;
begin
  (*
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  *)

  if ParamCount >= 1 then
  begin
    session.Url := Params[1];
    session.Protocol.DefaultProtocol := PROTOCOL_HTTP;
    session_info := TSessionInfo.Create(session);
    if session_info.Opened and not session_info.HasErrors then
    begin
      writeln('Url: ':20,                session_info.EffectiveUrl);
      writeln('Response code: ':20,      session_info.ResponseCode);
      writeln('Header size, kB: ':20,    session_info.HeaderSize.Format(dsKiloBytes, '0.00'));
      writeln('Content type: ':20,       session_info.ContentType);
      writeln('Content length, kB: ':20, session_info.Downloaded.Format(dsKiloBytes, '0.00'));
      writeln('IP: ':20,                 session_info.PimaryIP);
      writeln('Total time, ms: ':20,     session_info.TotalTime.Format(tiMicroseconds, '0.##'));
      writeln('==== Content ====');
      writeln(session_info.Content);
    end else
    begin
      writeln(session_info.ErrorMessage);
    end;
  end;

  Terminate;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  session := TSession.Create;
end;

destructor TApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TApplication.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TApplication;
begin
  Application:=TApplication.Create(nil);
  Application.Title:='console';
  Application.Run;
  Application.Free;
end.

