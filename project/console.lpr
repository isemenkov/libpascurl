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
  private
    session : TSession;
    session_info : TSessionInfo;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure WriteHelp;
    procedure ProcessNonOptions (AParams : TStringList);
  end;

{ TApplication }

procedure TApplication.DoRun;
var
  ErrorMsg: String;
  NonOptions : TStringList;
  ShortOptions : string = 'hs';
  LongOptions : array [1..2] of string = ('help', 'show-content');
begin
  ErrorMsg:=CheckOptions(ShortOptions, LongOptions);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  NonOptions := TStringList.Create;
  try
    GetNonOptions(ShortOptions, LongOptions, NonOptions);
    ProcessNonOptions(NonOptions);
  finally
    FreeAndNil(NonOptions);
  end;

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

    if HasOption('s', 'show-content') then
    begin
      writeln('==== Content ====');
      writeln(session_info.Content);
    end;
  end else
  begin
    writeln(session_info.ErrorMessage);
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
  writeln('-h --help':20,         ' show this help');
  writeln('-s --show-content':20, ' print content');
end;

procedure TApplication.ProcessNonOptions(AParams: TStringList);
begin
  if AParams.Count > 0 then
  begin
    session.Url := AParams[0];
  end;
end;

var
  Application: TApplication;
begin
  Application:=TApplication.Create(nil);
  Application.Title:='console';
  Application.Run;
  Application.Free;
end.

