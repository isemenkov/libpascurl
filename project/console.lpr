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
    FSession : TSession;
    FResponse : TResponse;
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
  ShortOptions : string = 'hu:p:';
  LongOptions : array [1..4] of string = ('help', 'show-content', 'username:',
    'password:');
  protocol : TProtocol;
begin
  ErrorMsg := CheckOptions(ShortOptions, LongOptions);
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('u', 'username') then
  begin
    FSession.Security.Username := GetOptionValue('u', 'username');
  end;

  if HasOption('p', 'password') then
  begin
    FSession.Security.Password := GetOptionValue('p', 'password');
  end;

  NonOptions := TStringList.Create;
  try
    GetNonOptions(ShortOptions, LongOptions, NonOptions);
    ProcessNonOptions(NonOptions);
  finally
    FreeAndNil(NonOptions);
  end;

  FResponse := TResponse.Create(FSession);
  if FResponse.Opened and not FResponse.HasErrors then
  begin
    protocol := FSession.ExtractProtocol(FResponse.EffectiveUrl);

    writeln('Url: ':20,                FResponse.EffectiveUrl);
    if protocol in [PROTOCOL_HTTP, PROTOCOL_HTTPS] then
    begin
      writeln('Response code: ':20,      THTTPStatusCode(FResponse.ResponseCode));
    end;
    if protocol in [PROTOCOL_FTP, PROTOCOL_FTPS] then
    begin
      writeln('Response code: ':20,    TFTPStatusCode(FResponse.ResponseCode));
    end;
    writeln('Header size, kB: ':20,    FResponse.HeaderSize.Format(dsKiloBytes, '0.00'));
    writeln('Content type: ':20,       FResponse.ContentType);
    writeln('Content length, kB: ':20, FResponse.Downloaded.Format(dsKiloBytes, '0.##'));
    writeln('IP: ':20,                 FResponse.PimaryIP);
    writeln('Total time, ms: ':20,     FResponse.TotalTime.Format(tiMicroseconds, '0.##'));

    if HasOption('show-content') then
    begin
      writeln('==== Content ====');
      writeln(FResponse.Content);
    end;
  end else
  begin
    writeln(FResponse.ErrorMessage);
  end;

  FreeAndNil(FResponse);
  Terminate;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FSession := TSession.Create;
end;

destructor TApplication.Destroy;
begin
  FReeAndNil(FSession);
  inherited Destroy;
end;

procedure TApplication.WriteHelp;
begin
  writeln('-h --help':20,                 ' show this help');
  writeln('   --show-content':20,         ' print content');
  writeln('-u --username=<username>':20,  ' set username');
  writeln('-p --password=<password>':20,  ' set password');
end;

procedure TApplication.ProcessNonOptions(AParams: TStringList);
begin
  if AParams.Count > 0 then
  begin
    FSession.Url := AParams[0];
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

