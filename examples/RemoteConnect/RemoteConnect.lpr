(******************************************************************************)
(*                                 libPasCURL                                 *)
(*                 object pascal wrapper around cURL library                  *)
(*                        https://github.com/curl/curl                        *)
(*                                                                            *)
(* Copyright (c) 2019                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* Project:         'RemoteConnect'                                           *)
(* Functionality:   Simple example for use TSession and TResponse classes for *)
(*                  connect to remote host                                    *)
(*                                                                            *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation; either version 3 of the License.                      *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License for *)
(* more details.                                                              *)
(*                                                                            *)
(* A copy  of the  GNU General Public License is available  on the World Wide *)
(* Web at <http://www.gnu.org/copyleft/gpl.html>. You  can also obtain  it by *)
(* writing to the Free Software Foundation, Inc., 51  Franklin Street - Fifth *)
(* Floor, Boston, MA 02110-1335, USA.                                         *)
(*                                                                            *)
(******************************************************************************)

program RemoteConnect;

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
    FSession : TSession;
    FResponse : TResponse;

    procedure DoRun; override;
    procedure PrintHeader;
    procedure PrintHelp;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TApplication }

procedure TApplication.DoRun;
var
  ErrorMsg: String;
  NonOptions : TStringList;
  ShortOptions : string = 'su:p:ah';
  LongOptions : array [1..16] of string = ('help', 'show-content', 'username:',
    'password:', 'all', 'effective-url', 'redirect-url', 'response-code',
    'content-type', 'primary-ip', 'local-ip', 'http-version', 'redirect-count',
    'content-size', 'header-size', 'request-size');
  Protocol : TProtocol;
begin
  ErrorMsg := CheckOptions(ShortOptions, LongOptions);
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  PrintHeader;

  if HasOption('h', 'help') then
    PrintHelp;

  if HasOption('u', 'username') then
    FSession.Security.Username := GetOptionValue('u', 'username');
  if HasOption('p', 'password') then
    FSession.Security.Password := GetOptionValue('p', 'password');

  NonOptions := TStringList.Create;
  GetNonOptions(ShortOptions, LongOptions, NonOptions);
  if NonOptions.Count = 0 then
  begin
    Terminate;
    Exit;
  end;

  FSession.Url := NonOptions[0];
  FResponse := TResponse.Create(FSession);
  if FResponse.Opened and not FResponse.HasErrors then
  begin
    Protocol := FSession.ExtractProtocol(FResponse.EffectiveUrl);

    if HasOption('a', 'all') or HasOption('effective-url') then
      writeln('Url: ':20, FResponse.EffectiveUrl);

    if HasOption('a', 'all') or HasOption('redirect-url') then
      writeln('Redirect url: ':20, FResponse.RedirectUrl);

    if HasOption('a', 'all') or HasOption('redirect-count') then
      writeln('Redirect count: ':20, FResponse.RedirectCount);

    if HasOption('a', 'all') or HasOption('content-type') then
      writeln('Content type: ':20, FResponse.ContentType);

    if HasOption('a', 'all') or HasOption('request-size') then
    begin
      if FResponse.RequestSize.KiloBytes >= 1 then
        writeln('Request size, Kb: ':20,
          FResponse.RequestSize.Format(dsKiloBytes, '0.##'))
      else
        writeln('Request size, b: ':20, FResponse.RequestSize.B);
    end;

    if HasOption('a', 'all') or HasOption('header-size') then
    begin
      if FResponse.HeaderSize.KiloBytes >= 1 then
        writeln('Header size, Kb: ':20,
          FResponse.HeaderSize.Format(dsKiloBytes, '0.##'))
      else
        writeln('Header size, b: ':20, FResponse.HeaderSize.B);
    end;

    if HasOption('a', 'all') or HasOption('content-size') then
    begin
      if FResponse.Downloaded.MegaBytes >= 1 then
        writeln('Content size, Mb: ':20,
          FResponse.Downloaded.Format(dsMegaBytes, '0.##'))
      else if FResponse.Downloaded.KiloBytes >= 1 then
        writeln('Content size, Kb: ':20,
          FResponse.Downloaded.Format(dsKiloBytes, '0.##'))
      else
        writeln('Content size, b: ':20, FResponse.Downloaded.B);
    end;

    if HasOption('a', 'all') or HasOption('primary-ip') then
      writeln('Primary IP: ':20, FResponse.PrimaryIP);

    if HasOption('a', 'all') or HasOption('local-ip') then
      writeln('Local IP: ':20, FResponse.LocalIP);

    if HasOption('a', 'all') or HasOption('response-code') then
    begin
      if Protocol in [PROTOCOL_HTTP, PROTOCOL_HTTPS] then
        writeln('Response code: ':20, THTTPStatusCode(FResponse.ResponseCode))
      else if Protocol in [PROTOCOL_FTP, PROTOCOL_FTPS] then
        writeln('Response code: ':20, TFTPStatusCode(FResponse.ResponseCode));
    end;

    if HasOption('a', 'all') or HasOption('http-version') then
    begin
      if Protocol in [PROTOCOL_HTTP, PROTOCOL_HTTPS] then
        writeln('HTTP version: ':20, FResponse.HttpVersion);
    end;

    if HasOption('s', 'show-content') then
    begin
      writeln();
      writeln('-=== Content ===-');
      writeln(FResponse.Content);
    end;
  end else
    writeln(FResponse.ErrorMessage);

  FreeAndNil(NonOptions);
  Terminate;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FSession := TSession.Create;
end;

destructor TApplication.Destroy;
begin
  FreeAndNil(FSession);
  inherited Destroy;
end;

procedure TApplication.PrintHeader;
begin
  writeln(
'(******************************************************************************)'+ sLineBreak +
'(*                                 libPasCURL                                 *)'+ sLineBreak +
'(*                 object pascal wrapper around cURL library                  *)'+ sLineBreak +
'(*                        https://github.com/curl/curl                        *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Copyright (c) 2019                                       Ivan Semenkov     *)'+ sLineBreak +
'(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)'+ sLineBreak +
'(*                                                          Ukraine           *)'+ sLineBreak +
'(******************************************************************************)'
  );
end;

procedure TApplication.PrintHelp;
begin
  writeln(
'(******************************************************************************)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Example how  to use TSession  and TResponse  classes to connect to  remote *)'+ sLineBreak +
'(* host.                                                                      *)'+ sLineBreak +
'(* Usage: RemoteConnect http://example.com/ --show-content                    *)'+ sLineBreak +
'(*    Or: RemoteConnect  ftp://ftp.example.com/ -u Root -p Password -s        *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* -s            or --show-content        write download content to termainal *)'+ sLineBreak +
'(* -u <username> or --username=<username> set user name to remote host        *)'+ sLineBreak +
'(* -p <password> or --password=<password> set password to remote host         *)'+ sLineBreak +
'(* -a            or --all                 write all response information      *)'+ sLineBreak +
'(*                  --effective-url       write effective url                 *)'+ sLineBreak +
'(*                  --redirect-url        write redirect url if is it         *)'+ sLineBreak +
'(*                  --redirect-count      write redirect counts if is it      *)'+ sLineBreak +
'(*                  --response-code       write response code for HTTP, HTTPS,*)'+ sLineBreak +
'(*                                        FTP, FTPS only                      *)'+ sLineBreak +
'(*                  --content-type        write content type                  *)'+ sLineBreak +
'(*                  --primary-ip          write primary IP address            *)'+ sLineBreak +
'(*                  --local-ip            write local IP address              *)'+ sLineBreak +
'(*                  --http-version        write HTTP version for HTTP, HTTPS  *)'+ sLineBreak +
'(*                                        protocols only                      *)'+ sLineBreak +
'(*                  --request-size        write send request size             *)'+ sLineBreak +
'(*                  --header-size         write response header size          *)'+ sLineBreak +
'(*                  --content-size        write response content size         *)'+ sLineBreak +
'(******************************************************************************)'
  );
end;

var
  Application: TApplication;
begin
  Application:=TApplication.Create(nil);
  Application.Title:='RemoteConnect';
  Application.Run;
  Application.Free;
end.

