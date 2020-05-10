(******************************************************************************)
(*                                 libPasCURL                                 *)
(*                 object pascal wrapper around cURL library                  *)
(*                        https://github.com/curl/curl                        *)
(*                                                                            *)
(* Copyright (c) 2019 - 2020                                Ivan Semenkov     *)
(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* Project:         'RemoteConnect'                                           *)
(* Functionality:   Simple  example for  use THTTPSessionPlain  and TResponse *)
(*                  classes for connect to remote host                        *)
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
  Classes, SysUtils, CustApp, http;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  protected
    FSession : THTTPSessionPlain;
    FResponse : THTTPSessionPlain.THTTPResponseResult;

    procedure DoRun; override;
    procedure PrintHeader;
    procedure PrintHelp;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TApplication }

procedure TApplication.DoRun;
const
  COLUMN_SIZE = 31;
var
  ErrorMsg: String;
  NonOptions : TStringList;
  ShortOptions : string = 'eah';
  LongOptions : array [1..25] of string = ('help', 'echo', 'all',
    'effective-url', 'redirect-url', 'response-code', 'content-type',
    'primary-ip', 'local-ip', 'http-version', 'redirect-count', 'content-size',
    'header-size', 'request-size', 'download-speed', 'total-time',
    'name-lookup-time', 'connect-time', 'verify-ssl', 'num-connects',
    'destination-port', 'local-port', 'pretransfer-time', 'start-transfer-time',
    'redirect-time');
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

  NonOptions := TStringList.Create;
  GetNonOptions(ShortOptions, LongOptions, NonOptions);
  if NonOptions.Count = 0 then
  begin
    Terminate;
    Exit;
  end;

  FSession.Url(NonOptions[0]);
  FResponse := FSession.Get;

  if FResponse.Ok and not FResponse.Value.HasErrors then
  begin
    if HasOption('a', 'all') or HasOption('effective-url') then
      writeln('Url: ':COLUMN_SIZE, FResponse.Value.EffectiveUrl);

    if HasOption('a', 'all') or HasOption('redirect-url') then
      writeln('Redirect url: ':COLUMN_SIZE, FResponse.Value.RedirectUrl);

    if HasOption('a', 'all') or HasOption('redirect-count') then
      writeln('Redirect count: ':COLUMN_SIZE, FResponse.Value.RedirectCount);

    if HasOption('a', 'all') or HasOption('content-type') then
      writeln('Content type: ':COLUMN_SIZE, FResponse.Value.ContentType);

    if HasOption('a', 'all') or HasOption('primary-ip') then
      writeln('Primary IP: ':COLUMN_SIZE, FResponse.Value.PrimaryIP);

    if HasOption('a', 'all') or HasOption('destination-port') then
      writeln('Destination port: ':COLUMN_SIZE, FResponse.Value.PrimaryPort);

    if HasOption('a', 'all') or HasOption('local-port') then
      writeln('Local port: ':COLUMN_SIZE, FResponse.Value.LocalPort);

    if HasOption('a', 'all') or HasOption('local-ip') then
      writeln('Local IP: ':COLUMN_SIZE, FResponse.Value.LocalIP);

    if HasOption('a', 'all') or HasOption('http-version') then
      writeln('HTTP version: ':COLUMN_SIZE, FResponse.Value.HttpVersion);

    if HasOption('a', 'all') or HasOption('response-code') then
      writeln('Response code: ':COLUMN_SIZE, FResponse.Value.ResponseCode);

    if HasOption('a', 'all') or HasOption('num-connects') then
      writeln('Number of created connections: ':COLUMN_SIZE,
        FResponse.Value.NumConnects);

    if HasOption('a', 'all') or HasOption('verify-ssl') then
      if FResponse.Value.VerifySSLResult then
        writeln('Verify SSL: ':COLUMN_SIZE, 'Good! All Ok.')
      else
        writeln('Verify SSL: ', COLUMN_SIZE, 'Something wrong :(');

    if HasOption('a', 'all') or HasOption('request-size') then
      writeln('Request size: ':COLUMN_SIZE,
        FResponse.Value.RequestSize.ToString);

    if HasOption('a', 'all') or HasOption('header-size') then
      writeln('Header size: ':COLUMN_SIZE, FResponse.Value.HeaderSize.ToString);

    if HasOption('a', 'all') or HasOption('content-size') then
      writeln('Content size: ':COLUMN_SIZE,
        FResponse.Value.Downloaded.ToString);

    if HasOption('a', 'all') or HasOption('total-time') then
      writeln('Total time: ':COLUMN_SIZE, FResponse.Value.TotalTime.ToString);

    if HasOption('a', 'all') or HasOption('name-lookup-time') then
      writeln('Name lookup time: ':COLUMN_SIZE,
        FResponse.Value.NameLookup.ToString);

    if HasOption('a', 'all') or HasOption('connect-time') then
      writeln('Connect time: ':COLUMN_SIZE,
        FResponse.Value.ConnectTime.ToString);

    if HasOption('a', 'all') or HasOption('pretransfer-time') then
      writeln('Time until the transfer start: ':COLUMN_SIZE,
        FResponse.Value.PretransferTime.ToString);

    if HasOption('a', 'all') or HasOption('start-transfer-time') then
      writeln('Start transfer time: ':COLUMN_SIZE,
        FResponse.Value.StartTransferTime.ToString);

    if HasOption('a', 'all') or HasOption('redirect-time') then
      writeln('Redirect time: ':COLUMN_SIZE,
        FResponse.Value.RedirectTime.ToString);

    if HasOption('a', 'all') or HasOption('download-speed') then
      writeln('Download speed: ':COLUMN_SIZE,
        FResponse.Value.DownloadSpeed.ToString('/s'));

    if HasOption('e', 'echo') then
    begin
      writeln();
      writeln('-=== Content ===-');
      writeln(FResponse.Value.Content);
    end;
  end else
    writeln(FResponse.Value.ErrorMessage);

  FreeAndNil(NonOptions);
  Terminate;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FSession := THTTPSessionPlain.Create;
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
'(* Copyright (c) 2019 - 2020                                Ivan Semenkov     *)'+ sLineBreak +
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
'(* Example how to use THTTPSessionPlain and THTTPResponse  classes to connect *)'+ sLineBreak +
'(* to remote host.                                                            *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Usage: RemoteConnect  http://example.com/ --echo                           *)'+ sLineBreak +
'(*    Or: RemoteConnect https://example.com/ -a                               *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* -e      or --echo                   write download content to terminal     *)'+ sLineBreak +
'(* -a      or --all                    write all response information         *)'+ sLineBreak +
'(*            --effective-url          write effective url                    *)'+ sLineBreak +
'(*            --redirect-url           write redirect url if is it            *)'+ sLineBreak +
'(*            --redirect-count         write redirect counts if is it         *)'+ sLineBreak +
'(*            --content-type           write return Content-Type header value *)'+ sLineBreak +
'(*            --primary-ip             write primary IP address               *)'+ sLineBreak +
'(*            --destination-port       write destination port number          *)'+ sLineBreak +
'(*            --local-port             write local port number                *)'+ sLineBreak +
'(*            --local-ip               write local IP address                 *)'+ sLineBreak +
'(*            --http-version           write HTTP version                     *)'+ sLineBreak +
'(*            --response-code          write response code                    *)'+ sLineBreak +
'(*            --num-connects           write number of created connections    *)'+ sLineBreak +
'(*            --verify-ssl             verify of the certificate verification *)'+ sLineBreak +
'(*            --request-size           write send request size                *)'+ sLineBreak +
'(*            --header-size            write response header size             *)'+ sLineBreak +
'(*            --content-size           write response content size            *)'+ sLineBreak +
'(*            --total-time             write total request time               *)'+ sLineBreak +
'(*            --name-lookup-time       write name lookup time                 *)'+ sLineBreak +
'(*            --connect-time           write connect time                     *)'+ sLineBreak +
'(*            --pretransfer-time       write time until the tfansfer start    *)'+ sLineBreak +
'(*            --start-transfer-time    write time until the first byte is     *)'+ sLineBreak +
'(*                                     received                               *)'+ sLineBreak +
'(*            --redirect-time          write time for all redirection steps   *)'+ sLineBreak +
'(*            --download-speed         write download speed                   *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
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

