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
(* Project:         'RemoteConnectCStyle'                                     *)
(* Functionality:   Simple example for use cURL wrapper in C-Style to connect *)
(*                  to remote host                                            *)
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

program RemoteConnectCStyle;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}cthreads,{$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, libpascurl, curl.http.response.status_code,
  curl.http.response.http_version;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  protected
    FCURL : libpascurl.CURL;
    FBuffer : TMemoryStream;

    procedure DoRun; override;
    procedure PrintHeader;
    procedure PrintHelp;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    class function WriteFunctionCallback (ptr : PChar; size : LongWord;
      nmemb : LongWord; data : Pointer) : LongWord; static; cdecl;
    function Write (ptr : PChar; size : LongWord; nmemb : LongWord) : LongWord;
      inline;
  end;

{ TApplication }

procedure TApplication.DoRun;
var
  ErrorMsg: String;
  NonOptions : TStringList;
  ShortOptions : string = 'su:p:ah';
  LongOptions : array [1..17] of string = ('help', 'show-content', 'username:',
    'password:', 'all', 'effective-url', 'redirect-url', 'response-code',
    'content-type', 'primary-ip', 'local-ip', 'http-version', 'redirect-count',
    'content-size', 'header-size', 'request-size', 'download-speed');
  ErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  Scheme, StringParam : PChar;
  Content : String;
  LongintParam : Longint;
  LongWordParam : LongWord;
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
    curl_easy_setopt(FCURL, CURLOPT_USERNAME,
      PChar(GetOptionValue('u', 'username')));
  if HasOption('p', 'password') then
    curl_easy_setopt(FCURL, CURLOPT_PASSWORD,
      PChar(GetOptionValue('p', 'password')));

  NonOptions := TStringList.Create;
  GetNonOptions(ShortOptions, LongOptions, NonOptions);
  if NonOptions.Count = 0 then
  begin
    Terminate;
    Exit;
  end;

  curl_easy_setopt(FCURL, CURLOPT_URL, PChar(NonOptions[0]));
  curl_easy_setopt(FCURL, CURLOPT_FOLLOWLOCATION, Longint(1));
  curl_easy_setopt(FCURL, CURLOPT_ERRORBUFFER, PChar(ErrorBuffer));
  curl_easy_setopt(FCURL, CURLOPT_WRITEDATA, Pointer(Self));
  curl_easy_setopt(FCURL, CURLOPT_WRITEFUNCTION,
    @TApplication.WriteFunctionCallback);

  New(Scheme);
  New(StringParam);

  if curl_easy_perform(FCURL) = CURLE_OK then
  begin
    curl_easy_getinfo(FCURL, CURLINFO_SCHEME, @Scheme);

    if HasOption('a', 'all') or HasOption('effective-url') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_EFFECTIVE_URL, @StringParam);
      writeln('Url: ':25, StringParam);
    end;

    if HasOption('a', 'all') or HasOption('redirect-url') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_REDIRECT_URL, @StringParam);
      writeln('Redirect url: ':25, StringParam);
    end;

    if HasOption('a', 'all') or HasOption('redirect-count') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_REDIRECT_COUNT, @LongintParam);
      writeln('Redirect count: ':25, LongintParam);
    end;

    if HasOption('a', 'all') or HasOption('content-type') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_CONTENT_TYPE, @StringParam);
      writeln('Content type: ':25, StringParam);
    end;

    if HasOption('a', 'all') or HasOption('request-size') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_REQUEST_SIZE, @LongWordParam);

      if LongWordParam >= 1 * 1024 then
        writeln('Request size, Kb: ':25,
          FormatFloat('0.##', Double(LongWordParam / 1024)))
      else
        writeln('Request size, b: ':25, LongWordParam);
    end;

    if HasOption('a', 'all') or HasOption('header-size') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_HEADER_SIZE, @LongWordParam);

      if LongWordParam >= 1 * 1024 then
        writeln('Header size, Kb: ':25,
          FormatFloat('0.##', Double(LongWordParam / 1024)))
      else
        writeln('Header size, b: ':25, LongWordParam);
    end;

    if HasOption('a', 'all') or HasOption('content-size') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_SIZE_DOWNLOAD_T, @LongWordParam);

      if LongWordParam >= 1 * 1024 * 1024 then
        writeln('Content size, Mb: ':25,
          FormatFloat('0.##', Double(LongWordParam / (1024 * 1024))))
      else if LongWordParam >= 1 * 1024 then
        writeln('Content size, Kb: ':25,
          FormatFloat('0.##', Double(LongWordParam / 1024)))
      else
        writeln('Content size, b: ':25, LongWordParam);
    end;

    if HasOption('a', 'all') or HasOption('primary-ip') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_PRIMARY_IP, @StringParam);
      writeln('Primary IP: ':25, StringParam);
    end;

    if HasOption('a', 'all') or HasOption('local-ip') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_LOCAL_IP, @StringParam);
      writeln('Local IP: ':25, StringParam);
    end;

    if HasOption('a', 'all') or HasOption('response-code') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_RESPONSE_CODE, @LongintParam);

      if (Scheme = 'http') or (Scheme = 'https') then
        writeln('Response code: ':25, THTTPStatusCode(LongintParam))
      else if (Scheme = 'ftp') or (Scheme = 'ftps') then
        writeln('Response code: ':25, LongintParam);
    end;

    if HasOption('a', 'all') or HasOption('http-version') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_HTTP_VERSION, @LongintParam);

      if (Scheme = 'http') or (Scheme = 'https') then
        writeln('HTTP version: ':25, THTTPVersion(LongintParam));
    end;

    if HasOption('a', 'all') or HasOption('download-speed') then
    begin
      curl_easy_getinfo(FCURL, CURLINFO_SPEED_DOWNLOAD_T, @LongWordParam);

      if LongWordParam >= 1 * 1024 * 1024 then
        writeln('Download speed, Mb/s: ':25,
          FormatFloat('0.##', Double(LongWordParam / (1024 * 1024))))
      else if LongWordParam >= 1 * 1024 then
        writeln('Download speed, Kb/s: ':25,
          FormatFloat('0.##', Double(LongWordParam / 1024)))
      else
        writeln('Download speed, b/s: ':25, LongWordParam);
    end;

    if HasOption('s', 'show-content') then
    begin
      writeln();
      writeln('-=== Content ===-');

      LongintParam := Length(string(PChar(FBuffer.Memory)));
      Content := '';
      SetLength(Content, LongintParam);
      Move(PChar(FBuffer.Memory^), PChar(Content)[0], LongintParam);
      writeln(Content);
    end;
  end else
    writeln(ErrorBuffer);

  FreeAndNil(NonOptions);
  Terminate;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FBuffer := TMemoryStream.Create;
  FCURL := curl_easy_init;
end;

destructor TApplication.Destroy;
begin
  curl_easy_cleanup(FCURL);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

class function TApplication.WriteFunctionCallback(ptr: PChar; size: LongWord;
  nmemb: LongWord; data: Pointer): LongWord; cdecl;
begin
  Result := TApplication(data).Write(ptr, size, nmemb);
end;

function TApplication.Write(ptr: PChar; size: LongWord; nmemb: LongWord)
  : LongWord;
begin
  Result := FBuffer.Write(ptr^, size * nmemb);
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
'(*                  --download-speed      write download speed                *)'+ sLineBreak +
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

