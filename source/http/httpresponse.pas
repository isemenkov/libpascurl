(******************************************************************************)
(*                                 libPasCURL                                 *)
(*                 object pascal wrapper around cURL library                  *)
(*                        https://github.com/curl/curl                        *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* Module:          Unit 'pascurl'                                            *)
(* Functionality:   Provides THTTPResponse class                              *)
(*                                                                            *)
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

unit httpresponse;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils, libpascurl, curlresult, timeinterval, datasize, errorstack,
  httpstatuscode;

type
  { HTTP(S) session result response data }
  THTTPResponse = class
  public
    type
      { HTTP(S) session errors }
      TError = class
      public
        { Return TRUE if has errors }
        function HasErrors : Boolean;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return error stack }
        function Errors : TErrorStack;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return errors enumerator }
        function GetEnumerator : TErrorsEnumerator;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      private
        constructor Create;
        destructor Destroy;
      private
        FErrorStack : TErrorStack;
        FErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
      end;

      { HTTP(S) session request }
      TRequest = class
      public
        { Get size of sent request }
        function Length : TDataSize;
      private
        constructor Create(ACurl : CURL; AErrors : PErrorStack);
      private
        FCurl : CURL;
        FErrors : PErrorStack;
      end;

      { HTTP(S) session headers }
      THeader = class
      public
        type
          { HTTP protocol version }
          THTTPVersion = (
            HTTP_UKNOWN                   = CURL_HTTP_VERSION_NONE,
            { Enforce HTTP 1.0 requests. }
            HTTP_1_0                      = CURL_HTTP_VERSION_1_0,
            { Enforce HTTP 1.1 requests. }
            HTTP_1_1                      = CURL_HTTP_VERSION_1_1,
            { Attempt HTTP 2 requests. Will fall back to HTTP 1.1 if
              HTTP 2 can't be negotiated with the server. }
            HTTP_2_0                      = CURL_HTTP_VERSION_2_0,
            { Attempt HTTP 2 over TLS (HTTPS) only. Will fall back to
              HTTP 1.1 if HTTP 2 can't be negotiated with the HTTPS server. For
              clear text HTTP servers, libcurl will use 1.1. }
            HTTP_2_0_TLS                  = CURL_HTTP_VERSION_2TLS,
            { Issue non-TLS HTTP requests using HTTP/2 without HTTP/1.1 Upgrade.
              It requires prior knowledge that the server supports HTTP/2
              straight away. HTTPS requests will still do HTTP/2 the standard
              way with negotiated protocol version in the TLS handshake. }
            HTTP_2_PRIOR_KNOWEDGE         = CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE,
            { Setting this value will make libcurl attempt to use HTTP/3
              directly to server given in the URL. Note that this cannot
              gracefully downgrade to earlier HTTP version if the server doesn't
              support HTTP/3. For more reliably upgrading to HTTP/3, set the
              preferred version to something lower and let the server announce
              its HTTP/3 support via Alt-Svc:. }
            HTTP_3_0                      = CURL_HTTP_VERSION_3
          );
      public
        { Get the CONNECT response code
          Received HTTP proxy response code to a CONNECT request. }
        function ConnectStatusCode : THTTPStatusCode;
          {$IFNDEF DEBUG}inline;{$EDNIF}

        { Get the response code }
        function StatusCode : THTTPStatusCode;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get the HTTP version used in the connection }
        function Version : THTTPVersion;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get size of retrieved headers }
        function Length : TDataSize;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      private
        constructor Create (ACurl : CURL; AErrors : PErrorStack);
        destructor Destroy; override;

        { This function gets called by libcurl as soon as it has received header
          data. The header callback will be called once for each header and only
          complete header lines are passed on to the callback. }
        class function HeaderFunctionCallback(buffer : PChar; size : LongWord;
          nitems : LongWord; userdata : Pointer) : LongWord;
        function HeaderCallback (buffer : PChar; size : LongWord; nitems :
          LongWord) : LongWord;
      private
        FCurl : CURL;
        FHeaders : TStringList;
        FErrors : PErrorStack;
      end;

      { HTTP(S) session redirected options }
      TRedirect = class
      public
        { Return TRUE if request is redirected }
        function IsRedirected : Boolean;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return redirect count times }
        function Count : Longint;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return redirected URL }
        function Url : String;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Return the time for all redirection steps }
        function TotalTime : TTimeInterval;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      private
        constructor Create(ACurl : CURL; AErrors : PErrorStack);
      private
        FCurl : CURL;
        FErrors : PErrorStack;
      end;

      TContent = class
      public
        { Get Content-Type
          This is the value read from the Content-Type: field. If you get empty,
          it means that the server didn't send a valid Content-Type header. }
        function ContentType : String;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get content-length of download }
        function Length : TDataSize;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      private
        constructor Create (ACurl : CURL; AErrors : PErrorStack);
      private
        FCurl : CURL;
        FErrors : PErrorStack;
      end;

      { HTTP(S) session timeouts }
      TTimeout = class
      public
        { Get transfer total time }
        function Total : TTimeInterval;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get the name lookup time }
        function NameLookup : TTimeInterval;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get the time until connect }
        function Connect : TTimeInterval;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get the time until the SSL/SSH handshake is completed }
        function AppConnect : TTimeInterval;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get the time until the file transfer start }
        function PreTransfer : TTimeInterval;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get time until the first byte is received }
        function StartTransfer : TTimeInterval;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      private
        constructor Create (ACurl : CURL; AErrors : PErrorStack);
      private
        FCurl : CURL;
        FErrors : PErrorStack;
      end;

      { HTTP(S) session speed data }
      TSpeed = class
      public
        { Get download speed per second }
        function Download : TDataSize;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get upload speed per second }
        function Upload : TDataSize;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      private
        constructor Create (ACurl : CURL; AErrors : PErrorStack);
      private
        FCurl : CURL;
        FErrors : PErrorStack;
      end;

      { Additional response information }
      TInfo = class
      public
        { Get number of created connections }
        function ConnectionsCount : Cardinal;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get IP address of last connection }
        function ConnectedIP : String;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get the latest destination port number }
        function ConnectedPort : Word;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get local IP address of last connection }
        function LocalIP : String;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get the latest local port number }
        function LocalPort : Word;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get the last socket used
          If the socket is no longer valid, -1 is returned. }
        function LastSocket : curl_socket_t;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get the active socket }
        function ActiveSocket : curl_socket_t;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        { Get private pointer }
        function UserData : Pointer;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      private
        constructor Create (ACurl : CURL; AErrors : PErrorStack);
      private
        FCurl : CURL;
        FErrors : PErrorStack;
      end;
  private
    FError : TError;
    FRedirect : TRedirect;
    FTimeout : TTimeout;
  end;

implementation

{ THTTPResponse.TError }

constructor THTTPResponse.TError.Create;
begin
  FErrorStack := TErrorStack.Create;
end;

destructor THTTPResponse.TError.Destroy;
begin
  FreeAndNil(FErrorStack);
end;

function THTTPResponse.TError.HasErrors : Boolean;
begin
  Result := FErrorStack.Count > 0;
end;

function THTTPResponse.TError.Errors : TErrorStack;
begin
  Result := FErrorStack;
end;

function THTTPResponse.TError.GetEnumerator : TErrorsEnumerator;
begin
  Result := FErrorStack.GetEnumerator;
end;

{ THTTPResponse.TRequest }

constructor THTTPResponse.TRequest.Create(ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function THTTPResponse.TRequest.Length : TDataSize;
var
  bytes : Longint = 0;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_REQUEST_SIZE, @bytes);
  Result := TDataSize.Create;
  Result.Bytes := bytes;
end;

{ THTTPResponse.THeader }

constructor THTTPResponse.THeader.Create(ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FHeaders := TStringList.Create;
  FErrors := AErrors;
end;

destructor THTTPResponse.THeader.Destroy;
begin
  FreeAndNil(AHeader);
  inherited Destroy;
end;

class function THTTPResponse.THeader.HeaderFunctionCallback(buffer : PChar;
  size : LongWord; nitems : LongWord; userdata : Pointer) : LongWord;
begin
  Result := THeader(userdata).HeaderCallback(buffer, size, nitems);
end;

function THTTPResponse.THeader.HeaderCallback(buffer : PChar; size : LongWord;
  nitems : LongWord) : LongWord;
begin
  FHeaders.Add(buffer);
  Result := size * nitems;
end;

function THTTPResponse.THeader.ConnectStatusCode : THTTPStatusCode;
var
  Code : Longint = 0;
begin
  FErrors.Push(curl_easy_getinfo(FCurl, CURLINFO_HTTP_CONNECTCODE, @Code);
  Result := THTTPStatusCode(Code);
end;

function THTTPResponse.THeader.StatusCode : THTTPStatusCode;
var
  Code : Longint = 0;
begin
  FErrors.Push(curl_easy_getinfo(FCurl, CURLINFO_RESPONSE_CODE, @Code);
  Result := THTTPStatusCode(Code);
end;

function THTTPResponse.THeader.Version : THTTPVersion;
var
  version : Longint = 0;
begin
  FErrors.Push(curl_easy_getinfo(FCurl, CURLINFO_HTTP_VERSION, @version);
  Result := THTTPVersion(version);
end;

function THTTPResponse.THeader.Length : TDataSize;
var
  bytes : LongWord = 0;
begin
  FErrors.Push(curl_easy_getinfo(FCurl, CURLINFO_HEADER_SIZE, @bytes);
  Result := TDataSize.Create;
  Result.Bytes := bytes;
end;

{ THTTPResponse.TRedirect }

constructor THTTPResponse.TRedirect.Create (ACurl : CURL;
  AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErorrs;

  FollowRedirect := True;
end;

function THTTPResponse.TRedirect.IsRedirected : Boolean;
begin
  Result := Count > 0;
end;

function THTTPResponse.TRedirect.Count : Longint;
begin
  Result := 0;
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_REDIRECT_COUNT, @Result));
end;

function THTTPResponse.TRedirect.Url : String;
var
  url : PChar;
begin
  New(url);
  url := '';
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_REDIRECT_URL, @url));
  Result := url;
end;

function THTTPResponse.TRedirect.TotalTime : TTimeInterval;
var
  time : Longword = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_REDIRECT_TIME_T, @time);
  Result := TTimeInterval.Create;
  Result.Milliseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_REDIRECT_TIME, @dtime);
    Result.Milliseconds := ceil(dtime);
  end;

  FErrors^.Push(CurlResult);
end;

{ THTTPResponse.TContent }

constructor THTTPResponse.TContent.Create(ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function THTTPResponse.TContent.ContentType : String;
var
  ctype : PChar;
begin
  New(ctype);
  ctype := '';
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_CONTENT_TYPE, @ctype));
  Result := ctype;
end;

function THTTPResponse.TContent.Length : TDataSize;
var
  size : Longword = 0;
  dsize : Double = 0;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T,
    @size);
  Result := TDataSize.Create
  Result.Bytes := size;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_CONTENT_LENGTH_DOWNLOAD,
      @dsize);
    Result.Bytes := ceil(dsize);
  end;

  FErrors^.Push(CurlResult);
end;

{ THTTPResponse.TTimeout }

constructor THTTPResponse.TTimeout.Create(ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function THTTPResponse.TTimeout.Total : TTimeInterval;
var
  time : Longword = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_TOTAL_TIME_T, @time);
  Result := TTimeInterval.Create;
  Result.Milliseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_TOTAL_TIME, @dtime);
    Result.Milliseconds := ceil(dtime);
  end;

  FErrors^.Push(CurlResult);
end;

function THTTPResponse.TTimeout.NameLookup : TTimeInterval;
var
  time : Longword = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_NAMELOOKUP_TIME_T, @time);
  Result := TTimeInterval.Create;
  Result.Milliseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_NAMELOOKUP_TIME, @dtime);
    Result.Milliseconds := ceil(dtime);
  end;

  FErrors^.Push(CurlResult);
end;

function THTTPResponse.TTimeout.Connect : TTimeInterval;
var
  time : Longword = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_CONNECT_TIME_T, @time);
  Result := TTimeInterval.Create;
  Result.Milliseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_CONNECT_TIME, @dtime);
    Result.Milliseconds := ceil(dtime);
  end;

  FErrors^.Push(CurlResult);
end;

function THTTPResponse.TTimeout.AppConnect : TTimeInterval;
var
  time : Longword = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_APPCONNECT_TIME_T, @time);
  Result := TTimeInterval.Create;
  Result.Milliseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_APPCONNECT_TIME, @dtime);
    Result.Milliseconds := ceil(dtime);
  end;

  FErrors^.Push(CurlResult);
end;

function THTTPResponse.TTimeout.PreTransfer : TTimeInterval;
var
  time : Longword = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_PRETRANSFER_TIME_T, @time);
  Result := TTimeInterval.Create;
  Result.Milliseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_PRETRANSFER_TIME, @dtime);
    Result.Milliseconds := ceil(dtime);
  end;

  FErrors^.Push(CurlResult);
end;

function THTTPResponse.TTimeout.StartTransfer : TTimeInterval;
var
  time : Longword = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_STARTTRANSFER_TIME_T, @time);
  Result := TTimeInterval.Create;
  Result.Milliseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_STARTTRANSFER_TIME, @dtime);
    Result.Milliseconds := ceil(dtime);
  end;

  FErrors^.Push(CurlResult);
end;

{ THTTPResponse.TSpeed }

constructor THTTPResponse.TSpeed.Create (ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function THTTPResponse.TSpeed.Download : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_SPEED_DOWNLOAD_T, @bytes);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_SPEED_DOWNLOAD, @dbytes);
    Result.Bytes := ceil(dbytes);
  end;

  FErrors^.Push(CurlResult);
end;

function THTTPResponse.TSpeed.Upload : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_SPEED_UPLOAD_T, @bytes);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_SPEED_UPLOAD, @dbytes);
    Result.Bytes := ceil(dbytes);
  end;

  FErrors^.Push(CurlResult);
end;

{ THTTPResponse.TInfo }

constructor THTTPResponse.TInfo.Create (ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function THTTPResponse.TInfo.ConnectionsCount : Cardinal;
var
  count : Longint;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_NUM_CONNECTS, @count);
  Result := count;
end;

function THTTPResponse.TInfo.ConnectedIP : String;
var
  ip : PChar;
begin
  New(ip);
  ip := '';
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_PRIMARY_IP, @ip));
  Result := ip;
end;

function THTTPResponse.TInfo.ConnectedPort : Word;
var
  port : Longint;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_PRIMARY_PORT, @port));
  Result := port;
end;

function THTTPResponse.TInfo.LocalIP : String;
var
  ip : PChar;
begin
  New(ip);
  ip := '';
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_LOCAL_IP, @ip));
  Result := ip;
end;

function THTTPResponse.TInfo.LocalPort : Word;
var
  port : Longint;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_LOCAL_PORT, @port));
  Result := port;
end;

function THTTPResponse.TInfo.LastSocket : curl_socket_t;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_LASTSOCKET, @Result));
end;

function THTTPResponse.TInfo.ActiveSocket : curl_socket_t;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_ACTIVESOCKET, @Result));
end;

function THTTPResponse.TInfo.UserData : Pointer;
var
  data : PPChar = nil;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_PRIVATE, data));
  if data <> nil then
    Result := data^
  else
    Result := nil;
end;

end.

