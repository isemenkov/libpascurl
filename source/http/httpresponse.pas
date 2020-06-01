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
      private
        constructor Create;
        destructor Destroy;
      private
        FErrorStack : TErrorStack;
        FErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
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

      { HTTP(S) session headers }
      THeader = class
      public
        { Get the CONNECT response code
          Received HTTP proxy response code to a CONNECT request. }
        function ConnectResponseCode : THTTPStatusCode;
          {$IFNDEF DEBUG}inline;{$EDNIF}

        { Get the response code }
        function ResponseCode : THTTPStatusCode;
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

function THTTPResponse.THeader.ConnectResponseCode : THTTPStatusCode;
var
  Code : Longint = 0;
begin
  FErrors.Push(curl_easy_getinfo(FCurl, CURLINFO_HTTP_CONNECTCODE, @Code);
  Result := THTTPStatusCode(Code);
end;

function THTTPResponse.THeader.ResponseCode : THTTPStatusCode;
var
  Code : Longint = 0;
begin
  FErrors.Push(curl_easy_getinfo(FCurl, CURLINFO_RESPONSE_CODE, @Code);
  Result := THTTPStatusCode(Code);
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

end.

