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

unit curl.response.timeout;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses 
  curl.utils.errorstack, utils.timeinterval, libpascurl;

type
  { Response timeouts }
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

implementation

{ TTimeout }

constructor TTimeout.Create(ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function TTimeout.Total : TTimeInterval;
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

function TTimeout.NameLookup : TTimeInterval;
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

function TTimeout.Connect : TTimeInterval;
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

function TTimeout.AppConnect : TTimeInterval;
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

function TTimeout.PreTransfer : TTimeInterval;
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

function TTimeout.StartTransfer : TTimeInterval;
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