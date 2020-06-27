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

unit curl.redirect;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  curl.utils.errorstack, utils.timeinterval, libpascurl;

type
  { Response redirected options }
  TRedirect = class
  public
    { Return true if request is redirected }
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

implementation

{ THTTPResponse.TRedirect }

constructor TRedirect.Create (ACurl : CURL;
  AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErorrs;
end;

function TRedirect.IsRedirected : Boolean;
begin
  Result := Count > 0;
end;

function TRedirect.Count : Longint;
begin
  Result := 0;
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_REDIRECT_COUNT, @Result));
end;

function TRedirect.Url : String;
var
  url : PChar;
begin
  New(url);
  url := '';
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_REDIRECT_URL, @url));
  Result := url;
end;

function TRedirect.TotalTime : TTimeInterval;
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

end.