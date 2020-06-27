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

unit curl.response.info;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  curl.utils.errorstack, libpascurl;

type
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

implementation

{ THTTPResponse.TInfo }

constructor TInfo.Create (ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function TInfo.ConnectionsCount : Cardinal;
var
  count : Longint;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_NUM_CONNECTS, @count));
  Result := count;
end;

function TInfo.ConnectedIP : String;
var
  ip : PChar;
begin
  New(ip);
  ip := '';
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_PRIMARY_IP, @ip));
  Result := ip;
end;

function TInfo.ConnectedPort : Word;
var
  port : Longint;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_PRIMARY_PORT, @port));
  Result := port;
end;

function TInfo.LocalIP : String;
var
  ip : PChar;
begin
  New(ip);
  ip := '';
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_LOCAL_IP, @ip));
  Result := ip;
end;

function TInfo.LocalPort : Word;
var
  port : Longint;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_LOCAL_PORT, @port));
  Result := port;
end;

function TInfo.LastSocket : curl_socket_t;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_LASTSOCKET, @Result));
end;

function TInfo.ActiveSocket : curl_socket_t;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_ACTIVESOCKET, @Result));
end;

function TInfo.UserData : Pointer;
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