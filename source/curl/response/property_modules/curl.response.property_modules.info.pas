(******************************************************************************)
(*                                 libPasCURL                                 *)
(*            delphi and object pascal wrapper around cURL library            *)
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

unit curl.response.property_modules.info;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, utils.timeinterval, 
  curl.response.property_module;

type
  TModuleInfo = class(TPropertyModule)
  protected
    { Get number of created connections }
    function GetConnectionsCount : Cardinal;

    { Get IP address of last connection }
    function GetConnectedIP : String;

    { Get the latest destination port number }
    function GetConnectedPort : Word;

    { Get local IP address of last connection }
    function GetLocalIP : String;

    { Get the latest local port number }
    function GetLocalPort : Word;

    { Get the last socket used
    If the socket is no longer valid, -1 is returned. }
    function GetLastSocket : curl_socket_t;

    { Get the active socket }
    function GetActiveSocket : curl_socket_t;
  protected
    { Receive how many new connections libcurl had to create to achieve the 
      previous transfer. }
    property ConnectionsCount : Cardinal read GetConnectionsCount;

    { Receive the IP address of the recent connection done. }
    property ConnectedIP : String read GetConnectedIP;

    { Receive the destination port of the recent connection done. }
    property ConnectedPort : Word read GetConnectedPort;

    { Receive the string holding the IP address of the local end of recent 
      connection done. }
    property LocalIP : String read GetLocalIP;

    { Receive the local port number of the recent connection done. }
    property LocalPort : Word read GetLocalPort;

    { Receive the last socket used by this session. If the socket is no longer 
      valid, -1 is returned. }
    property LastSocket : curl_socket_t read GetLastSocket;

    { Receive the recently active socket used for the transfer connection by 
      this session. If the socket is no longer valid, -1 is returned. }
    property ActiveSocket : curl_socket_t  read GetActiveSocket;
  end;

implementation

{ TModuleInfo }

function TModuleInfo.GetConnectionsCount : Cardinal;
begin
  Result := GetLongintValue(CURLINFO_NUM_CONNECTS);
end;

function TModuleInfo.GetConnectedIP : String;
begin
  Result := GetStringValue(CURLINFO_PRIMARY_IP);
end;

function TModuleInfo.GetConnectedPort : Word;
begin
  Result := GetLongintValue(CURLINFO_PRIMARY_PORT);
end;

function TModuleInfo.GetLocalIP : String;
begin
  Result := GetStringValue(CURLINFO_LOCAL_IP);
end;

function TModuleInfo.GetLocalPort : Word;
begin
  Result := GetLongintValue(CURLINFO_LOCAL_PORT);
end;

function TModuleInfo.GetLastSocket : curl_socket_t;
begin
  Result := curl_socket_t(GetLongintValue(CURLINFO_LASTSOCKET));
end;

function TModuleInfo.GetActiveSocket : curl_socket_t;
begin
  Result := curl_socket_t(GetLongintValue(CURLINFO_ACTIVESOCKET));
end;

end.