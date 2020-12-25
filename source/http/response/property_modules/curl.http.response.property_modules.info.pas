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

unit curl.http.response.property_modules.info;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  curl.response.property_modules.info;

type
  TModuleInfo = class(curl.response.property_modules.info.TModuleInfo)
  public
    { Receive how many new connections libcurl had to create to achieve the 
      previous transfer. }
    property ConnectionsCount;

    { Receive the IP address of the recent connection done. }
    property ConnectedIP;

    { Receive the destination port of the recent connection done. }
    property ConnectedPort;

    { Receive the string holding the IP address of the local end of recent 
      connection done. }
    property LocalIP;

    { Receive the local port number of the recent connection done. }
    property LocalPort;

    { Receive the last socket used by this session. If the socket is no longer 
      valid, -1 is returned. }
    property LastSocket;

    { Receive the recently active socket used for the transfer connection by 
      this session. If the socket is no longer valid, -1 is returned. }
    property ActiveSocket;
  end;

implementation

end.
