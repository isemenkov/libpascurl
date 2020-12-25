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

unit curl.http.response.property_modules.header;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.response.property_modules.header,
  curl.http.response.http_version, curl.http.response.status_code;

type
  TModuleHeader = class(curl.response.property_modules.header.TModuleHeader)
  protected
    { Get the response code. }
    function GetResponseCode : THTTPStatusCode;

    { Get the CONNECT response code. }
    function GetConnectResponseCode : THTTPStatusCode;

    { Get the http version used in the connection. }
    function GetHttpVersion : THTTPVersion;
  public
    { Get the response code. }
    property ResponseCode : THTTPStatusCode read GetResponseCode;

    { Get the CONNECT response code.
      Receive the last received HTTP proxy response code to a CONNECT request. }
    property ConnectResponseCode : THTTPStatusCode read GetConnectResponseCode;

    { Get the http version used in the connection. }
    property HTTPVersion : THTTPVersion read GetHttpVersion;

    { Get size of retrieved headers. }
    property Length;

    { Get Content-Type. }
    property ContentType;
  end;

implementation

{ TModuleHeader }

function TModuleHeader.GetResponseCode : THTTPStatusCode;
begin
  Result := THTTPStatusCode(inherited GetResponseCode);
end;

function TModuleHeader.GetConnectResponseCode : THTTPStatusCode;
begin
  Result := THTTPStatusCode(GetLongintValue(CURLINFO_HTTP_CONNECTCODE));
end;

function TModuleHeader.GetHttpVersion : THTTPVersion;
begin
  Result := THTTPVersion(GetLongintValue(CURLINFO_HTTP_VERSION));
end;

end.
