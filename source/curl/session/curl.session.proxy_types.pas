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

unit curl.session.proxy_types;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

type
  TProxyType = (
    { HTTP Proxy. Default. } 
    PROXY_TYPE_HTTP,

    { HTTPS Proxy. }
    PROXY_TYPE_HTTPS,

    { HTTP 1.0 Proxy. This is very similar to CURLPROXY_HTTP except it uses 
      HTTP/1.0 for any CONNECT tunnelling. It does not change the HTTP version 
      of the actual HTTP requests. }
    PROXY_TYPE_HTTP_1_0,

    { SOCKS4 Proxy. }
    PROXY_TYPE_SOCKS4,

    { SOCKS4a Proxy. Proxy resolves URL hostname. }
    PROXY_TYPE_SOCKS4A,

    { SOCKS5 Proxy. }
    PROXY_TYPE_SOCK5,

    { SOCKS5 Proxy. Proxy resolves URL hostname. }
    PROXY_TYPE_SOCKS5_HOSTNAME
  );

implementation

end.