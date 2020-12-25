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

unit curl.http.response.http_version;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl;

type
  { HTTP protocol version }
  THTTPVersion = (
    HTTP_UKNOWN                  = Longint(CURL_HTTP_VERSION_NONE),
    { Enforce HTTP 1.0 requests. }
    HTTP_1_0                     = Longint(CURL_HTTP_VERSION_1_0),
    { Enforce HTTP 1.1 requests. }
    HTTP_1_1                     = Longint(CURL_HTTP_VERSION_1_1),
    { Attempt HTTP 2 requests. Will fall back to HTTP 1.1 if
    HTTP 2 can't be negotiated with the server. }
    HTTP_2_0                     = Longint(CURL_HTTP_VERSION_2_0),
    { Attempt HTTP 2 over TLS (HTTPS) only. Will fall back to
        HTTP 1.1 if HTTP 2 can't be negotiated with the HTTPS server. For
        clear text HTTP servers, libcurl will use 1.1. }
    HTTP_2_0_TLS                 = Longint(CURL_HTTP_VERSION_2TLS),
    { Issue non-TLS HTTP requests using HTTP/2 without HTTP/1.1 Upgrade.
    It requires prior knowledge that the server supports HTTP/2
    straight away. HTTPS requests will still do HTTP/2 the standard
    way with negotiated protocol version in the TLS handshake. }
    HTTP_2_PRIOR_KNOWEDGE        = Longint(CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE),
    { Setting this value will make libcurl attempt to use HTTP/3
    directly to server given in the URL. Note that this cannot
    gracefully downgrade to earlier HTTP version if the server doesn't
    support HTTP/3. For more reliably upgrading to HTTP/3, set the
    preferred version to something lower and let the server announce
    its HTTP/3 support via Alt-Svc:. }
    HTTP_3_0                     = Longint(CURL_HTTP_VERSION_3)
  );

implementation

end.
