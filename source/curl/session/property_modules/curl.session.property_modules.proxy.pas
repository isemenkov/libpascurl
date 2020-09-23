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

unit curl.session.property_modules.proxy;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session.property_module;

type
  TModuleProxy = class(TPropertyModule)
  protected
    { Set proxy to use. }
    procedure SetUrl (AUrl : String);

    { Port number the proxy listens on. }
    procedure SetPort (APort : Word);
  protected
    { Set proxy to use. 
      Set the proxy to use for the upcoming request. The parameter should be a 
      string holding the host name or dotted numerical IP address. A numerical 
      IPv6 address must be written within [brackets].
      To specify port number in this string, append :[port] to the end of the 
      host name. If not specified, libcurl will default to using port 1080 for 
      proxies.
      
      The proxy string may be prefixed with [scheme]:// to specify which kind of 
      proxy is used.    
      
      http://
      HTTP Proxy. Default when no scheme or proxy type is specified.

      https://
      HTTPS Proxy. (Added in 7.52.0 for OpenSSL, GnuTLS and NSS)

      socks4://
      SOCKS4 Proxy.

      socks4a://
      SOCKS4a Proxy. Proxy resolves URL hostname.

      socks5://
      SOCKS5 Proxy.

      socks5h://
      SOCKS5 Proxy. Proxy resolves URL hostname. }
    property Url : String write SetUrl;

    { Port number the proxy listens on.
      Pass the proxy port to connect to unless it is specified in the proxy URL 
      or uses 443 for https proxies and 1080 for all others as default. }
    property Port : Word write SetPort;
  end;

implementation

{ TModuleProxy }

procedure TModuleProxy.SetUrl (AUrl : String);
begin
  Option(CURLOPT_PROXY, AUrl);
end;

procedure TModuleProxy.SetPort (APort : Word);
begin
  Option(CURLOPT_PROXYPORT, Longint(APort));
end;

end.
