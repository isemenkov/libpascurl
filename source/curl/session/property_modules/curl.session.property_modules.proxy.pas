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

unit curl.session.property_modules.proxy;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session.property_module, curl.session.proxy_types;

type
  TModuleProxy = class(TPropertyModule)
  protected
    { Set proxy to use. }
    procedure SetUrl (AUrl : String);

    { Port number the proxy listens on. }
    procedure SetPort (APort : Word);

    { Proxy protocol type. }
    procedure SetType (AType : TProxyType);

    { Set pre-proxy to use. }
    procedure SetPreProxy (AUrl : String);

    { Tunnel through HTTP proxy. }
    procedure SetHTTPProxyTunnel (AEnable : Boolean);
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

    { Proxy protocol type. }
    property ProxyType : TProxyType write SetType;

    { Set pre-proxy to use.
      Set the preproxy to use for the upcoming request. The parameter should be 
      a string holding the host name or dotted numerical IP address. A numerical 
      IPv6 address must be written within [brackets].
      To specify port number in this string, append :[port] to the end of the 
      host name.
      A pre proxy is a SOCKS proxy that curl connects to before it connects to 
      the HTTP(S) proxy specified in the CURLOPT_PROXY option. The pre proxy can 
      only be a SOCKS proxy.
      The pre proxy string should be prefixed with [scheme]:// to specify which 
      kind of socks is used. Use socks4://, socks4a://, socks5:// or socks5h:// 
      (the last one to enable socks5 and asking the proxy to do the resolving, 
      also known as CURLPROXY_SOCKS5_HOSTNAME type) to request the specific 
      SOCKS version to be used. Otherwise SOCKS4 is used as default.
      Setting the pre proxy string to "" (an empty string) will explicitly 
      disable the use of a pre proxy. }
    property PreProxy : String write SetPreProxy;

    { Tunnel through HTTP proxy. 
      Set the tunnel parameter to True to make libcurl tunnel all operations 
      through the HTTP proxy (set with Url). There is a big difference between 
      using a proxy and to tunnel through it.
      Tunneling means that an HTTP CONNECT request is sent to the proxy, asking 
      it to connect to a remote host on a specific port number and then the 
      traffic is just passed through the proxy. Proxies tend to white-list 
      specific port numbers it allows CONNECT requests to and often only port 80 
      and 443 are allowed.
      HTTP proxies can generally only speak HTTP (for obvious reasons), which 
      makes libcurl convert non-HTTP requests to HTTP when using an HTTP proxy 
      without this tunnel option set. For example, asking for an FTP URL and
      specifying an HTTP proxy will make libcurl send an FTP URL in an HTTP GET 
      request to the proxy. By instead tunneling through the proxy, you avoid 
      that conversion (that rarely works through the proxy anyway). }
    property HTTPProxyTunnel : Boolean write SetHTTPProxyTunnel default False;
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

procedure TModuleProxy.SetType (AType : TProxyType);
var
  proxy_type : Longint;
begin
  proxy_type := 0;

  case AType of
    PROXY_TYPE_HTTP     : begin proxy_type := Longint(CURLPROXY_HTTP);     end;
    PROXY_TYPE_HTTPS    : begin proxy_type := Longint(CURLPROXY_HTTPS);    end;
    PROXY_TYPE_HTTP_1_0 : begin proxy_type := Longint(CURLPROXY_HTTP_1_0); end;
    PROXY_TYPE_SOCKS4   : begin proxy_type := Longint(CURLPROXY_SOCKS4);   end;
    PROXY_TYPE_SOCKS4A  : begin proxy_type := Longint(CURLPROXY_SOCKS4A);  end;
    PROXY_TYPE_SOCK5    : begin proxy_type := Longint(CURLPROXY_SOCKS5);   end;
    PROXY_TYPE_SOCKS5_HOSTNAME : begin
      proxy_type := Longint(CURLPROXY_SOCKS5_HOSTNAME);
    end;
  end;

  Option(CURLOPT_PROXYTYPE, proxy_type);
end;

procedure TModuleProxy.SetPreProxy (AUrl : String);
begin
  Option(CURLOPT_PRE_PROXY, AUrl);
end;

procedure TModuleProxy.SetHTTPProxyTunnel (AEnable : Boolean);
begin
  Option(CURLOPT_HTTPPROXYTUNNEL, AEnable);
end;

end.
