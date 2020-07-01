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

unit curl.session.dnsproperty;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  curl.utils.errorstack, utils.timeinterval, libpascurl;

type
  { DNS property }
  TDNSProperty = class
  private
    { Set life-time for DNS cache entries. }
    procedure SetCacheTimeout (ATimeout : TTimeInterval);

    { Enable/disable global DNS cache. }
    procedure SetGlobalCache (AEnable : Boolean);

    { Provide the DNS-over-HTTPS URL. }
    procedure SetDOHUrl (AUrl : String);

    { Set interface to speak DNS over. }
    procedure SetInterfaceName (AName : String);

    { IPv4 address to bind DNS resolves to. }
    procedure SetLocalIP4 (AAdress : String);

    { IPv6 address to bind DNS resolves to. }
    procedure SetLocalIP6 (AAdress : String);

    { Set preferred DNS servers. }
    procedure SetServers (AServers : String);

    { Shuffle addresses when a hostname returns more that one. }
    procedure SetShuffleAdresses (AEnable : Boolean);
  public
    { Set life-time for DNS cache entries. 

      Name resolves will be kept in memory and used for this time interval.
      Set to zero to completely disable caching. }
    property CacheTimeout : TTimeInterval write SetCacheTimeout;

    { Enable/disable global DNS cache. 

      Tells curl to use a global DNS cache that will survive between easy
      handle creations and deletions. This is not thread-safe and this will use 
      a global variable.
         
      WARNING: this option is considered obsolete. Stop using it. Switch over to
      using the share interface instead! }
    property GlobalCache : Boolean write SetGlobalCache;

    { Provide the DNS-over-HTTPS URL.

      Pass in a string to a URL for the DOH server to use for name resolving. 
      The parameter should be a string which must be URL-encoded in the 
      following format: "https://host:port/path". It MUST specify a HTTPS URL. 
      Disable DOH use again by setting this option to '' (empty string). }
    property DNSoverHTTPSServer : String write SetDOHUrl;

    { Set interface to speak DNS over.

      Set the name of the network interface that the DNS resolver should bind 
      to. This must be an interface name (not an address). Set this option to ''
      (empty string) to use the default setting (don't bind to a specific 
      interface). }
    property InterfaceName : String write SetInterfaceName;

    { IPv4 address to bind DNS resolves to.

      Set the local IPv4 address that the resolver should bind to. The argument 
      should be string and contain a single numerical IPv4 address as a string. 
      Set this option to '' (empty string) to use the default setting (don't 
      bind to a specific IP address). }
    property LocalIP4 : String write SetLocalIP4;

    { IPv6 address to bind DNS resolves to.

      Set the local IPv6 address that the resolver should bind to. The argument 
      should be string and contain a single IPv6 address as a string. Set this 
      option to '' (empty string) to use the default setting (don't bind to a 
      specific IP address). }
    property LocalIP6 : String write SetLocalIP6;

    { Set preferred DNS servers.

      Pass a string that is the list of DNS servers to be used instead of the 
      system default. The format of the dns servers option is: 
      host[:port][,host[:port]]...
      For example:
      192.168.1.100,192.168.1.101,3.4.5.6 }
    property Servers : String write SetServers;

    { Shuffle addresses when a hostname returns more that one.
     
      When a name is resolved and more than one IP address is returned, shuffle 
      the order of all returned addresses so that they will be used in a random 
      order. This is similar to the ordering behavior of gethostbyname which is 
      no longer used on most platforms. Addresses will not be reshuffled if a 
      name resolution is completed using the DNS cache. 
      CURLOPT_DNS_CACHE_TIMEOUT can be used together with this option to reduce 
      DNS cache timeout or disable caching entirely if frequent reshuffling is 
      needed. Since the addresses returned will be reordered randomly, their 
      order will not be in accordance with RFC 3484 or any other deterministic
      order that may be generated by the system's name resolution 
      implementation. This may have performance impacts and may cause IPv4 to be
      used before IPv6 or vice versa. }
    property ShuffleAdresses : Boolean write SetShuffleAdresses;
  private
    constructor Create (ACurl : CURL; AErrors : PErrorStack);
  private
    FCurl : CURL;
    FErrors : PErrorStack;    
  end;

implementation

constructor TDNSProperty.Create (ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

procedure TDNSProperty.SetCacheTimeout (ATimeout : TTimeInterval);
begin
  FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_CACHE_TIMEOUT,
    ATimeout.Seconds));
end;

procedure TDNSProperty.SetGlobalCache (AEnable : Boolean);
begin
  FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_USE_GLOBAL_CACHE,
    Longint(AEnable)));
end;

procedure TDNSProperty.SetDOHUrl (AUrl : String);
begin
  if AUrl <> '' then
  begin
    FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DOH_URL, PChar(AUrl)));
  end else
  begin
    FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DOH_URL, 0));
  end;
end;

procedure TDNSProperty.SetInterfaceName (AName : String);
begin
  if AName <> '' then
  begin
    FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_INTERFACE, PChar(AName)));
  end else
  begin
    FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_INTERFACE, 0));
  end;
end;

procedure TDNSProperty.SetLocalIP4 (AAdress : String);
begin
  if AAddress <> '' then
  begin
    FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_LOCAL_IP4, 
      PChar(AAdress)));
  end else 
  begin
    FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_LOCAL_IP4, 0));
  end;
end;

procedure TDNSProperty.SetLocalIP6 (AAdress : String);
begin
  if AAdress <> '' then
  begin
    FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_LOCAL_IP6, 
      PChar(AAdress)));
  end else
  begin
    FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_LOCAL_IP6, 0));
  end;
end;

procedure TDNSProperty.SetServers (AServers : String);
begin
  FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_SERVERS, PChar(AServers)));
end;

procedure TDNSProperty.SetShuffleAdresses (AEnable : Boolean);
begin
  FErrors^.Push(curl_easy_setopt(FCurl, CURLOPT_DNS_SHUFFLE_ADDRESSES,
    Longint(AEnable)));
end;

end.