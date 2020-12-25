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

unit curl.session.property_modules.dns;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, utils.timeinterval,
  curl.session.property_module;

type
  TModuleDNS = class(TPropertyModule)
  protected
    { Enable global DNS cache. }
    procedure SetGlobalCache (AEnable : Boolean);

    { Set life-time for DNS cache entries. }
    procedure SetCacheTimeout (AInterval : TTimeInterval);

    { Provide the DNS-over-HTTPS URL server. }
    procedure SetDOHServer (AUrl : String);
  protected
    { Enable global DNS cache.
      It tells curl to use a global DNS cache that will survive between easy 
      handle creations and deletions. This is not thread-safe and this will use 
      a global variable. }
    property GlobalCache : Boolean write SetGlobalCache default False;

    { Set life-time for DNS cache entries. 
      This sets the timeout in seconds. Name resolves will be kept in memory and 
      used for this number of seconds. Set to zero to completely disable 
      caching, or set to -1 to make the cached entries remain forever. By 
      default, libcurl caches this info for 60 seconds. }
    property CacheTimeout : TTimeInterval write SetCacheTimeout;

    { Provide the DNS-over-HTTPS URL server.
      Pass in a URL for the DOH server to use for name resolving. The parameter 
      should be a string which must be URL-encoded in the following format: 
      "https://host:port/path". It MUST specify a HTTPS URL. }
    property DOHServer : String write SetDOHServer;
  end;

implementation

{ TModuleDNS }

procedure TModuleDNS.SetGlobalCache (AEnable : Boolean);
begin
  Option(CURLOPT_DNS_USE_GLOBAL_CACHE, AEnable);
end;

procedure TModuleDNS.SetCacheTimeout (AInterval : TTimeInterval);
begin
  Option(CURLOPT_DNS_CACHE_TIMEOUT, Longint(AInterval.Seconds));
end;

procedure TModuleDNS.SetDOHServer (AURL : String);
begin
  Option(CURLOPT_DOH_URL, AUrl);
end;

end.