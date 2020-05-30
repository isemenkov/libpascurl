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
(* Module:          Unit 'pascurl'                                            *)
(* Functionality:   Provides TDNSProperty class                               *)
(*                                                                            *)
(*                                                                            *)
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

unit DNSProperty;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils, libpascurl, errorstack, timeinterval;

type
  TDNSProperty = class
  public
    constructor Create (AHandle : CURL; AErrorStack : PErrorStack);

    procedure SetDNSCacheTimeout (ATimeout : TTimeInterval);
    procedure SetDNSGlobalCache (AEnable : Boolean);
    procedure SetDNSoverHTTPS (AUrl : string);
    procedure SetDNSInterface (AInterface : string);
    procedure SetDNSLocalIP4 (AAddress : string);
    procedure SetDNSLocalIP6 (AAddress : string);
    procedure SetDNSServers (AServers : string);
    procedure SetDNSShuffleAddresses (AEnable : Boolean);
  private
    FHandle : CURL;
    FErrorStack : PErrorStack;
  end;

implementation

constructor TDNSProperty.Create (AHandle : CURL; AErrorStack : PErrorStack);
begin
  FHandle := AHandle;
  FErrorStack := AErrorStack;
end;

procedure TDNSProperty.SetDNSCacheTimeout (ATimeout : TTimeInterval);
begin
  FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_CACHE_TIMEOUT,
    Longint(ATimeout.Seconds)));
end;

procedure TDNSProperty.SetDNSGlobalCache(AEnable: Boolean);
begin
  FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_USE_GLOBAL_CACHE,
    Longint(AEnable)));
end;

procedure TDNSProperty.SetDNSoverHTTPS(AUrl: string);
begin
  if AUrl <> '' then
  begin
    FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DOH_URL, PChar(AUrl)));
  end else
  begin
    FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DOH_URL, 0));
  end;
end;

procedure TDNSProperty.SetDNSInterface(AInterface: string);
begin
  if AInterface <> '' then
    FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_INTERFACE,
      PChar(AInterface)))
  else
    FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_INTERFACE, 0));
end;

procedure TDNSProperty.SetDNSLocalIP4(AAddress: string);
begin
  if AAddress <> '' then
    FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_LOCAL_IP4,
      PChar(AAddress)))
  else
    FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_LOCAL_IP4, 0));
end;

procedure TDNSProperty.SetDNSLocalIP6(AAddress: string);
begin
  if AAddress <> '' then
    FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_LOCAL_IP6,
      PChar(AAddress)))
  else
    FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_LOCAL_IP6, 0));
end;

procedure TDNSProperty.SetDNSServers(AServers: string);
begin
  FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_SERVERS,
    PChar(AServers)));
end;

procedure TDNSProperty.SetDNSShuffleAddresses(AEnable: Boolean);
begin
  FErrorStack^.Push(curl_easy_setopt(FHandle, CURLOPT_DNS_SHUFFLE_ADDRESSES,
    Longint(AEnable)));
end;

end.

