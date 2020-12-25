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

unit curl.session.property_modules.sock5;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session.property_module, curl.session.sock5_authmethod;

type
  TModuleSock5 = class(TPropertyModule)
  protected
    { Set allowed methods for SOCKS5 proxy authentication. }
    procedure SetAuthType (AType : TSock5Auth);

    { Proxy authentication service name. }
    procedure SetGSSAPIService (AName : String);

    { Set socks proxy gssapi negotiation protection. }
    procedure SetGSSAPIProtection (AEnable : Boolean);
  protected
    { Tell libcurl which authentication method(s) are allowed for SOCKS5 
      proxy authentication. }
    property AuthType : TSock5Auth write SetAuthType;

    { Proxy authentication service name. }
    property GSSAPIServiceName : String write SetGSSAPIService 
      default 'rcmd';

    { As part of the gssapi negotiation a protection mode is negotiated. 
      The RFC 1961 says in section 4.3/4.4 it should be protected, but the 
      NEC reference implementation does not. If enabled, this option allows 
      the unprotected exchange of the protection mode negotiation. }
    property GSSAPIProtection : Boolean write SetGSSAPIProtection;
  end;

implementation

{ TModuleSock5 }

procedure TModuleSock5.SetAuthType (AType : TSock5Auth);
var
  bitmask : Longint = 0;
begin
  if AUTH_BASIC in AType then
    bitmask := bitmask or CURLAUTH_BASIC;
  if AUTH_GSSAPI in AType then
    bitmask := bitmask or CURLAUTH_GSSAPI;

  Option(CURLOPT_SOCKS5_AUTH, bitmask);
end;

procedure TModuleSock5.SetGSSAPIService (AName : String);
begin
  Option(CURLOPT_SOCKS5_GSSAPI_SERVICE, AName);
end;

procedure TModuleSock5.SetGSSAPIProtection (AEnable : Boolean);
begin
  Option(CURLOPT_SOCKS5_GSSAPI_NEC, AEnable);
end;

end.