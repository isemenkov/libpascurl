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

unit curl.http.session.property_modules.auth;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.http.auth_methods, 
  curl.session.property_modules.auth;

type
  TModuleAuth = class(curl.session.property_modules.auth.TModuleAuth)
  protected
    { Set HTTP server authentication methods to try. }
    procedure SetAuthMethods (AMethods : TAuthMethods);
  public
    { User name to use in authentication. }
    property Username;

    { Password to use in authentication. }
    property Password;

    { Set HTTP server authentication methods to try. }
    property AuthMethod : TAuthMethods write SetAuthMethods;
  end;

implementation

{ TModuleAuth }

procedure TModuleAuth.SetAuthMethods (AMethods : TAuthMethods);
var
  Bitmask : Cardinal;
begin
  Bitmask := 0;

  if AUTH_BASIC in AMethods then
    Bitmask := Bitmask or CURLAUTH_BASIC;
  if AUTH_DIGEST in AMethods then
    Bitmask := Bitmask or CURLAUTH_DIGEST;
  if AUTH_DIGEST_IE in AMethods then
    Bitmask := Bitmask or CURLAUTH_DIGEST_IE;
  if AUTH_BEARER in AMethods then
    Bitmask := Bitmask or CURLAUTH_BEARER;
  if AUTH_NEGOTIATE in AMethods then
    Bitmask := Bitmask or CURLAUTH_NEGOTIATE;
  if AUTH_NTLM in AMethods then
    Bitmask := Bitmask or CURLAUTH_NTLM;
  if AUTH_NTLM_WB in AMethods then
    Bitmask := Bitmask or CURLAUTH_NTLM_WB;
  if AUTH_ONLY in AMethods then
    Bitmask := Bitmask or CURLAUTH_ONLY;

  Option(CURLOPT_HTTPAUTH, Bitmask);
end;

end.