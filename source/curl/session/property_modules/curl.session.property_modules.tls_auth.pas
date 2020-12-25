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

unit curl.session.property_modules.tls_auth;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session.tlsauth_methods, 
  curl.session.property_module;

type
  TModuleTLSAuth = class(TPropertyModule)
  protected
    { Set TLS authentication method. }
    procedure SetAuthMethod (AMethod : TTLSAuthMethod);

    { User name to use for TLS authentication. }
    procedure SetUserName (AName : String);

    { Password to use for TLS authentication. }
    procedure SetPassword (APass : String);
  protected
    { Set TLS authentication method. }
    property AuthMethod : TTLSAuthMethod write SetAuthMethod;

    { User name to use for TLS authentication. 
      Pass a string as parameter, which should be pointing to the user name to 
      use for the TLS authentification method sepecified. }
    property Username : String write SetUserName;

    { Password to use for TLS authentication. 
      Pass a string as parameter, which should be pointing to the password to 
      use for the TLS authentification method specified. }
    property Password : String write SetPassword;
  end;

implementation

{ TModuleAuth }

procedure TModuleTLSAuth.SetAuthMethod (AMethod : TTLSAuthMethod);
var
  method : String;
begin
  method := '';

  case AMethod of
    AUTH_SRP : begin method := 'SRP'; end;
  end;

  Option(CURLOPT_TLSAUTH_TYPE, method);  
end;

procedure TModuleTLSAuth.SetUserName (AName : String);
begin
  Option(CURLOPT_TLSAUTH_USERNAME, AName);
end;

procedure TModuleTLSAuth.SetPassword (APass : String);
begin
  Option(CURLOPT_TLSAUTH_PASSWORD, APass);
end;

end.
