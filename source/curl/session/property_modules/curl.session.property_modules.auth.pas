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

unit curl.session.property_modules.auth;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session.property_module;

type
  TModuleAuth = class(TPropertyModule)
  protected
    { User name to use in authentication. }
    procedure SetUserName (AName : String);

    { Password to use in authentication. }
    procedure SetPassword (APass : String);
  protected
    { User name to use in authentication. 
      Pass a string as parameter, which should be pointing to the user name to 
      use for the transfer. }
    property Username : String write SetUserName;

    { Password to use in authentication. 
      Pass a string as parameter, which should be pointing to the password to 
      use for the transfer. }
    property Password : String write SetPassword;
  end;

implementation

{ TModuleAuth }

procedure TModuleAuth.SetUserName (AName : String);
begin
  Option(CURLOPT_USERNAME, AName);
end;

procedure TModuleAuth.SetPassword (APass : String);
begin
  Option(CURLOPT_PASSWORD, APass);
end;

end.
