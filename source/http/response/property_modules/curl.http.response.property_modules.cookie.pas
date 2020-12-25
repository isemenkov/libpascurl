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

unit curl.http.response.property_modules.cookie;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, curl.utils.errors_stack, curl.utils.cookies_list,
  curl.response.property_module;

type
  TModuleCookie = class(TPropertyModule)
  protected
    type
      PCookiesList = ^TCookiesList;
  public
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack;
      ACookiesList : PCookiesList);
  protected
    FCookiesList : PCookiesList;
  end;

implementation

{ TModuleCookie }

constructor TModuleCookie.Create(ACURL : libpascurl.CURL; AErrorsStack :
  PErrorsStack; ACookiesList : PCookiesList);
begin
  inherited Create(ACURL, AErrorsStack);

  FCookiesList := ACookiesList;
  FCookiesList^.Append(GetListValue(CURLINFO_COOKIELIST));
end;

end.
