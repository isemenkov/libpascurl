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

unit curl.response.property_modules.redirect;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, utils.timeinterval, curl.response.property_module;

type
  TModuleRedirect = class(TPropertyModule)
  protected
    { Return true if request is redirected. }
    function GetIsRedirected : Boolean;

    { Return redirect count times. }
    function GetCount : LongWord;

    { Return redirected URL. }
    function GetUrl : String;

    { Return the time for all redirection steps. }
    function GetTotalTime : TTimeInterval;
  protected
    { Return true if request is redirected. }
    property IsRedirected : Boolean read GetIsRedirected;

    { Return redirect count times. }
    property Count : LongWord read GetCount;

    { Return redirected URL. }
    property Url : String read GetUrl;

    { Return the time for all redirection steps. }
    property TotalTime : TTimeInterval read GetTotalTime;
  end;

implementation

{ TModuleRedirect }

function TModuleRedirect.GetCount : LongWord;
begin
  Result := GetLongintValue(CURLINFO_REDIRECT_COUNT);
end;

function TModuleRedirect.GetIsRedirected : Boolean;
begin
  Result := Count > 0;
end;

function TModuleRedirect.GetUrl : String;
begin
  Result := GetStringValue(CURLINFO_REDIRECT_URL);
end;

function TModuleRedirect.GetTotalTime : TTimeInterval;
begin
  Result := TTimeInterval.Create;
  Result.Milliseconds := GetInt64Value(CURLINFO_REDIRECT_TIME, 
    CURLINFO_REDIRECT_TIME_T);
end;

end.